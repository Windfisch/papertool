use once_cell::unsync::OnceCell;
use std::marker::PhantomData;
use reqwest;
use soup;
use std::fmt::{Debug,Display};

use semanticscholar;

/// PublicationCache provides an interface to the local database that caches
/// all external API lookups.
pub struct PublicationCache {
	
}

impl PublicationCache {
	pub fn new() -> PublicationCache { PublicationCache{} }
}

/// The most commonly used, incomplete set of metadata about a publication that
/// should *usually* uniquely identify a publication (i.e.: title, authors),
/// but there are cases where this is not sufficient. Not suitable for exporting
/// to a BibTeX citation.
#[derive(Debug)]
pub struct StubMetadata {
	pub title: String,
	pub authors: Vec<String>,
}

/// The complete metadata record about a publication, suitable for creating a
/// BibTeX citation.
#[derive(Debug)]
pub struct Metadata {
	pub title: String,
	pub authors: Vec<(String,String)>,
	pub year: Option<i32>,
	// ... TODO
}

/// Proxy object identifying a publication. 
pub struct Publication<'a> {
	cache: &'a PublicationCache,

	stubmetadata: OnceCell<StubMetadata>,
	metadata: OnceCell<Option<Metadata>>,
	doi: OnceCell<Option<String>>,
	arxiv: OnceCell<Option<String>>,
	pdf: OnceCell<Option<String>>,
	semanticscholar: OnceCell<Option<String>>,

	cited_by: OnceCell<Vec<Publication<'a>>>,
	references: OnceCell<Vec<Publication<'a>>>,
}

impl<'a> std::fmt::Debug for Publication<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(),std::fmt::Error> {
		f.debug_struct("Publication").
			field("stubmetadata", &self.stubmetadata).
			field("metadata", &self.metadata).
			field("doi", &self.doi).
			field("arxiv", &self.arxiv).
			field("pdf", &self.pdf).
			field("semanticscholar", &self.semanticscholar).
			field("cited_by", &self.cited_by).
			field("references", &self.references).
			finish()
	}
}

pub trait FinalizableMarker {}
pub struct NotFinalizable {} impl FinalizableMarker for NotFinalizable {}
pub struct Finalizable {} impl FinalizableMarker for Finalizable {}

/// Encapsulates a [Publication] object and allows to set its IDs like doi, arxiv or
/// semanticscholar. If at least one ID has been set, [fin()] retrieves the actual
/// Publication object.
pub struct PublicationBuilder<'a, IsFinalizable : FinalizableMarker> {
	publication: Publication<'a>,
	_marker : PhantomData<IsFinalizable>
}

macro_rules! build_with {
	( $what:ident : $type:ty ) => {
		#[allow(dead_code)]
		pub fn $what(self, value: impl Into<$type>) -> PublicationBuilder<'a, Finalizable> {
			self.publication.$what.set(Some(value.into())).unwrap(); // TODO: this unwrap is not great. better do typestates on every $what
			PublicationBuilder{
				publication: self.publication,
				_marker: PhantomData{}
			}
		}
	}
}

impl<'a, IsFinalizable : FinalizableMarker> PublicationBuilder<'a, IsFinalizable> {
	build_with!(doi: String);
	build_with!(arxiv: String);
	build_with!(semanticscholar: String);
}

impl<'a> PublicationBuilder<'a, Finalizable> {
	/// Consumes the builder and returns the actual publication. Only available if one
	/// publication identifier has been set already.
	pub fn fin(self) -> Publication<'a> { self.publication }
}

macro_rules! smart_getter {
	( $what:ident: Option<$type:ty>, [ $( $retriever:ident ),+ ] ) => {
		#[allow(unused)]
		pub fn $what(&self) -> &Option<$type> {
			$(
				if self.$what.get().is_none() { self.$retriever(); }
			)+
			if self.$what.get().is_none() { println!("not found :("); self.$what.set(None); }
			return self.$what.get().unwrap();
		}
	};
	( $what:ident: $type:ty, [ $( $retriever:ident ),+ ] ) => {
		#[allow(unused)]
		pub fn $what(&self) -> &$type {
			$(
				if self.$what.get().is_none() { self.$retriever(); }
			)+
			return self.$what.get().unwrap();
		}
	}
}

#[derive(Debug)]
struct HTTPError(reqwest::StatusCode);
impl Display for HTTPError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "HTTP Error {} ({})", self.0.as_str(), self.0.canonical_reason().unwrap_or("?")) // user-facing output
	}
}
impl std::error::Error for HTTPError {}

#[derive(Debug)]
struct ParseError(String);
impl Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Parse Error: {}", self.0)
	}
}
impl ParseError {
	fn new(reason: &str) -> ParseError { ParseError(reason.to_string()) }
}
impl std::error::Error for ParseError {}

#[derive(Debug)]
pub enum RetrieveError {
	InsufficientData,
	NoResult,
	RequestError(Box<dyn std::error::Error>),
	MalformedReply(Box<dyn std::error::Error>)
}

impl<'a> Publication<'a> {
	/// Allows to create a new Publication object through a [PublicationBuilder].
	/// Example:
	/// ```
	/// let my_cache = ...;
	/// let my_publication = Publication::build(&my_cache).doi("10.1234.deadbeef").semanticscholar("1337").fin();
	/// ```
	pub fn build(cache: &'a PublicationCache) -> PublicationBuilder<'a, NotFinalizable> {
		PublicationBuilder {
			publication: Publication::new(cache),
			_marker: PhantomData{}
		}
	}

	fn new(cache: &'a PublicationCache) -> Publication<'a> {
		Publication {
			cache,
			stubmetadata: OnceCell::new(),
			metadata: OnceCell::new(),
			doi: OnceCell::new(),
			arxiv: OnceCell::new(),
			pdf: OnceCell::new(),
			semanticscholar: OnceCell::new(),
			cited_by: OnceCell::new(),
			references: OnceCell::new()
		}
	}

	/// scrapes the PDF link from the semanticscholar website, since this is not available through the api yet
	pub fn scrape_pdf_from_semanticscholar(&self) -> Result<String, RetrieveError> {
		use soup::QueryBuilderExt;
		use soup::NodeExt;

		let query =
			if let Some(Some(semanticscholar)) = self.semanticscholar.get() { semanticscholar.to_string() }
			else if let Some(Some(doi)) = self.doi.get() { doi.to_string() }
			else if let Some(Some(arxiv)) = self.arxiv.get() { "arXiv:".to_string() + arxiv }
			else { return Err(RetrieveError::InsufficientData); };

		let url = format!("https://api.semanticscholar.org/{}", query);
		println!("semanticscholar pdf: querying {}", url);
		match reqwest::blocking::get(&url) {
			Ok(response) => {
				
				let status = response.status();
				if !status.is_success() {
					if status == 404 {
						println!("semanticscholar returned 404");
						return Err(RetrieveError::NoResult);
					}
					else {
						return Err(RetrieveError::RequestError(Box::new(HTTPError(status))));
					}
				}

				println!("\tsuccess!");
				match response.text() {
					Ok(text) => {
						println!("\tparsing html!");
						let doc = soup::Soup::new(&text);
						for link in doc.tag("a").attr("data-heap-unpaywall-link","true").find_all() {
							//println!("\t\tlink: {}", link.display());
							match link.get("link") {
								Some(result) => { println!("\t\t=> {}", result); return Ok(result) },
								None => ()
							}
						}
						Err(RetrieveError::NoResult)
					}
					Err(e) => Err(RetrieveError::RequestError(Box::new(e)))
				}
			}
			Err(e) => Err(RetrieveError::RequestError(Box::new(e)))
		}
	}

	pub fn retrieve_pdf_from_semanticscholar(&self) {
		if let Ok(url) = self.scrape_pdf_from_semanticscholar() {
			self.pdf.set(Some(url));
		}
	}

	/// queries the semanticscholar API. this gives some metadata, cited-by and references, but not pdf.
	pub fn retrieve_from_semanticscholar(&self) {
		println!("**** RETRIEVING FROM SEMANTICSCHOLAR ****");
		let client = semanticscholar::Client::new();
		let mut result : Option<semanticscholar::Work> = None;
		if result.is_none() { if let Some(Some(semanticscholar)) = self.semanticscholar.get() { result = client.work(semanticscholar).ok(); } }
		if result.is_none() { if let Some(Some(doi)) = self.doi.get() { result = client.work(doi).ok(); } }
		if result.is_none() { if let Some(Some(arxiv)) = self.arxiv.get() { result = client.work(&("arXiv:".to_string() + arxiv)).ok(); } }
		Publication::fill_from_semanticscholar(self, result.expect("Could not find in semanticscholar"));
		self.cited_by.set( Vec::new() ).ok(); // if no cited_by was in the result, ensure this is the empty vector
		self.references.set( Vec::new() ).ok();
	}

	fn new_from_semanticscholar(cache: &'a PublicationCache, result: semanticscholar::Work) -> Publication<'a> {
		let ret = Publication::new(cache);
		ret.fill_from_semanticscholar(result);
		return ret;
	}

	fn fill_from_semanticscholar(&self, result: semanticscholar::Work) {
		if let Some(value) = result.arxiv_id { self.arxiv.set(Some(value)); }
		if let Some(value) = result.doi { self.doi.set(Some(value)); }
		if let Some(value) = result.paper_id { self.semanticscholar.set(Some(value)); }
		self.stubmetadata.set( StubMetadata {
			title: result.title.unwrap_or("<No title>".to_string()),
			authors: result.authors.into_iter().map(|a| { a.name.unwrap_or("<No name>".to_string()) }).collect()
		});
		if !result.citations.is_empty() {
			self.cited_by.set(result.citations.into_iter().map(|work| { Publication::new_from_semanticscholar(self.cache, work) }).collect());
		}
		if !result.references.is_empty() {
			self.references.set(result.references.into_iter().map(|work| { Publication::new_from_semanticscholar(self.cache, work) }).collect());
		}
	}

	pub fn metadata(&self) -> &Option<Metadata> {
		let foo = 42;
		self.metadata.get_or_init(|| {Some(Metadata{title:String::new(),authors:Vec::new(), year:Some(foo)})} )
	}

	
	smart_getter!(cited_by: Vec<Publication<'a>>, [ retrieve_from_semanticscholar ]);
	smart_getter!(references: Vec<Publication<'a>>, [ retrieve_from_semanticscholar ]);
	smart_getter!(pdf: Option<String>, [retrieve_pdf_from_semanticscholar]);
}

