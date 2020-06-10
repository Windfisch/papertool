use once_cell::unsync::OnceCell;
use std::marker::PhantomData;
use reqwest;
use soup;
use std::fmt::{Debug,Display};
use std::io::*;


use rusqlite;

use semanticscholar;
use colored::*;

/// PublicationCache provides an interface to the local database that caches
/// all external API lookups.
pub struct PublicationCache {
	database: rusqlite::Connection
}



trait Prettyable {
	fn pretty(&self) -> String;
}

impl<T: Display> Prettyable for Option<T> {
	fn pretty(&self) -> String {
		if let Some(val) = self {
			format!("{}", val)
		}
		else {
			"<none>".into()
		}
	}
}

impl<T: Prettyable> Prettyable for OnceCell<T> {
	fn pretty(&self) -> String {
		if let Some(val) = self.get() {
			val.pretty()
		}
		else {
			"<?>".into()
		}
	}
}

#[derive(Debug)]
pub struct InconsistencyError {
	expected: String,
	found: String
}

trait SafeSet<T> {
	/// Tries to store `item` in the OnceCell. Reports Ok(()) if either the OnceCell
	/// was empty or contained exactly `item`, and Err(item_in_oncecell) if not.
	fn safe_set(&self, item: T) -> std::result::Result<(), InconsistencyError>;
	fn conflicts(&self, item: &T) -> bool;
}

impl<T: std::cmp::PartialEq + std::fmt::Debug > SafeSet<T> for OnceCell<T> {
	fn conflicts(&self, item: &T) -> bool {
		if let Some(contained) = self.get() {
			if *item != *contained {
				return true;
			}
		}
		return false;
	}

	fn safe_set(&self, item: T) -> std::result::Result<(), InconsistencyError> {
		if self.conflicts(&item) {
			return Err(InconsistencyError{
				expected: format!("{:?}", item),
				found: format!("{:?}", self.get().unwrap())
			});
		}
		else {
			self.set(item).ok(); // we don't care if this fails
			return Ok(())
		}
	}
}

#[derive(Debug)]
pub enum InconsistencyType {
	Conflict,
	NonUnique
}

#[derive(Debug)]
pub enum MyError {
	Sqlite(rusqlite::Error),
	Inconsistency(InconsistencyType, PublicationData),
	Retrieve(RetrieveError)
}

impl From<rusqlite::Error> for MyError {
	fn from(e: rusqlite::Error) -> MyError {
		MyError::Sqlite(e)
	}
}

impl From<RetrieveError> for MyError {
	fn from(e: RetrieveError) -> MyError {
		MyError::Retrieve(e)
	}
}

pub type Result<T> = std::result::Result<T, MyError>;

impl PublicationCache {
	/// Creates a new PublicationCache object. This may initialize a new database and create the schema, if not already existing.
	pub fn create() -> Result<PublicationCache> {
		let dbfile = "/tmp/bla.sqlite";
		let database =
			// try to open the DB
			rusqlite::Connection::open_with_flags(dbfile, rusqlite::OpenFlags::SQLITE_OPEN_READ_WRITE)
			.or_else(|_: rusqlite::Error| -> rusqlite::Result<rusqlite::Connection> {
				// if we failed to open it, let's create the DB
				let db = rusqlite::Connection::open_with_flags(dbfile, rusqlite::OpenFlags::SQLITE_OPEN_CREATE | rusqlite::OpenFlags::SQLITE_OPEN_READ_WRITE)?;
				// and create all tables we need.
				db.execute("CREATE TABLE cache(\
					id INTEGER PRIMARY KEY AUTOINCREMENT,\
					doi TEXT UNIQUE,\
					doi_cached INTEGER,\
					arxiv TEXT UNIQUE,\
					arxiv_cached INTEGER,\
					semanticscholar TEXT UNIQUE,\
					semanticscholar_cached INTEGER,\
					metadata_title TEXT,\
					metadata_year INTEGER,\
					metadata_cached INTEGER,\
					stub_title TEXT,\
					pdf TEXT,\
					pdf_cached INTEGER,\
					cited_by_cached INTEGER,\
					references_cached INTEGER);",
					rusqlite::params![])?;
				db.execute("CREATE TABLE cited_by(\
					cited INTEGER,\
					citing INTEGER,\
					idx INTEGER,\
					PRIMARY KEY (cited, citing),\
					FOREIGN KEY (cited) REFERENCES cache,\
					FOREIGN KEY (citing) REFERENCES cache)",
					rusqlite::NO_PARAMS)?;
				db.execute("CREATE TABLE refs(\
					citing INTEGER,\
					cited INTEGER,\
					idx INTEGER, \
					PRIMARY KEY (cited, citing),\
					FOREIGN KEY (cited) REFERENCES cache,\
					FOREIGN KEY (citing) REFERENCES cache)",
					rusqlite::NO_PARAMS)?;
				db.execute("CREATE TABLE stub_authors(\
					publication INTEGER,\
					author TEXT,\
					idx INTEGER,\
					PRIMARY KEY (publication, author),\
					FOREIGN KEY (publication) REFERENCES cache)",
					rusqlite::NO_PARAMS)?;
				Ok(db)
			})?;
		Ok(PublicationCache {
			database
		})
	}

	// Note: this sucks. It if you haven't called get() on this publication before, you might
	// overwrite your cache entry :/
	/// Writes a publication to the cache database, including its citing/referenced publications. Does not deeply recurse into these, but only shallowly stores their metadata.
	pub fn write(&self, publ: &Publication) -> Result<()> {
		// write publ's metadata (excluding cited_by/references)
		self.write_nonrecursive(publ)?;

		let dbid = publ.database_id.get().unwrap();

		// for all citing/referenced publications, 1. ensure they're known to the cache and 2. store the array into the appropriate table
		if let Some(cited_by_list) = publ.cited_by.get() {
			for cited_by in cited_by_list { // 1
				self.write_nonrecursive(cited_by)?;
			}

			println!("writing {}'s cited_by...", dbid);
			self.database.execute("UPDATE cache SET cited_by_cached = 1 WHERE id = ?", rusqlite::params![dbid])?; // 2
			self.database.execute("DELETE FROM cited_by WHERE cited = ?", rusqlite::params![dbid])?;
			for (idx,cited_by) in cited_by_list.iter().enumerate() {
				println!("\tid={}, doi={:?}, semschol={:?} title={:?}", cited_by.database_id.get().unwrap(), cited_by.doi.get(), cited_by.semanticscholar.get(), cited_by.title());
				self.database.execute("INSERT INTO cited_by (cited, citing, idx) VALUES(?, ?, ?)", rusqlite::params![dbid, cited_by.database_id.get().unwrap(), idx as i32])?;
			}
		}
		else {
			self.database.execute("DELETE FROM cited_by WHERE cited = ?", rusqlite::params![dbid])?;
			self.database.execute("UPDATE cache SET cited_by_cached = 0 WHERE id = ?", rusqlite::params![dbid])?;
		}

		if let Some(references_list) = publ.references.get() {
			for reference in references_list { // 1
				self.write_nonrecursive(reference)?;
			}

			println!("writing {}'s references...", dbid);
			self.database.execute("UPDATE cache SET references_cached = 1 WHERE id = ?", rusqlite::params![dbid])?; // 2
			self.database.execute("DELETE FROM refs WHERE citing = ?", rusqlite::params![dbid])?;
			for (idx,reference) in references_list.iter().enumerate() {
				println!("\tid={}, doi={:?}, title={:?}", reference.database_id.get().unwrap(), reference.doi.get(), reference.title());
				self.database.execute("INSERT INTO refs (citing, cited, idx) VALUES(?, ?, ?)", rusqlite::params![dbid, reference.database_id.get().unwrap(), idx as i32])?;
			}
		}
		else {
			self.database.execute("DELETE FROM refs WHERE citing = ?", rusqlite::params![dbid])?;
			self.database.execute("UPDATE cache SET references_cached = 0 WHERE id = ?", rusqlite::params![dbid])?;
		}
		Ok(())
	}

	/// saves the shallow metadata of a publication in the cache. This includes everything except cited_by/references
	pub fn write_nonrecursive(&self, publ: &Publication) -> Result<()> {
		println!("Writing {:?} to the database", publ);
		
		// If the publication does not have a database_id yet, it's not already in the database. (This is ensured in PublicationBuilder::fin() and other places) (TODO FIXME it's not!)
		if publ.database_id.get().is_none() {
			// Simplicity: Create a dummy entry that is filled with proper data by the update below. Remember the rowid we've just assigned.
			self.database.execute("INSERT INTO cache DEFAULT VALUES", rusqlite::params![])?;
			let rowid = self.database.last_insert_rowid();
			println!("insert -> {}", rowid);
			publ.database_id.set(rowid).unwrap(); // cannot fail
		}

		// We just ensured that database_id must have a value. It's impossible for it to be None.
		let database_id = publ.database_id.get().unwrap();

		// Define a macro that accepts a list of identifiers (like doi, pdf, semanticscholar) and a list of metadata identifers (i.e. members of the Metadata struct) and
		// programmatically generate the code for each field.
		macro_rules! cache_update {
			([ $( $var:ident ),+ ], [ $( $metafield:ident ),+ ] ) => {
				{
					// these params are created to fit the '?' placeholders in the query below
					let params = rusqlite::params![
						// first, fields like doi, arxiv, pdf, ...
						$(
							publ.$var.get().unwrap_or(&None), // the value; condense None and Some(None) to None, but Some(Some(x)) to Some(x)
							publ.$var.get().is_some()         // retain the information whether it was None ("didn't check") or Some(None) ("I checked, there was no result. No need to check again")
						),+,
						// now, all fields of Metadata
						$(
							publ.metadata.get().unwrap_or(&None).as_ref().map(|m| &m.$metafield)
						),+,
						publ.metadata.get().is_some(),
						// Stubmetadata's title (or None)
						publ.stubmetadata.get().map(|m| &m.title),
						database_id
					];

					let query =
						"UPDATE cache SET ".to_string() +
						$(
							stringify!($var) + " = ?, " +
							stringify!($var) + "_cached = ?, " +
						)+
						$(
							"metadata_" + stringify!($metafield) + " = ?, " +
						)+
						"metadata_cached = ?, " +
						"stub_title = ?" +
						" WHERE id = ?";
					println!("cache.write -> {}", query);
					self.database.execute(&query, params)?;

					// don't forget to write the authors array. and to clean it first
					self.database.execute("DELETE FROM stub_authors WHERE publication = ?", rusqlite::params![database_id])?;
					if let Some(stubmetadata) = publ.stubmetadata.get() {
						for (idx,author) in stubmetadata.authors.iter().enumerate() {
							self.database.execute("INSERT INTO stub_authors (publication, author, idx) VALUES (?, ?, ?)", rusqlite::params![database_id, author, idx as i64])?;
						}
					}
				}
			}
		}
		cache_update!([doi, arxiv, semanticscholar, pdf], [title, year] );
		Ok(())
	}
	
	pub fn get_all(&self, publ: &PublicationData) -> Result<Vec<PublicationData>> {
		println!("Trying to get matching entries for {:?} from the database", publ);

		// always checks database id (which is special)
		macro_rules! find_by {
			([ $( $idfield:ident ),+ ], [ $( $field:ident ),+ ] ) =>
			{{
				// build a query string that selects all $fields
				let mut query = "SELECT id".to_string() +
				$(
					", " + stringify!($field) +
					", " + stringify!($field) + "_cached" +
				)+
				" FROM cache WHERE 0 = 1"; // this is extended by several ORs
				let mut params = Vec::<Box<dyn rusqlite::ToSql>>::new();

				if let Some(database_id) = publ.database_id.get() {
					// if there's a database id given, only use that for search
					query = query + " OR id = ?";
					params.push(Box::new(database_id));
				}
				else {
					// if not, add all applicable id conditions
					$(
						if let Some(Some(val)) = publ.$idfield.get() {
							query = query + " OR " + stringify!($idfield) + " = ?";
							params.push(Box::new(val));
						}
					)+
				}
				
				println!("cache.get -> {}", query);
				
				// ok, finally perform the query
				let mut stmt = self.database.prepare(&query)?;
				let mut rows = stmt.query(params)?;
				let mut query_results = Vec::<PublicationData>::new();

				// Parse all results into a Vec of PublicationData
				while let Some(row) = rows.next()? {
					let data: PublicationData = PublicationData::new();

					let dbid: i64 = row.get(0)?;
					data.database_id.safe_set(dbid).unwrap();
					let mut _i = 1;
					$(
						let is_cached: bool = row.get(_i+1)?;
						if is_cached {
							let val = row.get(_i)?;
							data.$field.safe_set(val).unwrap();
						}
						_i += 2;
					)+

					// we still need to read the "nontrivial" fields such as (stub_)metadata.
					let stubtitle_opt: Option<String> = self.database.query_row(
						"SELECT stub_title FROM cache WHERE id = ?",
						rusqlite::params![dbid],
						|r| r.get(0)
					)?;
					if let Some(stubtitle) = stubtitle_opt {
						// if there is actually a stub metadata cached
						let mut stubmetadata = StubMetadata{ title: String::new(), authors: Vec::new() };
						let mut stmt = self.database.prepare(
							"SELECT author FROM stub_authors WHERE publication = ? ORDER BY idx"
						)?;
						let mut rows = stmt.query(rusqlite::params![dbid])?;
						while let Some(row) = rows.next()? {
							stubmetadata.authors.push(row.get(0)?);
						}
						stubmetadata.title = stubtitle;
						data.stubmetadata.set(stubmetadata).unwrap();
					}

					query_results.push(data);
				}

				Ok(query_results)
			}}
		}

		find_by!([doi, arxiv, semanticscholar], [doi, arxiv, semanticscholar, pdf])
	}

	/// Tries to get a dataset from the cache. Returns any entry where at least one of the identifiers like doi, arxiv, ... match.
	/// Returns true if found, false if not.
	/// Returns Err(MyError::Inconsistency(...)) if either a conflict between the cached IDs and self's IDs is found, or
	/// if more than one database rows match the query. This can happen due to wrong data sources and must be resolved by the user.
	/// Returns Err(SqliteError(...)) if a database error occurred. (This should not happen.)
	pub fn get(&self, publ: &PublicationData) -> Result<bool> {
		println!("Trying to get {:?} from the database", publ);

		let mut matches = self.get_all(publ)?;

		if matches.len() > 1 {
			return Err(MyError::Inconsistency(InconsistencyType::NonUnique, publ.clone()));
		}

		if matches.len() == 0 {
			return Ok(false);
		}

		let mut data = matches.pop().unwrap(); // guaranteed to succeed

		// always checks database id (which is special)
		macro_rules! update_from {
			([ $( $idfield:ident ),+ ], [ $( $field:ident ),+ ] ) =>
			{{
				// first check whether any fields from the database conflict with ours
				let mut conflict = false;
				$(
					if let Some(val) = data.$field.get() {
						if publ.$field.conflicts(val) {
							println!("ERROR: conflicting values for {:?}: '{:?}' vs '{:?}'", stringify!($field), val, publ.$field.get().unwrap());
							conflict = true;
						}
					}
				)+
				
				if conflict {
					return Err(MyError::Inconsistency(InconsistencyType::Conflict, publ.clone()));
				}

				// then, if there was no conflict, actually apply these fields
				publ.database_id.safe_set(*data.database_id.get().unwrap()).unwrap();
				$(
					if let Some(val) = data.$field.take() {
						publ.$field.safe_set(val).unwrap();
					}
				)+

				if let Some(stubmetadata) = data.stubmetadata.take() {
					publ.stubmetadata.set(stubmetadata); // TODO not sure how to do safe set here
				}
			}}
		}

		update_from!([doi, arxiv, semanticscholar], [doi, arxiv, semanticscholar, pdf]);
		println!("Got {:?}", publ);
		return Ok(true);
	}

	pub fn solve_conflict(&self, reproducer: &PublicationData) {
		let matches = self.get_all(reproducer).unwrap();

		println!("##########################");
		println!("# NEED USER INTERVENTION #");
		println!("##########################");
		println!("Got #1 from API:\n{}", reproducer);
		let mut all_matching_keys = Vec::<String>::new();
		for (i, entry) in matches.iter().enumerate() {
			macro_rules! equalkeys {
				([ $( $field:ident ),+ ]) => {{
					let mut matching_keys = Vec::<String>::new();
					$(
						if let Some(val1) = reproducer.$field.get() {
							if let Some(val2) = entry.$field.get() {
								if val1 == val2 {
									matching_keys.push( stringify!($field).to_string() );
								}
							}
						}
					)+
					matching_keys
				}}
			}
			let matching_keys = equalkeys!([doi, arxiv, semanticscholar]);
			println!("Got #{} from database due to same {}:\n{}", i+2, matching_keys.join(", ").magenta(), entry);
			all_matching_keys.extend(matching_keys);
		}

		all_matching_keys.sort();
		all_matching_keys.dedup();

		println!("");
		println!("Obviously, one of these {} fields is wrong. Please enter a correction using the following syntax:", all_matching_keys.join(", "));
		println!("\t{}{}{}", 
			"> ".bright_black(),
			  "doi1=<insert new doi here>".magenta(),
			                            "           to change #1's doi to something else or");
		println!("\t{}{}{}", 
			"> ".bright_black(),
			  "arxiv2=-,doi2=-".magenta(),
			                 "                      to change #2's arxiv id and doi to <none>");
		println!("\t{}{}{}", 
			"> ".bright_black(),
			  "1=<...>".magenta(),
			         "                              to change #1's id, if there is only one conflicting id");
		print!("> ");
		stdout().flush().ok();

		'inputloop: loop {

			fn prompt() {
				println!("Please enter a correction.");
				print!("> ");
				stdout().flush().ok();
			}

			let mut input = String::new();
			stdin().read_line(&mut input).expect("error: unable to read user input");
			input = input.trim().into();

			let mut ok = false;
			
			let commands = input.split(',');

			for command in commands {
				let mut parts: Vec<_> = command.split('=').collect();
				
				if parts.len() != 2 {
					println!("error: invalid syntax in {}", command);
					prompt();
					continue 'inputloop;
				}

				let lvalue = parts.remove(0).trim();
				let rvalue = parts.remove(0).trim();

				fn split_num(s: &str) -> (&str, &str) {
					for (i, c) in s.chars().enumerate() {
						if c.is_numeric() {
							return ( &s[0..i], &s[i..] );
						}
					}
					return (s, "");
				}

				let (mut identifier, number_str) = split_num(lvalue);
				if number_str.len() == 0 {
					println!("error: missing number in {}", lvalue);
					prompt();
					continue 'inputloop;
				}

				let number = number_str.parse::<usize>().unwrap(); // cannot fail

				if number > matches.len()+1 {
					println!("error: number must be between 1 and {} in {}", matches.len()+1, lvalue);
					prompt();
					continue 'inputloop;
				}

				if identifier.len() == 0 {
					if all_matching_keys.len() == 1 {
						identifier = all_matching_keys.get(0).unwrap();
					}
					else {
						println!("error: cannot leave out the id part in this situation");
						prompt();
						continue 'inputloop;
					}
				}

				let new_value: Option<String> =
					match rvalue {
						"" => None,
						"-" => None,
						s => Some(s.to_string())
					};

				println!("parsed: set #{}'s {} to {:?}", number, identifier, new_value);
				ok = true;
			}

			if ok {
				//break 'inputloop;
			}
			else {
				prompt();
				continue 'inputloop;
			}
		}
	}
}

/// The most commonly used, incomplete set of metadata about a publication that
/// should *usually* uniquely identify a publication (i.e.: title, authors),
/// but there are cases where this is not sufficient. Not suitable for exporting
/// to a BibTeX citation.
#[derive(Debug,Clone)]
pub struct StubMetadata {
	pub title: String,
	pub authors: Vec<String>,
}

/// The complete metadata record about a publication, suitable for creating a
/// BibTeX citation.
#[derive(Debug,Clone)]
pub struct Metadata {
	pub title: String,
	pub authors: Vec<(String,String)>,
	pub year: i32,
	// ... TODO
}

/// Proxy object identifying a publication. 
#[derive(Clone)]
pub struct Publication<'a> {
	cache: &'a PublicationCache,
	data: PublicationData,
	cited_by: OnceCell<Vec<Publication<'a>>>,
	references: OnceCell<Vec<Publication<'a>>>,
}

use std::ops::Deref;
impl<'a> Deref for Publication<'a> {
	type Target = PublicationData;

	fn deref(&self) -> &PublicationData {
		&self.data
	}
}


#[derive(Clone,Debug)]
pub struct PublicationData {
	database_id: OnceCell<i64>,
	stubmetadata: OnceCell<StubMetadata>,
	metadata: OnceCell<Option<Metadata>>,
	doi: OnceCell<Option<String>>,
	arxiv: OnceCell<Option<String>>,
	pdf: OnceCell<Option<String>>,
	semanticscholar: OnceCell<Option<String>>
}

fn format_authors(authors: &Vec<String>) -> String {
	match authors.len() {
		0 => "?".into(),
		1 => authors[0].clone(),
		2 => authors.join(" and "),
		_ => format!("{} et al.", authors[0])
	}
}

impl Display for PublicationData {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let (title, authors) : (String, String) = match self.stubmetadata.get() {
			None => ("?".into(), "?".into()),
			Some(stubmetadata) => (stubmetadata.title.clone(), format_authors(&stubmetadata.authors))
		};

		macro_rules! mk_keystrs {
			( [ $( $field:ident . $color:ident ),+ ] ) => {{
				let mut keystr = Vec::<String>::new();
				$(
					if let Some(Some(val)) = self.$field.get() {
						keystr.push( format!("{}:{}", stringify!($field), val.$color()) );
					}
				)+
				keystr
			}}
		}
		let keystrs = mk_keystrs!([doi.red, arxiv.cyan, semanticscholar.purple]);

		write!(f, "'{}' by {} ({})",
			title.green(),
			authors.blue(),
			keystrs.join(", ")
		)
	}
}

impl PublicationData {
	pub fn new() -> PublicationData {
		PublicationData {
			database_id: OnceCell::new(),
			stubmetadata: OnceCell::new(),
			metadata: OnceCell::new(),
			doi: OnceCell::new(),
			arxiv: OnceCell::new(),
			pdf: OnceCell::new(),
			semanticscholar: OnceCell::new()
		}
	}
}

impl<'a> Publication<'a> {
	pub fn flush_cache(&self) {
		self.cache.write(self).unwrap();
	}

	fn try_get_cached(&self) -> Result<bool> {
		self.cache.get(&self.data)
	}
}

impl<'a> std::fmt::Debug for Publication<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(),std::fmt::Error> {
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
	fn database_id(self, dbid: i64) -> PublicationBuilder<'a, Finalizable> { // FIXME ugh
		self.publication.database_id.set(dbid).unwrap();
		PublicationBuilder{
			publication: self.publication,
			_marker: PhantomData{}
		}
	}
	build_with!(doi: String);
	build_with!(arxiv: String);
	build_with!(semanticscholar: String);
}

impl<'a> PublicationBuilder<'a, Finalizable> {
	/// Consumes the builder and returns the actual publication. Only available if one
	/// publication identifier has been set already.
	pub fn fin(self) -> Publication<'a> { self.publication.try_get_cached().unwrap(); self.publication.flush_cache(); self.publication }
}

macro_rules! smart_getter {
	( $what:ident: Option<$type:ty>, [ $( $retriever:ident ),+ ] ) => {
		#[allow(unused)]
		pub fn $what(&self) -> Result<&Option<$type>> {
			if self.$what.get().is_none() {
				$(
					if self.$what.get().is_none() { self.$retriever()?; }
				)+
				if self.$what.get().is_none() { println!("not found :("); self.$what.set(None); }
				self.flush_cache();
			}
			return Ok(self.$what.get().unwrap());
		}
	};
	( $what:ident: $type:ty, [ $( $retriever:ident ),+ ] ) => {
		#[allow(unused)]
		pub fn $what(&self) -> Result<&$type> {
			if self.$what.get().is_none() {
				$(
					if self.$what.get().is_none() { self.$retriever()?; }
				)+
				self.flush_cache();
			}
			return Ok(self.$what.get().unwrap());
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

	pub fn from_dbid(cache: &'a PublicationCache, id: i64) -> Result<Option<Publication<'a>>> {
		let tmp = Publication::new(cache);
		tmp.database_id.set(id).unwrap(); // cannot fail
		let found = cache.get(&tmp.data)?;
		if found {
			Ok(Some(tmp))
		}
		else {
			Ok(None)
		}
	}

	fn new(cache: &'a PublicationCache) -> Publication<'a> {
		Publication {
			cache,
			data: PublicationData::new(),
			cited_by: OnceCell::new(),
			references: OnceCell::new()
		}
	}

	/// scrapes the PDF link from the semanticscholar website, since this is not available through the api yet
	pub fn scrape_pdf_from_semanticscholar(&self) -> std::result::Result<String, RetrieveError> {
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

	pub fn retrieve_pdf_from_semanticscholar(&self) -> Result<()> {
		if let Ok(url) = self.scrape_pdf_from_semanticscholar() {
			self.pdf.set(Some(url)).unwrap();
		}
		Ok(())
	}

	/// queries the semanticscholar API. this gives some metadata, cited-by and references, but not pdf.
	pub fn retrieve_from_semanticscholar(&self) -> Result<()> {
		println!("**** RETRIEVING FROM SEMANTICSCHOLAR ****");
		let client = semanticscholar::Client::new();
		let mut result : Option<semanticscholar::Work> = None;
		if result.is_none() { if let Some(Some(semanticscholar)) = self.semanticscholar.get() { result = client.work(semanticscholar).ok(); } }
		if result.is_none() { if let Some(Some(doi)) = self.doi.get() { result = client.work(doi).ok(); } }
		if result.is_none() { if let Some(Some(arxiv)) = self.arxiv.get() { result = client.work(&("arXiv:".to_string() + arxiv)).ok(); } }
		Publication::fill_from_semanticscholar(self, result.expect("Could not find in semanticscholar"))?;
		self.cited_by.set( Vec::new() ).ok(); // if no cited_by was in the result, ensure this is the empty vector
		self.references.set( Vec::new() ).ok();
		Ok(())
	}


	fn publ_array_from_rows(&self, rows: &mut rusqlite::Rows) -> Result<Vec<Publication<'a>>> {
		let mut publs = Vec::<Publication>::new();
		while let Some(row) = rows.next()? {
			let dbid: i64 = row.get(0)?;
			let publ = Publication::build(self.cache).database_id(dbid).fin();
			publ.try_get_cached()?;
			publs.push(publ);
		}
		return Ok(publs);
	}

	// this function expects database_id to be set properly.
	pub fn get_cited_by_from_cache(&self) -> Result<()> {
		// TODO this should be in database, not here.
		if let Some(database_id) = self.database_id.get() {
			let is_cached: bool = self.cache.database.query_row("SELECT cited_by_cached FROM cache WHERE id = ?", rusqlite::params![database_id], |r| r.get(0))?;
			if is_cached {
				let mut stmt = self.cache.database.prepare("SELECT citing FROM cited_by WHERE cited = ? ORDER BY idx")?;
				let mut rows = stmt.query(rusqlite::params![database_id])?;
				self.cited_by.set( self.publ_array_from_rows(&mut rows)?).unwrap();
			}
		}
		Ok(())
	}

	pub fn get_references_from_cache(&self) -> Result<()> {
		// TODO this should be in database, not here.
		if let Some(database_id) = self.database_id.get() {
			let is_cached: bool = self.cache.database.query_row("SELECT references_cached FROM cache WHERE id = ?", rusqlite::params![database_id], |r| r.get(0))?;
			if is_cached {
				let mut stmt = self.cache.database.prepare("SELECT cited FROM refs WHERE citing = ? ORDER BY idx")?;
				let mut rows = stmt.query(rusqlite::params![database_id])?;
				self.references.set( self.publ_array_from_rows(&mut rows)?).unwrap();
			}
		}
		Ok(())
	}

	fn new_from_semanticscholar(cache: &'a PublicationCache, result: semanticscholar::Work) -> Result<Publication<'a>> {
		let ret = Publication::new(cache);
		ret.fill_from_semanticscholar(result)?;
		ret.try_get_cached()?;
		ret.flush_cache();
		return Ok(ret);
	}

	fn fill_from_semanticscholar(&self, result: semanticscholar::Work) -> Result<()> {
		// First, set the semanticscholar id and try to find that ID in our cache.
		// This is needed, because the semanticscholar data might be wrong, esp. the DOI.
		// Only the semanticscholar ID can really be trusted, for everything else, we prefer
		// what's already in our DB if we have a match.
		if let Some(value) = result.paper_id {
			self.semanticscholar.safe_set(Some(value)).unwrap();
			self.try_get_cached()?;
		}

		// Now, set
		if let Some(value) = result.arxiv_id {
			if self.arxiv.safe_set(Some(value.clone())).is_err() {
				println!("Warning: semanticscholar {} returned arxiv {}, but our cache says {}",
					self.semanticscholar.pretty(), value, self.arxiv.pretty());
			}
		}
		if let Some(value) = result.doi {
			if self.doi.safe_set(Some(value.clone())).is_err() {
				println!("Warning: semanticscholar {} returned doi {}, but our cache says {:?}",
					self.semanticscholar.pretty(), value, self.doi.pretty());
			}
		}
		self.stubmetadata.set( StubMetadata {
			title: result.title.unwrap_or("<No title>".to_string()),
			authors: result.authors.into_iter().map(|a| { a.name.unwrap_or("<No name>".to_string()) }).collect()
		});
		if !result.citations.is_empty() {
			let r: Result<Vec<_>> = result.citations.into_iter().map(|work| { Publication::new_from_semanticscholar(self.cache, work) }).collect();
			self.cited_by.set(r?);
		}
		if !result.references.is_empty() {
			let r: Result<Vec<_>> = result.references.into_iter().map(|work| { Publication::new_from_semanticscholar(self.cache, work) }).collect();
			self.references.set(r?);
		}
		Ok(())
	}

	smart_getter!(cited_by: Vec<Publication<'a>>, [ get_cited_by_from_cache, retrieve_from_semanticscholar ]);
	smart_getter!(references: Vec<Publication<'a>>, [ get_references_from_cache, retrieve_from_semanticscholar ]);
	smart_getter!(pdf: Option<String>, [retrieve_pdf_from_semanticscholar]);
	smart_getter!(doi: Option<String>, [retrieve_from_semanticscholar]);
	smart_getter!(arxiv: Option<String>, [retrieve_from_semanticscholar]);
	smart_getter!(semanticscholar: Option<String>, [retrieve_from_semanticscholar]);
	smart_getter!(stubmetadata: StubMetadata, [retrieve_from_semanticscholar]);

	pub fn title(&self) -> Option<&String> {
		match self.stubmetadata.get() {
			None => None,
			Some(sm) => Some(&sm.title)
		}
	}

	pub fn database_id(&self) -> i64 {
		if self.database_id.get().is_none() {
			self.try_get_cached().unwrap(); // TODO this should be handled
			if self.database_id.get().is_none() {
				self.flush_cache();
			}
		}
		*self.database_id.get().unwrap() // there MUST be a database id available now.
	}
}

