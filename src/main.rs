mod database;
use database::*;

fn main() {
	let cache = PublicationCache::new();
	//let mypub = Publication::build(&cache).doi("10.1109/ICoCS.2015.7483250").fin();
	let mypub = Publication::build(&cache).doi("10.1001/jama.2020.2648").fin();
	mypub.retrieve_pdf_from_semanticscholar();
	println!("before:\n{:#?}", mypub);
	println!("\n\n============================================================\n\n");
	println!("references is\n{:#?}", mypub.references());
	println!("\n\n============================================================\n\n");
	println!("after:\n{:#?}", mypub);

	//mypub.scrape_pdf_from_semanticscholar();

	println!("pdf url is {:?}" , mypub.pdf());
	println!("pdf url is {:?}" , mypub.pdf());

}
