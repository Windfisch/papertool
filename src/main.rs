mod database;
use database::*;

fn main() {
	let cache = PublicationCache::new();
	let mypub = Publication::build(&cache).doi("10.1109/ICoCS.2015.7483250").fin();
	//mypub.metadata();
	//mypub.retrieve_from_semanticscholar();
	println!("before:\n{:#?}", mypub);
	println!("\n\n============================================================\n\n");
	println!("references is\n{:#?}", mypub.references());
	println!("\n\n============================================================\n\n");
	println!("after:\n{:#?}", mypub);

	mypub.cited_by();
}
