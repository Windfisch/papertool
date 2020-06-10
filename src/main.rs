mod database;
use database::*;
use std::collections::HashMap;

fn do_stuff(cache: &PublicationCache) -> database::Result<()> {
	let relevant_dois = ["10.1109/3DIMPVT.2011.45", "10.1109/ICoCS.2015.7483250", "10.1109/WACV.2016.7477638", "10.1109/ICCV.2015.248", "10.1109/CVPR.2014.93"];

	let relevant_pubs: Vec<_> = relevant_dois.iter().map(|s| Publication::build(&cache).doi(*s).fin()).collect();

	let mut fnord = HashMap::<i64, u32>::new();

	println!("HASS");

	for publ in relevant_pubs {
		for p in publ.references()? {
			let counter = fnord.entry(p.database_id()).or_insert(0);
			*counter += 1;
		}
	}
	
	println!("HASS2");

	let mut fnord2 = Vec::<(String, u32)>::new();
	for (key, counter) in fnord.drain() {
		let publ = Publication::from_dbid(&cache, key).unwrap().unwrap();
		fnord2.push( (publ.stubmetadata()?.title.clone(), counter) );
	}
	fnord2.sort_by_key(|x| x.1);

	println!("{:#?}", fnord2);
	println!("HASS3");

	return Ok(());
}

fn main() -> database::Result<()> {
	let cache = PublicationCache::create().unwrap();

	match do_stuff(&cache) {
		Ok(_) => println!("everything is fine :)"),
		Err(MyError::Inconsistency(kind,reproducer)) => {
			println!("Inconsistency of kind {:?} detected! Reproducer:\n{:#?}", kind, reproducer);

			cache.solve_conflict(&reproducer);
		},
		Err(e) => return Err(e)
	}

	return Ok(());

	let mypub = Publication::build(&cache).doi("10.1109/ICoCS.2015.7483250").fin();
	//let mypub = Publication::build(&cache).doi("10.1001/jama.2020.2648").fin();
	//mypub.metadata();
	//mypub.retrieve_from_semanticscholar();
	println!("before:\n{:#?}", mypub);
	println!("\n\n============================================================\n\n");
	println!("references is\n{:#?}", mypub.references());
	println!("cited_by is\n{:#?}", mypub.cited_by());
	println!("\n\n============================================================\n\n");
	println!("after:\n{:#?}", mypub);

	//mypub.scrape_pdf_from_semanticscholar();

	println!("pdf url is {:?}" , mypub.pdf());
	println!("pdf url is {:?}" , mypub.pdf());

}
