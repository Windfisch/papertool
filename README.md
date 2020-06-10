Papertool
=========

Papertool is (will be) a multi-datasource **client for viewing and navigating
scientific publications** written in Rust. It allows to **navigate** through
the **references** and citations of a publication, query metadata and even
retrieve a (link to) the PDF file in most cases.

It's **extensible** and can support multiple data sources. It can deal with
wrong metadata, ultimately by asking the user on how to fix e.g. a situation
where two publications have the same DOI (which cannot be right, DOIs are
unique).

All queried data is locally **cached** in a SQLite3 database to speed up
subsequent accesses.

This is still in an early stage of development. The "hard stuff" seems to work
quite well already, though this is certainly not usable yet for real-world
usage.
