# Waves of Words: Tools for studying Australia's past

A suite of functions for querying [Trove](https://trove.nla.gov.au) and extracting useful results in R.

## Usage

This is not a package, just a script with some useful functions and a schema for a corresponding postgres database.

To use these scripts, you need to have access to a [postgresql](https://www.postgresql.org/) database. If you install postgres on your machine, it is easy to set up a local database to store your research data.

Once you have created the database, simply run `create_wow_db.pgsql` to create the necessary tables.

Then open an R session, using RStudio or whichever framework you prefer. You will need a [Trove API key]() to access Trove, and you will need to know the credentials for connecting to your postgres database. Use `source('helper_functions.R')` to load the functions.

The main functions are:

* `simpleSearch()` allows you to query the database, returning the first twenty results of your query to test it out.
* `countResults()` returns the number of hits for your search query.
* `harvest()` allows you to harvest results from Trove directly into memory. This is good if you have a smaller number of results, and want to work with them directly in R.
* `harvest_into_db()` downloads articles from Trove, and inserts them into your Postgres database.

## Author

* Michael Falk

## Licence

MIT Licence (see attached licence file)