# dissertation

This repository contains all of the files related to my 2025 Master's dissertation, _Observations or Memorials of the Most Remarkable Occurrences: Evaluating Digital Archives of the COVID-19 Pandemic_.

## Directories

### `csv`

Contains datasets in `.csv` format. 

*Contents:*

* `SAA_COVID19_WebArchives.csv`: Collection-level metadata of COVID-19 digital archives submitted to the Society of American Archivists web archiving section. These descriptions are written by archivists, for archivists. Data downloaded from SAA Web Archiving Section blog.
* `UKWA_Covid19Collection.csv`: Metadata for all domains archived by the UK Web Archive as part of their COVID-19 collection. Somewhere between collection- and item-level metadata, because seed domains can represent a _collection_ of pages. Data provided by Carlos Lelkes-Rarugal, Assistant Web Archivist for the UKWA.

### `json`

Contains datasets in `.JSON` format, particularly those which require a column to contain a list (of subjects, tags, etc.).

*Contents:*

* `archive_it_JSON.json`: Collection-level metadata of publicly-available digital archives created using Archive-It and tagged by Archive-It as being related to COVID-19. Collected via web scraping.
* `dis_archive_JSON.json`: Item-level metadata of entries in the COVID-19 Disability Archive. Collected via web scraping. 

### `scripts`

Contains all R scripts for web scraping and data processing.

*Contents:*

* `archive_it_scrape.R`: Script used to scrape collection-level metadata from Archive-It collections.
* `disability_archive_scrape.R`: Script used to scrape item-level metadata from the [COVID Disability Archive](https://coviddisabilityarchive.com/archive/). 
* `ukwa_analysis.R`: Script used for analysis of UKWA data, including visualizations.  
