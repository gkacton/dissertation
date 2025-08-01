# dissertation

This repository contains all of the data and code related to my 2025 dissertation, _Unprecendented Times, Unexpected Opportunities: Evaluating Metadata Quality and Community Engagement in Digital Archives of the COVID-19 Pandemic_, submitted in partial fulfillment of the requirements of the degree of Masters of Arts in Digital Humanities at Lancaster University. 

## Directories

### data
Divided by file type, with `.csv` and  `.json` files in separate folders. 

Some data files were too large to upload to GitHub -- specifically, the metadata data for the Journal of the Plague Year (JOTPY), which contains over 17,000 items, was unable to be uploaded. The code used to access the data via the JOTPY Omeka API is found [here](https://github.com/gkacton/dissertation/blob/main/scripts/scrapes/jotpy_api.R). 

Because JOTPY is such a large archive, other related data, including a reconciliation against the Library of Congress Subject Headings, are also too large to be shared here. If you would like these files, please contact me directly. 

Metadata for the UK Web Archive Coronavirus Collection was provided in `.csv` format, via email, by Carlos Lelkes-Rarugal, Assistant Web Archivist at the British Library. 

### scripts
  #### functions 
  Contains R files for functions I created for use in my analyses.
  #### analysis
  Contains R files for the complete evaluations of each sample archive. These analyses are divided into folders by archive, with the exception of scripts which compare multiple archives or are required to be run during evaluations of more than one archive.
  #### scrapes
  Contains R files used to web scrape the COVID Disability Archive, Archive-It, and JOTPY. Also contains an experiment in scraping an archived version of the UK Web Archive, hosted on the Internet Archive, which was ultimately rendered obsolete when the UKWA Coronavirus Collection metadata was shared with me directly. 

  The [PrettyTables](https://github.com/gkacton/dissertation/blob/main/scripts/pretty_tables.Rmd) notebook does **not** run independently, as it was only used to render some individual tables after each archive's analysis was complete. 

### C19WA
HTML files for each page of the Archive-It COVID-19 Web Archive. These were gathered from the site using Google Chrome's "Inspect" view. 

### renv
The R virtual environment within which all scripts were run. 

## Other Files

- The [Readable Analysis](https://github.com/gkacton/dissertation/blob/main/Analysis_Readable.Rmd) notebook contains my early experiments in applying the Bruce and Hillman Metadata Quality Framework programmatically. It does not represent the final form of my analyses -- these are contained in the `scripts/analysis` directory. 
