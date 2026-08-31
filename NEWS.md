# floraR 1.0.0

## Initial Release

The first official release of the `floraR` R package, designed to provide comprehensive access, analysis, and curation tools for data from the Flora e Funga do Brasil (FFB) platform maintained by the Rio de Janeiro Botanical Garden.

### Features

- `flora_version()`: Retrieve metadata and check available dataset versions from the Flora e Funga do Brasil IPT data portal.
- `flora_download()`: Download taxonomic and distributional data in Darwin Core Archive (DwC-A) format for specific or all available versions.
- `flora_parse()`: Parse and organize locally downloaded FFB datasets, extracting structured information from DwC-A files.
- `flora_records()`: Browse and filter the FFB checklist directly by taxonomic, geographic, and trait-based criteria, without requiring an input name list. Downloads and parses the dataset automatically, reusing the local cache on repeated calls.
- `flora_search()`: Resolve your own species name list against the FFB checklist, with exact matching first and fuzzy (Levenshtein-distance) matching as a fallback for typos, including synonym resolution.
- `flora_match()`: Compare two independent species name lists, aligning names that resolve to the same accepted taxon.
- `flora_get_children_taxa()`: Retrieve all child taxa (species, subspecies, varieties, genera, etc.) below a given taxonomic name and rank.
- `flora_get_descriptions()`: Scrape controlled-field and free-text descriptions from FFB taxon pages, to assist taxonomic experts curating new records.
- `flora_build_matrix()`: Turn extracted taxon descriptions into a character matrix (taxa x character states) ready for downstream comparison or trait analysis.
- Automated data cleaning and standardization of taxon names, distribution data, and species profiles.
- Support for offline data analysis once datasets are downloaded.
- Integration with global biodiversity repositories (IPNI, REFLORA, GBIF) for data curation workflows.
- Seamless integration with tidyverse packages for downstream analyses.

### Key Capabilities

- **Version Control**: Track, download, and parse specific dataset versions
- **Checklist Filtering**: Browse and filter the FFB checklist by taxonomic, geographic, and trait-based criteria without an input name list
- **Name Resolution**: Exact and fuzzy matching of your own species lists against the FFB checklist, including synonym resolution
- **Taxonomic Hierarchy**: Retrieve child taxa at any rank, from class down to species
- **Distribution Data**: Extract endemism status and phytogeographic domain information
- **Species Profiles**: Parse life form, habitat, and vegetation type data
- **Data Curation**: Tools for taxonomic experts to integrate new records and names
- **Offline Analysis**: Work with downloaded data without internet connection

### Infrastructure

- MIT license
- Comprehensive test coverage with testthat
- Continuous integration via GitHub Actions
- Hosted documentation: [floraR-website](https://dboslab.github.io/floraR-website/), with How-To articles for every function and a full Portuguese translation (EN/PT language switcher)
- CRAN-ready package structure

### Workflow

The package supports a full workflow for working with Flora e Funga do Brasil data:

1. Check available versions with `flora_version()`
2. Download datasets with `flora_download()`
3. Parse datasets with `flora_parse()`
4. Filter and retrieve checklist records with `flora_records()`
5. Resolve your own species names with `flora_search()` and `flora_match()`
6. Explore the taxonomic hierarchy with `flora_get_children_taxa()`
7. Curate new records with `flora_get_descriptions()` and `flora_build_matrix()`

Most functions download and parse the FFB dataset automatically and cache it locally, so `flora_download()`/`flora_parse()` rarely need to be called directly unless you want to inspect the raw data.

### Target Users

- Researchers analyzing Brazilian plant diversity
- Taxonomic experts contributing to the Flora e Funga do Brasil
- Ecologists studying plant distributions and biogeography
- Conservation biologists working with Brazilian flora
- Educators and students in biodiversity informatics

### Feedback

Please report bugs or issues at:
<https://github.com/DBOSlab/floraR/issues>

### Citation

Cardoso, D. 2026. floraR: An R Package for Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil Platform. https://github.com/dboslab/floraR
