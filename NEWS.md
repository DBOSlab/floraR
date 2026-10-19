# floraR (development version)

### New features

- `flora_records()`: `endemism = TRUE` combined with a `state`, `phytogeographicDomain`, or `vegetationType` now returns the endemics *exclusive* to those areas - taxa whose whole FFB range lies within them - instead of every Brazilian endemic that merely occurs there (e.g. 2,111 accepted taxa endemic to Bahia, rather than the 6,655 Brazilian endemics recorded in Bahia). Without an area filter, `endemism = TRUE` still returns all Brazilian endemics. The previous behaviour is available as `endemism = "Brazil"`.
- `flora_records()`: new `establishmentMeans` argument - the taxon's origin in Brazil, as recorded by FFB - to keep native (`"native"`/`"nativa"`) or introduced (`"introduced"`/`"exotica"`) taxa.
- `flora_records()`: every result now carries each taxon's FFB distribution and species profile as extra columns - `state`, `phytogeographicDomain`, `endemism`, `establishmentMeans`, `lifeForm`, `habitat`, and `vegetationType` - with the taxon's values joined in one cell (e.g. `"BA | PE | PI"`). They always describe the taxon's full FFB record, whatever filters were applied.
- `flora_records()`: text filters (`phytogeographicDomain`, `vegetationType`, `lifeForm`, `habitat`) now ignore case and accents, so the documented examples `"Mata Atlantica"` and `"Arvore"` now match FFB's "Mata Atlântica" and "Árvore" (they returned nothing before).
- `flora_species_gap()`: Find species of a genus of plants or algae that occur in Brazil but are still missing from FFB, combining two kinds of evidence: the species distributions of Plants of the World Online (POWO) and type specimens collected in Brazil (GBIF). Returns one table, one row per name, stating which source(s) reported it, with names reported by both listed first. POWO's data is read locally from 'rWCVPdata' by default, or live from ChecklistBank with `powo_source = "checklistbank"`; each GBIF name comes with a summary of its Brazilian type specimens. Names FFB holds under a different genus or spelling (e.g. *Luetzelburgia andrade-limae* vs. *L. andradelimae*) are recognised. Saves an `.xlsx` spreadsheet and an HTML report.

- `flora_distribution_gap()`: Find candidate new Brazilian state records for a genus or species of plants or algae: states with specimen evidence in GBIF (and optionally REFLORA and speciesLink) that are not yet in its official FFB distribution. The HTML report lists the individual records behind each candidate state.
- `flora_distribution_gap()` now also places georeferenced records in the six Brazilian phytogeographic domains (Amazônia, Caatinga, Cerrado, Mata Atlântica, Pampa, Pantanal), using IBGE's biome map (2019) bundled with the package, and flags candidate new domain records. It returns a list of three tables - `states`, `domains`, and `records` - saved as spreadsheet sheets, and the HTML report adds an interactive map of the records over the domains, coloured by candidate status. New arguments `domains` and `max_coordinates`; 'sf' and 'leaflet' are suggested packages.
- A workflow diagram of the package, in the README and on the website.

### Bug fixes

- `flora_build_matrix()` now also handles free descriptions (`descriptions[[2]]`), turning them into a taxon-by-organ matrix (Habit, Leaf, Panicle, Flower, Samara, Seed, ...) aligned by organ across taxa. Before, it treated the first taxon's sentences as column names and left every other taxon empty. The kind of description is detected automatically, or set with `type = "controlled"` or `type = "free"`.
- `flora_get_descriptions()` now reads the text of each FFB page directly in a headless Chrome browser (via 'chromote') instead of from a PDF of it. The PDF clipped the right end of every line, silently losing words in both the controlled and the free descriptions (e.g. "leaflet margin commonly denticulate" was read as "leaflet denticulate"). It is also much faster: one browser session is reused for all taxa.
- `flora_get_descriptions()` no longer fails with "Error in i0:i1 : argument of length 0" when a description's closing section header is not found (e.g. when the description ran onto the next PDF page); that taxon is recorded as a failure instead. Language tag lines ("EN"/"PT") and non-breaking spaces no longer leak into the extracted text.
- `library(floraR)` no longer prints "Registered S3 method overwritten by 'hoardr'" when `httr` is already loaded. The `finch` package is now loaded quietly only when `flora_parse()` needs it.

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

Cardoso, D. 2026. floraR: An R Package for Accessing, Analyzing, and Curating Plant and Algae Data from the Flora e Funga do Brasil Platform. https://github.com/dboslab/floraR
