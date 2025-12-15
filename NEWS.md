# refloraR 1.0.0

## Initial Release

The first official release of the `floraR` R package, designed to provide comprehensive access, analysis, and curation tools for data from the Flora e Funga do Brasil (FFB) platform maintained by the Rio de Janeiro Botanical Garden.

### Features

- `flora_version()`: Retrieve metadata and check available dataset versions from the Flora e Funga do Brasil IPT data portal.
- `flora_download()`: Download taxonomic and distributional data in Darwin Core Archive (DwC-A) format for specific or all available versions.
- `flora_parse()`: Parse and organize locally downloaded FFB datasets, extracting structured information from DwC-A files.
- Automated data cleaning and standardization of taxon names, distribution data, and species profiles.
- Support for offline data analysis once datasets are downloaded.
- Integration with global biodiversity repositories (IPNI, REFLORA, GBIF) for data curation workflows.
- Seamless integration with tidyverse packages for downstream analyses.

### Key Capabilities

- **Version Control**: Track, download, and parse specific dataset versions
- **Taxonomic Data**: Access complete taxonomic hierarchies and nomenclature
- **Distribution Data**: Extract endemism status and phytogeographic domain information
- **Species Profiles**: Parse life form, habitat, and vegetation type data
- **Data Curation**: Tools for taxonomic experts to integrate new records and names
- **Offline Analysis**: Work with downloaded data without internet connection

### Infrastructure

- MIT license
- Comprehensive test coverage with testthat
- Continuous integration via GitHub Actions
- Hosted documentation: [floraR-website](https://dboslab.github.io/floraR-website/)
- CRAN-ready package structure

### Workflow

The package follows a three-step workflow:
1. Check available versions with `flora_version()`
2. Download datasets with `flora_download()`
3. Parse and analyze with `flora_parse()` and `flora_records()`

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

Cardoso, D. & Martins-Cunha, K. (2025). floraR: An R Package for Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil Platform. https://github.com/dboslab/floraR