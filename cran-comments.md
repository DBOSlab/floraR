## CRAN submission for floraR 1.0.0

### Package Summary

* **Package name:** floraR  
* **Version:** 1.0.0  
* **Title:** Tools for Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil Platform  
* **Authors:** Domingos Cardoso, Kelmer Martins-Cunha  
* **Maintainer:** Domingos Cardoso <domingoscardoso@jbrj.gov.br>  
* **License:** MIT  
* **URL:** https://github.com/DBOSlab/floraR  
* **BugReports:** https://github.com/DBOSlab/floraR/issues  

### Submission Notes

* This is the initial submission of the `floraR` package to CRAN.
* The package provides tools to access, download, parse, and analyze taxonomic and distributional data from the Flora e Funga do Brasil (FFB) platform maintained by the Rio de Janeiro Botanical Garden (JBRJ).
* It supports both data exploration for researchers and data curation workflows for taxonomic experts contributing to the FFB.
* The package leverages standard packages (`dplyr`, `tidyr`, `stringr`, `finch`, `purrr`, `magrittr`) and integrates seamlessly with tidyverse workflows.
* All exported functions are documented with reproducible examples using `\dontrun{}` for functions requiring internet access.
* Unit tests using `testthat` cover all key functionalities, with comprehensive test coverage.

### R CMD check

* `R CMD check` was run on:
  - Local machine (macOS, R 4.3.2)
  - GitHub Actions CI/CD pipeline (Ubuntu, Windows, macOS)

  Results:  
  ❯ 0 errors ✔  
  ❯ 0 warnings ✔  
  ❯ 0 notes ✔ 

### Test Environments

* Local: macOS Ventura, R 4.3.2
* GitHub Actions:
  - Ubuntu 22.04, R-release
  - macOS 13, R-release
  - Windows 2022, R-release
  - Ubuntu 22.04, R-devel

### Additional Information

* This package contributes to biodiversity informatics by facilitating access to one of the largest and most comprehensive databases of Brazilian plant diversity.
* The package supports offline data analysis once datasets are downloaded, making it suitable for researchers with limited internet access.
* Development supported by institutional resources from the Rio de Janeiro Botanical Garden (JBRJ) and the Brazilian National Council for Scientific and Technological Development (CNPq).
* The package follows FAIR data principles by providing structured access to Darwin Core Archive (DwC-A) format data.
* All network-dependent functions have appropriate error handling for offline use cases.

### Reverse Dependencies

* This is a new package with no reverse dependencies.

### Data Sources

* The package accesses data from the Flora e Funga do Brasil IPT data portal: https://ipt.jbrj.gov.br/jbrj
* All data is publicly available under open access policies.
* The package does not redistribute any data but provides tools to access and analyze the data.