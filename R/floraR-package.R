#' The floraR package provides a comprehensive interface to the Flora e Funga do
#' Brasil (FFB) Platform, maintained by the Rio de Janeiro Botanical Garden.
#' It offers tools for downloading, parsing, and analyzing taxonomic and
#' distributional data published in Darwin Core Archive (DwC-A) format.
#'
#' Beyond data retrieval, floraR also assists taxonomic experts contributing to
#' the FFB by simplifying the integration of new species names and records from
#' global biodiversity repositories such as IPNI, REFLORA, and GBIF. The package
#' is designed to streamline both data exploration and data curation workflows,
#' integrating seamlessly with the tidyverse for downstream analyses and
#' reproducible research pipelines.
#'
#' The package's main functions \code{\link{flora_version}} retrieves metadata
#' about available dataset versions from the FFB IPT data portal.
#' \code{\link{flora_download}} downloads specific or all available dataset
#' versions in DwC-A format, while \code{\link{flora_parse}} parses and
#' organizes the locally downloaded data for analysis. These functions together
#' provide a complete workflow for accessing and analyzing Brazilian plant
#' diversity data.
#'
#' For researchers, floraR enables comprehensive analyses of taxonomic
#' distributions, endemism patterns, phytogeographic domains, life forms,
#' habitats, and vegetation types across Brazil. For taxonomic experts, it
#' facilitates data curation and integration of new records into the FFB
#' platform.
#'
#' For the most recent version of floraR, please visit the package's
#' GitHub repository (\url{https://github.com/dboslab/floraR}).
#'
#' @name floraR-package
#'
#' @aliases floraR-package
#'
#' @title Tools for Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil Platform
#'
#' @author \strong{Domingos Cardoso}\cr
#' (ORCID: \href{https://orcid.org/0000-0001-7072-2656}{0000-0001-7072-2656};
#' email: \email{domingoscardoso@@jbrj.gov.br};
#' Rio de Janeiro Botanical Garden, Brazil)
#'
#' @author \strong{Kelmer Martins-Cunha}\cr
#' (ORCID: \href{https://orcid.org/0000-0002-0545-5966}{0000-0002-0545-5966};
#' email: \email{kelmermartinscunha@@gmail.com})
#'
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab floraR\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2025-02-25\cr
#' License: \tab MIT\cr
#' }
#'
#' @references Cardoso, D. & Martins-Cunha, K. (2025). floraR: An R Package for
#' Accessing, Analyzing, and Curating Data from the Flora e Funga do Brasil
#' Platform.
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://dboslab.github.io/floraR-website/} - Package documentation website
#'   \item \url{https://github.com/DBOSlab/floraR} - Source code and issue tracker
#'   \item \url{https://floradobrasil.jbrj.gov.br/} - Flora e Funga do Brasil platform
#'   \item \url{https://ipt.jbrj.gov.br/jbrj} - FFB IPT data portal
#' }
#'
#' @import stringr utils finch tidyr magrittr dplyr purrr
#' @importFrom rlang .data
"_PACKAGE"

NULL
