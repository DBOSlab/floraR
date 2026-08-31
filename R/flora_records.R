#' Retrieve taxon records from the Flora e Funga do Brasil database
#'
#' @description
#' Retrieves and filters plant taxon records from the locally parsed
#' \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)}
#' taxonomic database, hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro
#' Botanical Garden}. Unlike \code{\link{flora_search}}, which resolves a list of
#' species names you already have against the FFB database, \code{flora_records()}
#' browses and filters the FFB checklist itself by taxonomic, geographic, and
#' trait-based criteria; no input name list required.
#'
#' @details
#' The function downloads (if needed) and parses the FFB dataset exactly like
#' \code{\link{flora_search}}, then applies the requested filters:
#' \itemize{
#'   \item \code{taxon} is matched against the \code{family}, \code{genus}, and
#'         \code{taxonName} columns of the FFB taxon table (family names ending
#'         in \code{"aceae"}, single-word genus names, and multi-word species
#'         names are detected automatically).
#'   \item \code{taxonRank} and \code{taxonomicStatus} are matched directly
#'         against the taxon table.
#'   \item \code{state}, \code{phytogeographicDomain}, and \code{endemism} are
#'         matched against the FFB distribution table (\code{distribution.txt}).
#'   \item \code{lifeForm}, \code{habitat}, and \code{vegetationType} are matched
#'         against the FFB species profile table (\code{speciesprofile.txt}).
#' }
#' All filters are combined with a logical AND. The result always has one row
#' per matching taxon (geographic/trait filters restrict \emph{which} taxa are
#' returned, they do not multiply rows).
#'
#' @usage
#' flora_records(
#'   taxon = NULL,
#'   taxonRank = NULL,
#'   taxonomicStatus = NULL,
#'   state = NULL,
#'   phytogeographicDomain = NULL,
#'   endemism = NULL,
#'   lifeForm = NULL,
#'   habitat = NULL,
#'   vegetationType = NULL,
#'   version = "latest",
#'   rm_flora_database = FALSE,
#'   verbose = TRUE,
#'   save = FALSE,
#'   dir = "flora_records",
#'   filename = "flora_records_search"
#' )
#'
#' @param taxon Character vector. One or more family, genus, or species names
#'   to filter by (e.g. \code{c("Fabaceae", "Luetzelburgia", "Inga edulis")}).
#'   \code{NULL} (default) does not filter by taxon.
#'
#' @param taxonRank Character vector. Taxonomic rank(s) to keep (e.g.
#'   \code{"ESPECIE"}, \code{"VARIEDADE"}, \code{"SUBESPECIE"}). \code{NULL}
#'   (default) keeps all ranks.
#'
#' @param taxonomicStatus Character vector. FFB taxonomic status to keep:
#'   \code{"NOME_ACEITO"} (accepted), \code{"SINONIMO"} (synonym), or
#'   \code{"NOME_DUVIDOSO"} (doubtful). \code{NULL} (default) keeps all statuses.
#'
#' @param state Character vector. Brazilian state(s) - full name or acronym,
#'   diacritics-insensitive (e.g. \code{"Bahia"} or \code{"BA"}) - to filter by
#'   occurrence, based on the FFB distribution table. \code{NULL} (default)
#'   does not filter by state.
#'
#' @param phytogeographicDomain Character vector. Phytogeographic domain(s) to
#'   filter by (e.g. \code{"Caatinga"}, \code{"Mata Atlantica"}). \code{NULL}
#'   (default) does not filter by domain.
#'
#' @param endemism Logical. If \code{TRUE}, keeps only taxa flagged as
#'   Brazilian endemics; if \code{FALSE}, keeps only non-endemics. \code{NULL}
#'   (default) does not filter by endemism.
#'
#' @param lifeForm,habitat,vegetationType Character vectors. Life form (e.g.
#'   \code{"Arbusto"}, \code{"Arvore"}), habitat (e.g. \code{"Terricola"}), and
#'   vegetation type (e.g. \code{"Cerrado (lato sensu)"}) to filter by, based on
#'   the FFB species profile table. \code{NULL} (default) does not filter.
#'
#' @param version Character. FFB dataset version to use. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}} and
#'   \code{\link{flora_parse}}.
#'
#' @param rm_flora_database Logical. If \code{TRUE}, the downloaded FFB database
#'   folder (\code{"flora_download"}) is deleted after the search is complete.
#'   If \code{FALSE} (default), the database is kept on disk and reused by
#'   subsequent calls, which significantly speeds up repeated queries.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative
#'   progress messages during download and parsing. If \code{FALSE}, runs
#'   quietly.
#'
#' @param save Logical. If \code{TRUE}, the filtered records are saved to disk
#'   as a CSV file. Default is \code{FALSE}.
#'
#' @param dir Character. Directory where the CSV file will be saved when
#'   \code{save = TRUE}. Defaults to \code{"flora_records"}.
#'
#' @param filename Character. Name of the CSV file (without extension) to save
#'   when \code{save = TRUE}. Defaults to \code{"flora_records_search"}.
#'
#' @return A \code{data.frame} of FFB taxon records matching the requested
#'   filters, with one row per taxon. If \code{save = TRUE}, the result is also
#'   written to \code{<dir>/<filename>.csv}.
#'
#' @section Database caching behavior:
#' The FFB dataset is downloaded only once and cached locally in the
#' \code{"flora_download"} folder. On subsequent calls:
#' \itemize{
#'   \item If \code{rm_flora_database = FALSE} (default), the function checks if
#'     the cached version matches the requested \code{version}. If yes, it
#'     reuses the existing download; if not, it downloads the correct version.
#'   \item If \code{rm_flora_database = TRUE}, the database is deleted after
#'     each call, forcing a fresh download on every call (not recommended for
#'     repeated use).
#' }
#'
#' @seealso
#' \code{\link{flora_search}} to resolve a list of names you already have.
#' \code{\link{flora_download}} to manually download the DwC-A dataset.
#' \code{\link{flora_parse}} to manually parse the downloaded dataset.
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # All accepted species in Fabaceae (downloads + parses automatically, caches result)
#' fabaceae <- flora_records(taxon = "Fabaceae",
#'                           taxonomicStatus = "NOME_ACEITO")
#'
#' # Accepted species endemic to Bahia
#' bahia_endemics <- flora_records(state = "Bahia",
#'                                 endemism = TRUE,
#'                                 taxonomicStatus = "NOME_ACEITO")
#'
#' # Species in the Caatinga with a shrub life form
#' caatinga_shrubs <- flora_records(phytogeographicDomain = "Caatinga",
#'                                  lifeForm = "Arbusto")
#'
#' # Use a specific FFB version stored in a custom folder, and save the result
#' flora_records(taxon = "Luetzelburgia",
#'              version = "393.418",
#'              save = TRUE,
#'              dir = "flora_records",
#'              filename = "luetzelburgia_records")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom stringi stri_trans_general
#'
#' @export

flora_records <- function(taxon = NULL,
                          taxonRank = NULL,
                          taxonomicStatus = NULL,
                          state = NULL,
                          phytogeographicDomain = NULL,
                          endemism = NULL,
                          lifeForm = NULL,
                          habitat = NULL,
                          vegetationType = NULL,
                          version = "latest",
                          rm_flora_database = FALSE,
                          verbose = TRUE,
                          save = FALSE,
                          dir = "flora_records",
                          filename = "flora_records_search") {

  ffb <- .flora_prepare_records(version = version,
                                verbose = verbose,
                                rm_flora_database = rm_flora_database)

  records <- ffb$taxon_df
  distribution_df <- ffb$distribution_df
  speciesprofile_df <- ffb$speciesprofile_df

  # Filter by taxon name (family/genus/species) -- reuses the existing
  # family/genus/species auto-detection logic (state handled separately below,
  # since FFB distribution data uses 'locationID', not 'stateProvince')
  if (!is.null(taxon)) {
    records <- .filter_occur_df(records, taxon = taxon, state = NULL, verbose = verbose)
  }

  # Filter by taxonRank
  if (!is.null(taxonRank)) {
    taxonRank <- toupper(trimws(taxonRank))
    records <- records[records$taxonRank %in% taxonRank, ]
  }

  # Filter by taxonomicStatus
  if (!is.null(taxonomicStatus)) {
    taxonomicStatus <- toupper(trimws(taxonomicStatus))
    records <- records[records$taxonomicStatus %in% taxonomicStatus, ]
  }

  # Filter by state, via the FFB distribution table
  if (!is.null(state)) {
    state_abbrev <- .arg_check_state(state, return_abbrev = TRUE)
    dist_ids <- distribution_df$id[gsub("^BR-", "", distribution_df$locationID) %in% state_abbrev]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by phytogeographic domain, via the FFB distribution table
  if (!is.null(phytogeographicDomain)) {
    domain_norm <- tolower(trimws(phytogeographicDomain))
    dist_domain_norm <- tolower(trimws(distribution_df$phytogeographicDomain))
    dist_ids <- distribution_df$id[dist_domain_norm %in% domain_norm]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by endemism, via the FFB distribution table
  if (!is.null(endemism)) {
    truthy <- c("true", "endemic", "endemica", "end\u00eamica", "sim")
    falsy  <- c("false", "not endemic", "nao endemica", "n\u00e3o end\u00eamica", "nao", "n\u00e3o")
    tokens <- if (isTRUE(endemism)) truthy else falsy
    dist_endemism_norm <- tolower(trimws(as.character(distribution_df$endemism)))
    dist_ids <- distribution_df$id[dist_endemism_norm %in% tokens]
    records <- records[records$id %in% dist_ids, ]
  }

  # Filter by lifeForm / habitat / vegetationType, via the FFB species profile table
  if (!is.null(lifeForm)) {
    lifeForm_norm <- tolower(trimws(lifeForm))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$lifeForm)) %in% lifeForm_norm]
    records <- records[records$id %in% sp_ids, ]
  }
  if (!is.null(habitat)) {
    habitat_norm <- tolower(trimws(habitat))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$habitat)) %in% habitat_norm]
    records <- records[records$id %in% sp_ids, ]
  }
  if (!is.null(vegetationType)) {
    vegetation_norm <- tolower(trimws(vegetationType))
    sp_ids <- speciesprofile_df$id[tolower(trimws(speciesprofile_df$vegetationType)) %in% vegetation_norm]
    records <- records[records$id %in% sp_ids, ]
  }

  rownames(records) <- NULL

  if (verbose) {
    message(sprintf("\n\u2713 Returned %d taxon record(s) matching the requested filters", nrow(records)))
  }

  if (save) {
    dir <- .arg_check_dir(dir)
    .save_csv(records, verbose = verbose, filename = filename, dir = dir)
  }

  return(records)
}


#_______________________________________________________________________________
# Download (if needed) and parse the FFB dataset, returning the taxon,
# distribution, and species profile tables shared by flora_records().
.flora_prepare_records <- function(version, verbose, rm_flora_database) {
  floraR::flora_download(version = version, dir = "flora_download", verbose = verbose)
  dwca <- floraR::flora_parse(path = "flora_download", version = version, verbose = verbose)

  key <- names(dwca)[1L]
  taxon_df <- dwca[[key]][["data"]][["taxon.txt"]]
  distribution_df <- dwca[[key]][["data"]][["distribution.txt"]]
  speciesprofile_df <- dwca[[key]][["data"]][["speciesprofile.txt"]]

  if (is.null(taxon_df)) {
    stop("No 'taxon.txt' table found in the parsed dataset. Run flora_parse() first.",
        call. = FALSE)
  }

  if (rm_flora_database) {
    unlink("flora_download", recursive = TRUE)
  }

  list(taxon_df = taxon_df,
      distribution_df = distribution_df,
      speciesprofile_df = speciesprofile_df)
}
