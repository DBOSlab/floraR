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
#'   \item \code{state}, \code{phytogeographicDomain}, \code{endemism}, and
#'         \code{establishmentMeans} are matched against the FFB distribution table
#'         (\code{distribution.txt}).
#'   \item \code{lifeForm}, \code{habitat}, and \code{vegetationType} are matched
#'         against the FFB species profile table (\code{speciesprofile.txt}).
#' }
#' Text filters are case- and accent-insensitive (\code{"Mata Atlantica"} matches
#' \code{"Mata Atl\u00e2ntica"}, \code{"Arvore"} matches \code{"\u00c1rvore"}).
#' All filters are combined with a logical AND. The result always has one row
#' per matching taxon (geographic/trait filters restrict \emph{which} taxa are
#' returned, they do not multiply rows).
#'
#' @section Endemism:
#' FFB's own \code{endemism} flag means \emph{endemic to Brazil}. How
#' \code{endemism} is applied therefore depends on whether an area filter -
#' \code{state}, \code{phytogeographicDomain}, or \code{vegetationType} - is also
#' given:
#' \itemize{
#'   \item \code{endemism = TRUE} without an area filter returns all Brazilian
#'     endemics. \strong{With} an area filter it returns the endemics
#'     \emph{exclusive} to those areas: Brazilian endemics whose entire FFB range
#'     lies within them. For example, \code{state = "Bahia", endemism = TRUE}
#'     returns the taxa found only in Bahia, not every Brazilian endemic that also
#'     occurs there. Several values of one filter define one area (\code{state =
#'     c("BA", "PE")}: taxa restricted to Bahia and/or Pernambuco), and with
#'     several area filters the taxon must be exclusive to each (\code{state =
#'     "Bahia", phytogeographicDomain = "Caatinga"}: taxa found only in Bahia and
#'     only in the Caatinga). A taxon with no record for an area filter (e.g. no
#'     state-level distribution) cannot be shown to be exclusive to it and is not
#'     returned.
#'   \item \code{endemism = "Brazil"} keeps the Brazilian endemics that
#'     \emph{occur} in the requested areas, wherever else in Brazil they also
#'     occur (e.g. the Brazilian endemics recorded in Bahia).
#'   \item \code{endemism = FALSE} without an area filter returns the taxa not
#'     endemic to Brazil; with an area filter, the taxa occurring in those areas
#'     that are \emph{not} exclusive to them.
#' }
#'
#' @usage
#' flora_records(
#'   taxon = NULL,
#'   taxonRank = NULL,
#'   taxonomicStatus = NULL,
#'   state = NULL,
#'   phytogeographicDomain = NULL,
#'   endemism = NULL,
#'   establishmentMeans = NULL,
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
#'   occurrence, based on the FFB distribution table (or, with \code{endemism =
#'   TRUE}, by exclusive occurrence; see the Endemism section). \code{NULL}
#'   (default) does not filter by state.
#'
#' @param phytogeographicDomain Character vector. Phytogeographic domain(s) to
#'   filter by (e.g. \code{"Caatinga"}, \code{"Mata Atlantica"}). \code{NULL}
#'   (default) does not filter by domain.
#'
#' @param endemism \code{TRUE}, \code{FALSE}, \code{"Brazil"}, or \code{NULL}.
#'   \code{TRUE} keeps endemics: Brazilian endemics without an area filter, or
#'   the endemics exclusive to the requested \code{state},
#'   \code{phytogeographicDomain}, or \code{vegetationType}. \code{"Brazil"}
#'   keeps Brazilian endemics occurring in those areas. \code{FALSE} keeps the
#'   complement of \code{TRUE}. \code{NULL} (default) does not filter by
#'   endemism. See the Endemism section.
#'
#' @param establishmentMeans Character vector. The origin of the taxon in Brazil -
#'   whether it is native to the country or was introduced into it - as recorded
#'   in FFB's \code{establishmentMeans} field: \code{"native"} (FFB's
#'   \code{"Nativa"}) or \code{"introduced"} (FFB's \code{"Ex\u00f3tica"}, i.e.
#'   exotic). English or Portuguese, case- and accent-insensitive:
#'   \code{"native"}/\code{"nativa"}, \code{"introduced"}/\code{"exotic"}/
#'   \code{"exotica"}. \code{NULL} (default) does not filter by origin; taxa
#'   whose origin FFB does not record are only returned then.
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
#'   filters, with one row per taxon. Besides the taxon fields, each row carries
#'   the taxon's FFB distribution and species profile, the unique values of each
#'   field joined in one cell with \code{" | "} (e.g. \code{"BA | MG | PE"}):
#'   \code{state} (abbreviations), \code{phytogeographicDomain},
#'   \code{endemism}, \code{establishmentMeans}, \code{lifeForm},
#'   \code{habitat}, and \code{vegetationType} - always describing the taxon's
#'   full record, whatever filters were applied. A field FFB does not record for
#'   a taxon is \code{NA}. If \code{save = TRUE}, the result is also
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
#' # All accepted species endemic to Brazil
#' brazil_endemics <- flora_records(endemism = TRUE,
#'                                  taxonomicStatus = "NOME_ACEITO")
#'
#' # Accepted species endemic to Bahia: found only in Bahia
#' bahia_endemics <- flora_records(state = "Bahia",
#'                                 endemism = TRUE,
#'                                 taxonomicStatus = "NOME_ACEITO")
#'
#' # Accepted Brazilian endemics that occur in Bahia (and possibly elsewhere)
#' brazil_endemics_in_bahia <- flora_records(state = "Bahia",
#'                                           endemism = "Brazil",
#'                                           taxonomicStatus = "NOME_ACEITO")
#'
#' # Species endemic to the Caatinga of Bahia: found only in Bahia, only in the Caatinga
#' bahia_caatinga_endemics <- flora_records(state = "Bahia",
#'                                          phytogeographicDomain = "Caatinga",
#'                                          endemism = TRUE)
#'
#' # Introduced (exotic) species recorded in Bahia
#' bahia_introduced <- flora_records(state = "Bahia",
#'                                   establishmentMeans = "introduced")
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
                          establishmentMeans = NULL,
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

  # Validate endemism: TRUE, FALSE, "Brazil", or NULL
  if (!is.null(endemism) &&
      !(isTRUE(endemism) || isFALSE(endemism) || identical(tolower(endemism), "brazil"))) {
    stop("'endemism' must be TRUE, FALSE, \"Brazil\", or NULL.", call. = FALSE)
  }
  brazil_mode <- is.character(endemism)

  # Each taxon's states, domains, and vegetation types (accent-insensitive
  # keys), as recorded by FFB: the distribution table has one row per
  # taxon x state x domain, the species profile one per taxon x life form x
  # vegetation type
  dist_state <- gsub("^BR-", "", distribution_df$locationID)
  dist_domain <- .ffb_key(distribution_df$phytogeographicDomain)
  prof_vegetation <- .ffb_key(speciesprofile_df$vegetationType)

  # Area filters: list(requested values, each record's value, record ids)
  areas <- list()
  if (!is.null(state)) {
    areas$state <- list(wanted = .arg_check_state(state, return_abbrev = TRUE),
                        value = dist_state, id = distribution_df$id)
  }
  if (!is.null(phytogeographicDomain)) {
    areas$phytogeographicDomain <- list(wanted = .ffb_key(phytogeographicDomain),
                                        value = dist_domain, id = distribution_df$id)
  }
  if (!is.null(vegetationType)) {
    areas$vegetationType <- list(wanted = .ffb_key(vegetationType),
                                 value = prof_vegetation, id = speciesprofile_df$id)
  }

  # Occurrence: the taxon is recorded in at least one requested area
  for (area in areas) {
    occurs <- unique(area$id[area$value %in% area$wanted])
    records <- records[records$id %in% occurs, ]
  }

  # Endemism
  if (!is.null(endemism)) {
    # FFB writes "Endemica" / "Nao endemica" (accent-insensitive keys)
    dist_endemism <- .ffb_key(distribution_df$endemism)
    brazil_endemic <- unique(distribution_df$id[dist_endemism %in%
                                                  c("endemica", "endemic", "true", "sim")])

    if (brazil_mode || length(areas) == 0) {
      # Endemic (or not) to Brazil, regardless of the areas it occurs in
      keep <- if (isFALSE(endemism)) {
        unique(distribution_df$id[dist_endemism %in%
                                    c("nao endemica", "not endemic", "false", "nao")])
      } else {
        brazil_endemic
      }
      records <- records[records$id %in% keep, ]
    } else {
      # Exclusive to the requested areas: a Brazilian endemic with every
      # recorded value of each area filter within the requested ones
      exclusive <- records$id[records$id %in% brazil_endemic]
      for (area in areas) {
        known <- !is.na(area$value) & nzchar(area$value)
        outside <- unique(area$id[known & !area$value %in% area$wanted])
        with_record <- unique(area$id[known])
        exclusive <- exclusive[exclusive %in% with_record & !exclusive %in% outside]
      }
      records <- if (isTRUE(endemism)) {
        records[records$id %in% exclusive, ]
      } else {
        records[!records$id %in% exclusive, ]
      }
    }
  }

  # Filter by origin in Brazil (FFB's establishmentMeans: "Nativa" or "Exotica")
  if (!is.null(establishmentMeans)) {
    origin_key <- .ffb_key(establishmentMeans)
    ffb_origin <- ifelse(origin_key %in% c("native", "nativa", "nativo"), "nativa",
                         ifelse(origin_key %in% c("introduced", "exotic", "exotica",
                                                  "exotico", "introduzida"), "exotica", NA))
    if (anyNA(ffb_origin)) {
      stop("Unrecognised 'establishmentMeans': ",
           paste(establishmentMeans[is.na(ffb_origin)], collapse = ", "),
           ". Use \"native\" (or \"nativa\") or \"introduced\" (or \"exotica\").",
           call. = FALSE)
    }
    origin_ids <- distribution_df$id[.ffb_key(distribution_df$establishmentMeans) %in% ffb_origin]
    records <- records[records$id %in% origin_ids, ]
  }

  # Filter by lifeForm / habitat, via the FFB species profile table
  if (!is.null(lifeForm)) {
    sp_ids <- speciesprofile_df$id[.ffb_key(speciesprofile_df$lifeForm) %in% .ffb_key(lifeForm)]
    records <- records[records$id %in% sp_ids, ]
  }
  if (!is.null(habitat)) {
    sp_ids <- speciesprofile_df$id[.ffb_key(speciesprofile_df$habitat) %in% .ffb_key(habitat)]
    records <- records[records$id %in% sp_ids, ]
  }

  # Bring each taxon's distribution and species profile into the table, one
  # column per field, its values joined in one cell ("AM | BA | PA")
  records <- .add_ffb_profile(records, distribution_df, speciesprofile_df)

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
# Add the taxon's FFB distribution (state, phytogeographicDomain, endemism,
# establishmentMeans) and species profile (lifeForm, habitat, vegetationType)
# to the taxon records, the unique values of each field joined with " | "
.add_ffb_profile <- function(records, distribution_df, speciesprofile_df, sep = " | ") {
  ids <- records$id
  dist <- distribution_df
  prof <- speciesprofile_df
  if (!is.null(dist) && !is.null(dist$locationID)) {
    dist$state <- gsub("^BR-", "", dist$locationID)
  }
  fields <- list(state = dist, phytogeographicDomain = dist, endemism = dist,
                 establishmentMeans = dist, lifeForm = prof, habitat = prof,
                 vegetationType = prof)
  for (field in names(fields)) {
    df <- fields[[field]]
    records[[field]] <- if (is.null(df) || is.null(df[[field]])) {
      rep(NA_character_, length(ids))
    } else {
      .collapse_by_id(df$id, df[[field]], ids, sep = sep)
    }
  }
  records
}

# One string per id in 'ids': the sorted unique non-empty values recorded for
# it, joined with 'sep'; NA when it has none
.collapse_by_id <- function(id, value, ids, sep = " | ") {
  value <- trimws(as.character(value))
  keep <- id %in% ids & !is.na(value) & nzchar(value)
  d <- unique(data.frame(id = id[keep], value = value[keep], stringsAsFactors = FALSE))
  d <- d[order(d$id, stringi::stri_trans_general(d$value, "Latin-ASCII")), ]
  joined <- vapply(split(d$value, d$id), paste, character(1), collapse = sep)
  unname(joined[as.character(ids)])
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


#_______________________________________________________________________________
# Case-, accent-, and whitespace-insensitive key for matching FFB text values
# ("Mata Atlantica" matches "Mata Atl\u00e2ntica", "Arvore" matches "\u00c1rvore") ####
.ffb_key <- function(x) {
  tolower(trimws(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
}
