#' Find candidate new Brazilian state records for a plant or algal taxon
#'
#' @description
#' Given a genus or species of plants or algae, checks \href{https://www.gbif.org}{GBIF}
#' and, optionally, the \href{https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/}{Reflora
#' Virtual Herbarium} and \href{https://specieslink.net}{speciesLink} for specimen and
#' occurrence evidence of the taxon in Brazilian states that are \emph{not} currently
#' listed in its official Flora e Funga do Brasil (FFB) distribution. This flags
#' candidate new state records for a taxonomist to verify - e.g. a species listed by FFB
#' only for Bahia, but with herbarium specimens from Pernambuco - so that the
#' distributions in FFB monographs can be kept complete.
#'
#' @details
#' \strong{FFB side.} The officially listed states come from the parsed FFB taxon and
#' distribution tables (the same locally cached dataset as \code{\link{flora_records}}).
#' For a genus, the states of all its species and infraspecific taxa are combined; for a
#' species, those of the species and its infraspecific taxa. If the name is a synonym in
#' FFB, the distribution of its accepted name is used.
#'
#' \strong{Occurrence evidence.} Up to three sources can be queried (see
#' \code{sources}):
#' \describe{
#'   \item{\code{"gbif"} (default)}{GBIF's public API, which aggregates most Brazilian
#'     herbaria. The name is first resolved to its GBIF backbone key among plants and
#'     algae only (so, e.g., the red alga \emph{Gracilaria} is not confused with the
#'     moth genus of the same name), so the records of its synonyms and infraspecific
#'     taxa are included. All states are counted in a single request.}
#'   \item{\code{"reflora"}}{REFLORA herbarium specimens, via
#'     \code{\link[refloraR]{reflora_records}} (requires the \pkg{refloraR} package).
#'     Note that the first use downloads the REFLORA herbarium archives - several
#'     gigabytes - which \pkg{refloraR} then caches for later calls.}
#'   \item{\code{"speciesLink"}}{speciesLink's API, which requires a free API key
#'     (\code{specieslink_key}; create one at \url{https://specieslink.net}).}
#' }
#' Free-text state values in these sources (e.g. \code{"Ba"}, \code{"Est. do Bahia"},
#' \code{"Paraiba"}) are normalised to FFB state names before comparison; values that are
#' not Brazilian states are ignored.
#'
#' Occurrence records are evidence to review, not proof: a record in a new state may be
#' a cultivated plant, a misidentification, or a georeferencing error, which is why the
#' HTML report lists the individual records behind each candidate state.
#'
#' @usage
#' flora_distribution_gap(
#'   taxon,
#'   state = NULL,
#'   sources = "gbif",
#'   specieslink_key = Sys.getenv("SPECIESLINK_KEY"),
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "flora_distribution_gap",
#'   filename = NULL,
#'   html_report = TRUE,
#'   open_report = interactive()
#' )
#'
#' @param taxon Character. A single genus or species name of plants or algae (e.g.
#'   \code{"Luetzelburgia auriculata"} or \code{"Luetzelburgia"}).
#'
#' @param state Character vector. Optional subset of Brazilian states (full name or
#'   acronym) to restrict the check to. \code{NULL} (default) checks all states with any
#'   evidence.
#'
#' @param sources Character vector. Which occurrence sources to query: any of
#'   \code{"gbif"}, \code{"reflora"}, \code{"speciesLink"}. Defaults to \code{"gbif"},
#'   the fastest. See Details for the requirements of the other two.
#'
#' @param specieslink_key Character. speciesLink API key, used when
#'   \code{"speciesLink"} is among \code{sources}. Defaults to the
#'   \code{SPECIESLINK_KEY} environment variable.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result is saved to disk as an
#'   \code{.xlsx} spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheet is saved when \code{save = TRUE}.
#'   Defaults to \code{"flora_distribution_gap"}.
#'
#' @param filename Character. Name of the \code{.xlsx} file (without extension) to save.
#'   Defaults to \code{"flora_distribution_gap_<taxon>"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report (\code{<dir>/<filename>.html}) summarizing the results: KPI counts, an
#'   evidence-by-source breakdown, the full state-by-state table, and the individual
#'   occurrence records found in the new-state-record candidate states - each linking to
#'   the record in its source database - all as sortable, filterable \pkg{DT} widgets with
#'   buttons to copy or download them as CSV/Excel. Requires the \pkg{rmarkdown},
#'   \pkg{DT}, and \pkg{htmltools} packages; if any is missing, the report is skipped with
#'   a message (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE} (default in interactive sessions), opens
#'   the HTML report in the browser once rendered.
#'
#' @return A \code{data.frame}, one row per Brazilian state with any evidence for
#'   \code{taxon} (in FFB or in the queried sources), with columns:
#' \describe{
#'   \item{Taxon}{The queried taxon name.}
#'   \item{State}{Brazilian state name.}
#'   \item{In_FFB_distribution}{Logical; whether FFB already lists this state for the taxon.}
#'   \item{GBIF_records}{Number of GBIF occurrence records in this state (when
#'     \code{"gbif"} is among \code{sources}).}
#'   \item{REFLORA_records}{Number of REFLORA specimens in this state (when
#'     \code{"reflora"} is among \code{sources} and \pkg{refloraR} is installed).}
#'   \item{speciesLink_records}{Number of speciesLink records in this state (when
#'     \code{"speciesLink"} is among \code{sources} and a key is given).}
#'   \item{New_state_record_candidate}{\code{TRUE} when the state has evidence from at
#'     least one source but is \emph{not} in FFB's distribution - a candidate for
#'     taxonomic review.}
#' }
#'
#' @seealso \code{\link{flora_species_gap}}, \code{\link{flora_records}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # Candidate new state records for a species
#' gap <- flora_distribution_gap(taxon = "Luetzelburgia auriculata")
#'
#' # A whole genus, restricted to two states
#' gap_ne <- flora_distribution_gap(taxon = "Luetzelburgia",
#'                                  state = c("Pernambuco", "Paraiba"))
#'
#' # Adding REFLORA herbarium specimens (requires refloraR)
#' gap_reflora <- flora_distribution_gap(taxon = "Luetzelburgia auriculata",
#'                                       sources = c("gbif", "reflora"))
#' }
#'
#' @importFrom openxlsx write.xlsx
#'
#' @export

flora_distribution_gap <- function(taxon,
                                   state = NULL,
                                   sources = "gbif",
                                   specieslink_key = Sys.getenv("SPECIESLINK_KEY"),
                                   version = "latest",
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "flora_distribution_gap",
                                   filename = NULL,
                                   html_report = TRUE,
                                   open_report = interactive()) {

  if (missing(taxon) || is.null(taxon) || !is.character(taxon) || length(taxon) != 1) {
    stop("'taxon' must be a single character string (one genus or species name).",
         call. = FALSE)
  }
  sources <- match.arg(sources, c("gbif", "reflora", "speciesLink"), several.ok = TRUE)
  taxon <- trimws(gsub("\\s+", " ", taxon))

  if (is.null(filename)) {
    filename <- paste0("flora_distribution_gap_", gsub("\\s+", "_", taxon))
  }

  # ------------------------------------------------------------------------
  # 1. FFB's officially listed states for this taxon
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving FFB's officially listed states for '", taxon, "'...")

  ffb <- .flora_prepare_records(version = version, verbose = FALSE, rm_flora_database = FALSE)
  ffb_taxon <- .ffb_taxon_states(taxon, ffb$taxon_df, ffb$distribution_df)
  ffb_states <- ffb_taxon$states

  if (verbose) {
    if (!ffb_taxon$found) {
      message("  '", taxon, "' was not found in the current FFB checklist ",
              "(treating FFB's officially listed states as empty).")
    } else {
      if (ffb_taxon$name != taxon) {
        message("  '", taxon, "' is a synonym in FFB; using the distribution of its ",
                "accepted name, '", ffb_taxon$name, "'.")
      }
      if (length(ffb_states) == 0) {
        message("  FFB lists no Brazilian state for '", ffb_taxon$name, "' (as for some ",
                "marine algae), so every state with records will be flagged.")
      } else {
        message("  FFB lists ", length(ffb_states), " state(s).")
      }
    }
  }

  # ------------------------------------------------------------------------
  # 2. Occurrence evidence, one source at a time
  # ------------------------------------------------------------------------
  gbif_df <- data.frame(State = character(0), GBIF_records = integer(0), stringsAsFactors = FALSE)
  gbif_key <- NULL
  gbif_raw <- list()
  if ("gbif" %in% sources) {
    if (verbose) message("Querying GBIF for Brazilian occurrence records...")
    gbif_key <- .gbif_plant_key(taxon)
    if (is.null(gbif_key)) {
      if (verbose) message("  '", taxon, "' was not found in GBIF among plants and algae; ",
                           "skipping GBIF.")
    } else {
      gbif <- .gbif_brazil_state_counts(gbif_key$key)
      if (is.null(gbif)) {
        if (verbose) message("  The GBIF request failed; skipping GBIF.")
      } else {
        gbif_df <- gbif$state_counts
        gbif_raw <- gbif$raw_states
        if (verbose) {
          message("  ", gbif$n_records, " Brazilian record(s) in ", nrow(gbif_df), " state(s).")
        }
      }
    }
  }

  reflora_df <- data.frame(State = character(0), REFLORA_records = integer(0),
                           stringsAsFactors = FALSE)
  reflora_result <- NULL
  if ("reflora" %in% sources) {
    if (!requireNamespace("refloraR", quietly = TRUE)) {
      if (verbose) {
        message("  Package 'refloraR' is not installed; skipping REFLORA. Install it ",
                "with remotes::install_github('DBOSlab/refloraR') to include this source.")
      }
    } else {
      if (verbose) {
        message("Querying REFLORA herbarium specimens (via refloraR; the first use ",
                "downloads the REFLORA archives)...")
      }
      reflora_result <- tryCatch({
        refloraR::reflora_records(taxon = taxon, verbose = FALSE, save = FALSE)
      }, error = function(e) {
        if (verbose) message("  REFLORA query failed: ", conditionMessage(e))
        NULL
      })
      if (!is.null(reflora_result) && nrow(reflora_result) > 0 &&
          "stateProvince" %in% names(reflora_result)) {
        st <- .normalize_br_state(reflora_result$stateProvince)
        st <- st[!is.na(st)]
        if (length(st) > 0) {
          tab <- table(st)
          reflora_df <- data.frame(State = names(tab), REFLORA_records = as.integer(tab),
                                   stringsAsFactors = FALSE)
        }
      }
    }
  }

  splink_df <- data.frame(State = character(0), speciesLink_records = integer(0),
                          stringsAsFactors = FALSE)
  if ("speciesLink" %in% sources) {
    if (is.null(specieslink_key) || !nzchar(specieslink_key)) {
      if (verbose) {
        message("  No speciesLink API key given; skipping speciesLink. Create a free key ",
                "at https://specieslink.net and pass it as 'specieslink_key' or set the ",
                "SPECIESLINK_KEY environment variable.")
      }
    } else {
      if (verbose) message("Querying speciesLink for Brazilian records...")
      splink <- .splink_brazil_state_counts(taxon, specieslink_key)
      if (is.null(splink)) {
        if (verbose) message("  The speciesLink request failed; skipping speciesLink.")
      } else {
        splink_df <- splink
      }
    }
  }

  # ------------------------------------------------------------------------
  # 3. One row per state with ANY evidence (FFB or external)
  # ------------------------------------------------------------------------
  all_states <- unique(c(ffb_states, gbif_df$State, reflora_df$State, splink_df$State))

  if (!is.null(state)) {
    requested <- .normalize_br_state(state)
    if (any(is.na(requested))) {
      stop("Unrecognised Brazilian state(s) in 'state': ",
           paste(state[is.na(requested)], collapse = ", "), call. = FALSE)
    }
    all_states <- intersect(all_states, requested)
  }

  empty <- data.frame(Taxon = character(0), State = character(0),
                      In_FFB_distribution = logical(0),
                      New_state_record_candidate = logical(0), stringsAsFactors = FALSE)
  if (length(all_states) == 0) {
    if (verbose) message("\nNo evidence found for '", taxon, "' in the requested states or sources.")
    return(empty)
  }

  result <- data.frame(Taxon = taxon, State = sort(all_states), stringsAsFactors = FALSE)
  result$In_FFB_distribution <- result$State %in% ffb_states
  fill <- function(df, col) {
    v <- df[[col]][match(result$State, df$State)]
    v[is.na(v)] <- 0L
    v
  }
  if ("gbif" %in% sources) result$GBIF_records <- fill(gbif_df, "GBIF_records")
  if ("reflora" %in% sources) result$REFLORA_records <- fill(reflora_df, "REFLORA_records")
  if ("speciesLink" %in% sources) {
    result$speciesLink_records <- fill(splink_df, "speciesLink_records")
  }

  external <- unique(c(gbif_df$State, reflora_df$State, splink_df$State))
  result$New_state_record_candidate <- !result$In_FFB_distribution & result$State %in% external
  rownames(result) <- NULL

  n_new <- sum(result$New_state_record_candidate)
  if (verbose) {
    message(sprintf("\n\u2713 %d state(s) with evidence for '%s', %d not yet in FFB's distribution",
                    nrow(result), taxon, n_new))
  }

  if (save) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report) {
    source_summary <- data.frame(
      Source = c("FFB (official)", "GBIF", "REFLORA", "speciesLink"),
      States = c(length(ffb_states), nrow(gbif_df), nrow(reflora_df), nrow(splink_df)),
      Records = c(NA_integer_, sum(gbif_df$GBIF_records), sum(reflora_df$REFLORA_records),
                  sum(splink_df$speciesLink_records)),
      stringsAsFactors = FALSE
    )
    source_summary <- source_summary[c(TRUE, c("gbif", "reflora", "speciesLink") %in% sources), ]

    # ---- Individual records behind each new-state-record candidate, each
    # linking to the record itself in its source database
    candidates <- result$State[result$New_state_record_candidate]
    records_detail <- data.frame(Source = character(0), State = character(0),
                                 Municipality = character(0), Locality = character(0),
                                 RecordedBy = character(0), RecordNumber = character(0),
                                 EventDate = character(0), Institution = character(0),
                                 CatalogNumber = character(0), Identified_as = character(0),
                                 RecordLink = character(0), stringsAsFactors = FALSE)

    if (!is.null(gbif_key) && length(candidates) > 0) {
      if (verbose) message("Fetching the GBIF records behind the new-state candidates...")
      for (st in candidates) {
        recs <- .gbif_brazil_state_records(gbif_key$key, gbif_raw[[st]])
        if (nrow(recs) > 0) {
          records_detail <- rbind(records_detail,
                                  cbind(data.frame(Source = "GBIF", State = st,
                                                   stringsAsFactors = FALSE), recs))
        }
      }
    }

    if (!is.null(reflora_result) && nrow(reflora_result) > 0 && length(candidates) > 0 &&
        "stateProvince" %in% names(reflora_result)) {
      st <- .normalize_br_state(reflora_result$stateProvince)
      rc <- reflora_result[st %in% candidates, , drop = FALSE]
      if (nrow(rc) > 0) {
        col <- function(nm) if (nm %in% names(rc)) as.character(rc[[nm]]) else NA_character_
        records_detail <- rbind(records_detail, data.frame(
          Source = "REFLORA",
          State = .normalize_br_state(rc$stateProvince),
          Municipality = col("municipality"), Locality = col("locality"),
          RecordedBy = col("recordedBy"), RecordNumber = col("recordNumber"),
          EventDate = col("year"), Institution = col("herbarium"),
          CatalogNumber = col("catalogNumber"), Identified_as = col("scientificName"),
          RecordLink = col("bibliographicCitation"),
          stringsAsFactors = FALSE
        ))
      }
    }

    report_data <- list(
      taxon = taxon,
      ffb_name = ffb_taxon$name,
      ffb_found = ffb_taxon$found,
      gbif_name = if (is.null(gbif_key)) NA_character_ else gbif_key$name,
      n_in_ffb = length(ffb_states),
      n_states_total = nrow(result),
      n_new_candidates = n_new,
      source_summary = source_summary,
      result = result,
      records_detail = records_detail
    )

    dir <- .arg_check_dir(dir)
    .flora_render_report(template = "flora_distribution_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}
