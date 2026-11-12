#' Find candidate new Brazilian state records for plant or algal taxa
#'
#' @description
#' Given one or more genera or species of plants or algae, checks \href{https://www.gbif.org}{GBIF},
#' the \href{https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/}{Reflora Virtual
#' Herbarium}, \href{https://jabot.jbrj.gov.br}{JABOT}, and, optionally,
#' \href{https://specieslink.net}{speciesLink} for specimen and occurrence evidence of the taxon in Brazilian states that are \emph{not} currently
#' listed in its official Flora e Funga do Brasil (FFB) distribution. This flags
#' candidate new state records for a taxonomist to verify - e.g. a species listed by FFB
#' only for Bahia, but with herbarium specimens from Pernambuco - so that the
#' distributions in FFB monographs can be kept complete. Georeferenced records are also
#' placed in the six Brazilian phytogeographic domains, to flag candidate new domain
#' records in the same way.
#'
#' @details
#' \strong{FFB side.} The officially listed states come from the parsed FFB taxon and
#' distribution tables (the same locally cached dataset as \code{\link{flora_records}}).
#' For a genus, the states of all its species and infraspecific taxa are combined; for a
#' species, those of the species and its infraspecific taxa. If the name is a synonym in
#' FFB, the distribution of its accepted name is used.
#'
#' \strong{Occurrence evidence.} Up to four sources can be queried (see
#' \code{sources}); by default GBIF, REFLORA, and JABOT are all queried, and their
#' evidence is combined, so that a state reported by any one of them is flagged:
#' \describe{
#'   \item{\code{"gbif"}}{GBIF's public API, which aggregates most Brazilian
#'     herbaria. The name is first resolved to its GBIF backbone key among plants and
#'     algae only (so, e.g., the red alga \emph{Gracilaria} is not confused with the
#'     moth genus of the same name), so the records of its synonyms and infraspecific
#'     taxa are included. All states are counted in a single request.}
#'   \item{\code{"reflora"}}{REFLORA herbarium specimens, via
#'     \code{\link[refloraR]{reflora_records}} (requires the \pkg{refloraR} package).}
#'   \item{\code{"jabot"}}{Specimens of the herbaria hosted by JABOT, the Rio de
#'     Janeiro Botanical Garden's herbarium system, via
#'     \code{\link[jabotR]{jabot_records}} (requires the \pkg{jabotR} package).}
#'   \item{\code{"speciesLink"}}{speciesLink's API, which requires a free API key
#'     (\code{specieslink_key}; create one at \url{https://specieslink.net}).}
#' }
#' REFLORA and JABOT are read from their herbaria's Darwin Core archives: the first
#' use downloads them - several gigabytes - to \code{"reflora_download"} and
#' \code{"jabot_download"} in the working directory, where they are reused by later
#' calls. Reading them takes a few minutes, so they are read only once per call, for
#' all the taxa in \code{taxon} together: checking many taxa in one call is much
#' faster than one call per taxon. Only their specimens collected in Brazil are
#' counted. Use \code{sources = "gbif"} for a quick check.
#' The place fields of all sources are standardised with \code{barRoso::std_place()},
#' so free-text state values (e.g. \code{"Ba"}, \code{"Est. do Bahia"},
#' \code{"Paraiba"}) match FFB state names before comparison; values that are not
#' Brazilian states are ignored.
#'
#' \strong{Phytogeographic domains.} With \code{domains = TRUE} (default), the GBIF
#' records that have coordinates - excluding those GBIF flags with geospatial issues,
#' such as zero coordinates or coordinates outside the stated country - are placed in
#' the six Brazilian phytogeographic domains (Amazonia, Caatinga, Cerrado, Mata
#' Atlantica, Pampa, Pantanal) using IBGE's biome map (2019) bundled with floraR,
#' simplified to a 1 km tolerance. A domain with records but not listed for the taxon
#' in FFB is a candidate new domain record. Up to \code{max_coordinates} GBIF records
#' are read (300 per request); the REFLORA and JABOT specimens with coordinates are
#' added when those sources are queried. Records that fall outside the six domains
#' (e.g. just offshore) are kept in \code{records}, with \code{Domain = NA}, but
#' are never flagged nor drawn on the report's map. This
#' analysis requires the \pkg{sf} package, and the map in the HTML report the
#' \pkg{leaflet} package; without them, they are skipped with a message.
#'
#' Occurrence records are evidence to review, not proof: a record in a new state may be
#' a cultivated plant, a misidentification, or a georeferencing error, which is why the
#' HTML report lists the individual records behind each candidate state.
#'
#' \strong{Several taxa.} Each taxon in \code{taxon} is analysed separately, and its
#' spreadsheet and HTML report are saved under its own name,
#' \code{flora_distribution_gap_<taxon>} (e.g.
#' \code{flora_distribution_gap_Luetzelburgia_auriculata.xlsx}). A taxon that fails
#' (e.g. a misspelt name) is reported and skipped, without stopping the others.
#'
#' \strong{Taxa with nothing new.} A taxon without any candidate new state or domain
#' record needs no FFB update, so no spreadsheet or detailed report is written for it.
#' Instead, all such taxa of a call are listed in a single, simple HTML report,
#' \code{<dir>/__no_new_records_report.html} (written when \code{html_report = TRUE}),
#' with what was searched for each - its FFB states and domains, the number of records
#' per source - and a conclusion: \code{"Within FFB distribution - no update needed"},
#' or \code{"No records found - check the name"} when the databases returned nothing
#' at all (which says nothing about the taxon's distribution). Their rows are still
#' returned.
#'
#' \strong{Validating the spreadsheet.} The spreadsheet is meant to be reviewed by a
#' specialist of the group. Its first sheet, \code{instructions}, explains what to do:
#' the goal is to find at least \emph{one} voucher specimen supporting each candidate
#' new state or domain, not to check every record. The \code{records} sheet ends with
#' two columns for the specialist: \code{validation}, filled in from a drop-down list
#' with \code{"correct"} (a voucher for the new record), \code{"incorrect"}
#' (misidentified, or wrong locality), or \code{"uncertain"} (checked, but impossible
#' to decide), and left empty for records not reviewed; and \code{comments}, optional
#' free text. In the \code{states} and \code{domains} sheets,
#' \code{records_reviewed} and \code{vouchers_validated} count, for each candidate,
#' the records reviewed and those validated as correct, and the \code{instructions}
#' sheet sums up the progress; Excel updates them as the records are reviewed. Every
#' sheet has filters on its header, and the header row and the taxon column stay
#' frozen while scrolling.
#'
#' @usage
#' flora_distribution_gap(
#'   taxon,
#'   state = NULL,
#'   sources = c("gbif", "reflora", "jabot"),
#'   keep_downloads = TRUE,
#'   specieslink_key = Sys.getenv("SPECIESLINK_KEY"),
#'   domains = TRUE,
#'   max_coordinates = 5000,
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "flora_distribution_gap",
#'   html_report = TRUE,
#'   open_report = FALSE
#' )
#'
#' @param taxon Character vector. One or more genus or species names of plants or
#'   algae (e.g. \code{"Luetzelburgia auriculata"}, \code{"Luetzelburgia"}, or
#'   \code{c("Luetzelburgia auriculata", "Luetzelburgia bahiensis")}). Each is
#'   analysed separately; see Details.
#'
#' @param state Character vector. Optional subset of Brazilian states (full name or
#'   acronym) to restrict the check to. \code{NULL} (default) checks all states with any
#'   evidence.
#'
#' @param sources Character vector. Which occurrence sources to query: any of
#'   \code{"gbif"}, \code{"reflora"}, \code{"jabot"}, \code{"speciesLink"}. Defaults
#'   to \code{c("gbif", "reflora", "jabot")}, combining the evidence of all three
#'   collection databases; \code{"gbif"} alone is the fastest. See Details for the
#'   requirements of each.
#'
#' @param keep_downloads Logical. Applies to REFLORA and JABOT. If \code{TRUE}
#'   (default), the herbarium archives downloaded to \code{"reflora_download"} and
#'   \code{"jabot_download"} in the working directory are kept, so that later runs -
#'   for this or other taxa - reuse them instead of downloading them again. If
#'   \code{FALSE}, those folders are deleted once the analysis is finished, leaving the
#'   working directory clean (they take several gigabytes).
#'
#' @param specieslink_key Character. speciesLink API key, used when
#'   \code{"speciesLink"} is among \code{sources}. Defaults to the
#'   \code{SPECIESLINK_KEY} environment variable.
#'
#' @param domains Logical. If \code{TRUE} (default), also place the georeferenced
#'   records in the Brazilian phytogeographic domains and flag candidate new domain
#'   records (see Details). Requires the \pkg{sf} package.
#'
#' @param max_coordinates Numeric. Maximum number of georeferenced GBIF records read
#'   for the domain analysis. Defaults to \code{5000}.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result of each taxon with at
#'   least one candidate new state or domain record is saved to disk as an \code{.xlsx}
#'   spreadsheet named after the taxon, \code{flora_distribution_gap_<taxon>.xlsx},
#'   ready for a specialist to validate the candidates (see Details). Taxa with nothing
#'   new get no spreadsheet.
#'
#' @param dir Character. Directory where the spreadsheets and reports are saved.
#'   Defaults to \code{"flora_distribution_gap"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report for each taxon with candidates
#'   (\code{<dir>/flora_distribution_gap_<taxon>.html}), plus a single
#'   \code{<dir>/__no_new_records_report.html} listing the taxa with nothing new (see
#'   Details) summarizing the results: KPI counts, an
#'   evidence-by-source breakdown, the full state-by-state table, the domain table, an
#'   interactive map of the georeferenced records over the phytogeographic domains
#'   (coloured by candidate status), the records in candidate domains, and the individual
#'   occurrence records found in the new-state-record candidate states - each linking to
#'   the record in its source database - all as sortable, filterable \pkg{DT} widgets with
#'   buttons to copy or download them as CSV/Excel. Requires the \pkg{rmarkdown},
#'   \pkg{DT}, and \pkg{htmltools} packages; if any is missing, the report is skipped with
#'   a message (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE}, opens the HTML report in the
#'   browser once rendered (in interactive sessions only). Default \code{FALSE}:
#'   the report is only saved to \code{dir}.
#'
#' @return A list of three \code{data.frame}s, each with a \code{taxon} column; with
#'   several taxa, the rows of all of them are stacked. Fields taken from the source
#'   databases keep their original Darwin Core names (e.g. \code{recordedBy},
#'   \code{decimalLatitude}); the columns created by floraR are in lower case:
#' \describe{
#'   \item{\code{states}}{One row per Brazilian state with any evidence for
#'     \code{taxon} (in FFB or in the queried sources): \code{taxon}, \code{state},
#'     \code{in_ffb_distribution} (whether FFB already lists the state),
#'     \code{gbif_records}, \code{reflora_records}, \code{jabot_records}, and
#'     \code{specieslink_records}
#'     (records in the state, for each source queried), and
#'     \code{new_state_record_candidate} (\code{TRUE} when the state has evidence from
#'     at least one source but is not in FFB's distribution).}
#'   \item{\code{domains}}{One row per phytogeographic domain with evidence (in FFB or
#'     among the georeferenced records): \code{taxon}, \code{domain},
#'     \code{in_ffb_distribution}, \code{georeferenced_records}, and
#'     \code{new_domain_record_candidate}. Empty when \code{domains = FALSE}.}
#'   \item{\code{records}}{The individual records: every georeferenced record, plus the
#'     records without coordinates from the candidate new states, with the candidates
#'     first. Columns: \code{taxon}, \code{source}, \code{decimalLatitude},
#'     \code{decimalLongitude}, \code{state}, \code{domain}, \code{municipality},
#'     \code{locality}, \code{recordedBy}, \code{recordNumber}, \code{eventDate} (the
#'     year, for REFLORA and JABOT), \code{institutionCode}, \code{catalogNumber},
#'     \code{scientificName} (the record's identification), \code{record_link}, and the
#'     logical columns \code{new_state_record_candidate} and
#'     \code{new_domain_record_candidate}.}
#' }
#' With \code{save = TRUE}, each table is saved as a sheet of the \code{.xlsx}
#' spreadsheet, after an \code{instructions} sheet (see Details).
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
#' # The candidate new phytogeographic domains, and the records behind them
#' gap$domains
#' gap$records[gap$records$new_domain_record_candidate, ]
#'
#' # Several species in one call: REFLORA and JABOT are read once, and each species
#' # gets its own spreadsheet and report
#' gap_many <- flora_distribution_gap(taxon = c("Luetzelburgia auriculata",
#'                                              "Luetzelburgia bahiensis",
#'                                              "Luetzelburgia praecox"))
#' gap_many$states[gap_many$states$new_state_record_candidate, ]
#'
#' # A quick check with GBIF only
#' gap_gbif <- flora_distribution_gap(taxon = "Luetzelburgia auriculata",
#'                                    sources = "gbif")
#' }
#'
#' @importFrom openxlsx createWorkbook
#'
#' @export

flora_distribution_gap <- function(taxon,
                                   state = NULL,
                                   sources = c("gbif", "reflora", "jabot"),
                                   keep_downloads = TRUE,
                                   specieslink_key = Sys.getenv("SPECIESLINK_KEY"),
                                   domains = TRUE,
                                   max_coordinates = 5000,
                                   version = "latest",
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "flora_distribution_gap",
                                   html_report = TRUE,
                                   open_report = FALSE) {

  taxon <- .check_taxa(taxon, "genus or species name")
  sources <- match.arg(sources, c("gbif", "reflora", "jabot", "speciesLink"),
                       several.ok = TRUE)
  if (!isTRUE(keep_downloads)) {
    on.exit(.rm_herbarium_downloads(sources, verbose), add = TRUE)
  }

  # FFB and the herbarium archives are read once, for all taxa
  ffb <- .flora_prepare_records(version = version, verbose = FALSE, rm_flora_database = FALSE)
  herb_all <- .herbarium_records_all(sources, taxon, verbose)

  outs <- .run_per_taxon(taxon, verbose, function(tx) {
    .distribution_gap_taxon(tx, state = state, sources = sources, ffb = ffb,
                            herb_all = herb_all, specieslink_key = specieslink_key,
                            domains = domains, max_coordinates = max_coordinates,
                            verbose = verbose, save = save, dir = dir,
                            html_report = html_report, open_report = open_report)
  })
  .no_gap_report(outs, what = "distribution", sources = sources, dir = dir,
                 html_report = html_report, verbose = verbose, open_report = open_report)
  if (length(outs) == 0) {
    return(.distribution_output(list(states = .rbind_fill(list()),
                                     domains = .empty_domain_table(),
                                     records = .empty_coord_records())))
  }
  list(states = .rbind_fill(lapply(outs, `[[`, "states")),
       domains = .rbind_fill(lapply(outs, `[[`, "domains")),
       records = .rbind_fill(lapply(outs, `[[`, "records")))
}


#_______________________________________________________________________________
# flora_distribution_gap() for one taxon, given the FFB tables ('ffb') and the
# standardised REFLORA/JABOT records of all queried taxa ('herb_all') ####
.distribution_gap_taxon <- function(taxon, state, sources, ffb, herb_all, specieslink_key,
                                    domains, max_coordinates, verbose, save, dir,
                                    html_report, open_report) {

  filename <- paste0("flora_distribution_gap_", gsub("\\s+", "_", taxon))

  # ------------------------------------------------------------------------
  # 1. FFB's officially listed states for this taxon
  # ------------------------------------------------------------------------
  if (verbose) message("Retrieving FFB's officially listed states for '", taxon, "'...")

  ffb_taxon <- .ffb_taxon_states(taxon, ffb$taxon_df, ffb$distribution_df)
  ffb_states <- ffb_taxon$states
  ffb_domains <- ffb_taxon$domains

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

  # Herbarium specimens (REFLORA, JABOT): records collected in Brazil, and
  # their counts per state
  herbaria <- intersect(c("reflora", "jabot"), sources)
  herb_records <- list()
  herb_counts <- list()
  for (src in herbaria) {
    column <- .herbarium_sources[[src]]$column
    herb_counts[[src]] <- stats::setNames(data.frame(character(0), integer(0),
                                                     stringsAsFactors = FALSE),
                                          c("State", column))
    recs <- .herbarium_subset(herb_all[[src]], taxon)
    if (is.null(recs)) next
    recs <- recs[.in_brazil(recs$Country, recs$State), , drop = FALSE]
    herb_records[[src]] <- recs
    st <- recs$State[!is.na(recs$State)]
    if (length(st) > 0) {
      tab <- table(st)
      herb_counts[[src]] <- stats::setNames(data.frame(names(tab), as.integer(tab),
                                                       stringsAsFactors = FALSE),
                                            c("State", column))
    }
    if (verbose) {
      message("  ", .herbarium_sources[[src]]$label, ": ", nrow(recs),
              " Brazilian specimen(s) in ", nrow(herb_counts[[src]]), " state(s).")
    }
  }
  herb_states <- unlist(lapply(herb_counts, `[[`, "State"), use.names = FALSE)

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
  all_states <- unique(c(ffb_states, gbif_df$State, herb_states, splink_df$State))

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
    out <- list(states = empty, domains = .empty_domain_table(), records = .empty_coord_records())
    attr(out, "no_gap") <- .distribution_no_gap_row(taxon, ffb_taxon, sources, gbif_df,
                                                    herb_counts, splink_df, 0L)
    return(.distribution_output(out))
  }

  result <- data.frame(Taxon = taxon, State = sort(all_states), stringsAsFactors = FALSE)
  result$In_FFB_distribution <- result$State %in% ffb_states
  fill <- function(df, col) {
    v <- df[[col]][match(result$State, df$State)]
    v[is.na(v)] <- 0L
    v
  }
  if ("gbif" %in% sources) result$GBIF_records <- fill(gbif_df, "GBIF_records")
  for (src in herbaria) {
    column <- .herbarium_sources[[src]]$column
    result[[column]] <- fill(herb_counts[[src]], column)
  }
  if ("speciesLink" %in% sources) {
    result$speciesLink_records <- fill(splink_df, "speciesLink_records")
  }

  external <- unique(c(gbif_df$State, herb_states, splink_df$State))
  result$New_state_record_candidate <- !result$In_FFB_distribution & result$State %in% external
  rownames(result) <- NULL

  n_new <- sum(result$New_state_record_candidate)

  # ------------------------------------------------------------------------
  # 4. Phytogeographic domains, from georeferenced records placed on IBGE's
  #    biome map
  # ------------------------------------------------------------------------
  domain_table <- .empty_domain_table()
  coord_records <- .empty_coord_records()
  if (domains) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      if (verbose) {
        message("  Package 'sf' is not installed; skipping the phytogeographic domain ",
                "analysis. Install it with install.packages('sf') to include it.")
      }
    } else {
      coord_cols <- c("Source", "Latitude", "Longitude", "State", "Municipality",
                      "Locality", "RecordedBy", "RecordNumber", "EventDate", "Institution",
                      "CatalogNumber", "Identified_as", "RecordLink")
      coords <- .empty_coord_records()[, coord_cols]
      if (!is.null(gbif_key)) {
        if (verbose) message("Placing georeferenced GBIF records in phytogeographic domains...")
        g <- .gbif_brazil_coordinates(gbif_key$key, max_records = max_coordinates)
        if (nrow(g) > 0) {
          g$Source <- "GBIF"
          # Queried for Brazil only: the country is set for barRoso::std_place()
          place <- suppressMessages(barRoso::std_place(data.frame(
            country = "Brazil", stateProvince = g$StateProvince,
            municipality = g$Municipality, locality = g$Locality, stringsAsFactors = FALSE)))
          g$State <- .normalize_br_state(place$stateProvince)
          g$Municipality <- place$municipality
          g$Locality <- place$locality
          coords <- rbind(coords, g[, coord_cols])
        }
      }
      for (src in names(herb_records)) {
        h <- herb_records[[src]]
        h <- h[!is.na(h$Latitude) & !is.na(h$Longitude), coord_cols, drop = FALSE]
        coords <- rbind(coords, h)
      }

      if (nrow(coords) > 0) {
        coords$Domain <- .assign_br_domain(coords$Longitude, coords$Latitude)
        if (!is.null(state)) coords <- coords[coords$State %in% all_states, ]
        coords$New_state_record_candidate <- !is.na(coords$State) & !coords$State %in% ffb_states
        coords$New_domain_record_candidate <- !is.na(coords$Domain) & !coords$Domain %in% ffb_domains
        coord_records <- cbind(data.frame(Taxon = rep(taxon, nrow(coords)), stringsAsFactors = FALSE),
                               coords[, setdiff(names(.empty_coord_records()), "Taxon")])
        rownames(coord_records) <- NULL
      }

      in_records <- coord_records$Domain[!is.na(coord_records$Domain)]
      all_domains <- .br_domain_names[.br_domain_names %in% c(ffb_domains, in_records)]
      if (length(all_domains) > 0) {
        n_rec <- table(factor(in_records, levels = .br_domain_names))
        domain_table <- data.frame(
          Taxon = taxon, Domain = all_domains,
          In_FFB_distribution = all_domains %in% ffb_domains,
          Georeferenced_records = as.integer(n_rec[all_domains]),
          stringsAsFactors = FALSE)
        domain_table$New_domain_record_candidate <- !domain_table$In_FFB_distribution &
          domain_table$Georeferenced_records > 0
      }
      if (verbose) {
        message(sprintf("  %d georeferenced record(s); %d of them in a domain not listed by FFB.",
                        nrow(coord_records), sum(coord_records$New_domain_record_candidate)))
      }
    }
  }

  n_new_domains <- sum(domain_table$New_domain_record_candidate)
  if (verbose) {
    message(sprintf("\n\u2713 %d state(s) with evidence for '%s', %d not yet in FFB's distribution",
                    nrow(result), taxon, n_new))
    if (domains && nrow(domain_table) > 0) {
      message(sprintf("\u2713 %d phytogeographic domain(s) with evidence, %d not yet in FFB's distribution",
                      nrow(domain_table), n_new_domains))
    }
  }

  # ------------------------------------------------------------------------
  # 5. The individual records behind each new-state-record candidate, each
  #    linking to the record itself in its source database
  # ------------------------------------------------------------------------
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
        place <- suppressMessages(barRoso::std_place(data.frame(
          country = "Brazil", stateProvince = st, municipality = recs$Municipality,
          locality = recs$Locality, stringsAsFactors = FALSE)))
        recs$Municipality <- place$municipality
        recs$Locality <- place$locality
        records_detail <- rbind(records_detail,
                                cbind(data.frame(Source = "GBIF", State = st,
                                                 stringsAsFactors = FALSE), recs))
      }
    }
  }

  for (src in names(herb_records)) {
    h <- herb_records[[src]]
    h <- h[!is.na(h$State) & h$State %in% candidates, names(records_detail), drop = FALSE]
    records_detail <- rbind(records_detail, h)
  }

  # The records a specialist reviews: every georeferenced record (domains),
  # plus the records without coordinates from the new-state candidate states;
  # candidates first, grouped by state and domain
  records_all <- .review_records(coord_records, records_detail, taxon)

  out <- list(states = result, domains = domain_table, records = records_all)

  # Nothing new for FFB: no spreadsheet or report for this taxon, only a row in
  # the run's single no-gap report
  if (n_new == 0 && n_new_domains == 0) {
    if (verbose) message("  No new state or domain record: no files written for '", taxon, "'.")
    attr(out, "no_gap") <- .distribution_no_gap_row(taxon, ffb_taxon, sources, gbif_df,
                                                    herb_counts, splink_df,
                                                    nrow(coord_records))
    return(.distribution_output(out))
  }

  out <- .distribution_output(out)
  if (save) {
    dir <- .arg_check_dir(dir)
    .save_distribution_spreadsheet(out, taxon, filename, dir, verbose)
  }

  if (html_report) {
    herb_count <- function(src, what) {
      h <- herb_counts[[src]]
      if (is.null(h)) return(0L)
      if (what == "states") nrow(h) else sum(h[[.herbarium_sources[[src]]$column]])
    }
    source_summary <- data.frame(
      Source = c("FFB (official)", "GBIF", "REFLORA", "JABOT", "speciesLink"),
      States = c(length(ffb_states), nrow(gbif_df), herb_count("reflora", "states"),
                 herb_count("jabot", "states"), nrow(splink_df)),
      Records = c(NA_integer_, sum(gbif_df$GBIF_records), herb_count("reflora", "records"),
                  herb_count("jabot", "records"), sum(splink_df$speciesLink_records)),
      stringsAsFactors = FALSE
    )
    source_summary <- source_summary[c(TRUE, c("gbif", "reflora", "jabot", "speciesLink") %in%
                                         sources), ]

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
      records_detail = records_detail,
      domains_run = domains && requireNamespace("sf", quietly = TRUE),
      ffb_domains = ffb_domains,
      domain_table = domain_table,
      n_new_domains = n_new_domains,
      coord_records = coord_records,
      max_coordinates = max_coordinates
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

  out
}


# One row of the no-gap report: what was searched for a taxon without any
# candidate new state or domain record ####
.distribution_no_gap_row <- function(taxon, ffb_taxon, sources, gbif_df, herb_counts,
                                     splink_df, n_georef) {
  n_herb <- function(src) {
    h <- herb_counts[[src]]
    if (is.null(h)) NA_integer_ else as.integer(sum(h[[.herbarium_sources[[src]]$column]]))
  }
  row <- data.frame(Taxon = taxon,
                    FFB_name = if (ffb_taxon$found) ffb_taxon$name else NA_character_,
                    FFB_states = length(ffb_taxon$states),
                    FFB_domains = paste(ffb_taxon$domains, collapse = ", "),
                    stringsAsFactors = FALSE)
  if ("gbif" %in% sources) row$GBIF_records <- as.integer(sum(gbif_df$GBIF_records))
  if ("reflora" %in% sources) row$Reflora_records <- n_herb("reflora")
  if ("jabot" %in% sources) row$Jabot_records <- n_herb("jabot")
  if ("speciesLink" %in% sources) {
    row$speciesLink_records <- as.integer(sum(splink_df$speciesLink_records))
  }
  row$Georeferenced_records <- as.integer(n_georef)
  n_rec <- sum(unlist(row[intersect(c("GBIF_records", "Reflora_records", "Jabot_records",
                                      "speciesLink_records"), names(row))]), na.rm = TRUE)
  row$Conclusion <- if (n_rec == 0) {
    "No records found - check the name"
  } else {
    "Within FFB distribution - no update needed"
  }
  row
}


# Empty result tables, with the columns flora_distribution_gap() returns
.empty_domain_table <- function() {
  data.frame(Taxon = character(0), Domain = character(0), In_FFB_distribution = logical(0),
             Georeferenced_records = integer(0), New_domain_record_candidate = logical(0),
             stringsAsFactors = FALSE)
}

.empty_coord_records <- function() {
  data.frame(Taxon = character(0), Source = character(0), Latitude = numeric(0),
             Longitude = numeric(0), State = character(0), Domain = character(0),
             Municipality = character(0), Locality = character(0), RecordedBy = character(0),
             RecordNumber = character(0), EventDate = character(0), Institution = character(0),
             CatalogNumber = character(0), Identified_as = character(0),
             RecordLink = character(0), New_state_record_candidate = logical(0),
             New_domain_record_candidate = logical(0), stringsAsFactors = FALSE)
}


#_______________________________________________________________________________
# The records table of flora_distribution_gap(): the georeferenced records
# ('coords', which carry the domain), plus the records without coordinates
# found in the new-state candidate states ('detail'), not repeated. Sorted
# with the candidates first, grouped by state and domain, so a specialist can
# review them one candidate state or domain at a time ####
.review_records <- function(coords, detail, taxon) {
  if (nrow(detail) > 0) {
    key <- function(d) ifelse(!is.na(d$RecordLink), d$RecordLink,
                              paste(d$Source, d$Institution, d$CatalogNumber))
    detail <- detail[!key(detail) %in% key(coords) & !duplicated(key(detail)), , drop = FALSE]
  }
  if (nrow(detail) > 0) {
    extra <- .empty_coord_records()[rep(NA_integer_, nrow(detail)), ]
    extra$Taxon <- taxon
    for (nm in intersect(names(detail), names(extra))) extra[[nm]] <- detail[[nm]]
    extra$New_state_record_candidate <- TRUE
    extra$New_domain_record_candidate <- FALSE
    coords <- rbind(coords, extra)
  }
  if (nrow(coords) == 0) return(coords)
  coords <- coords[order(!coords$New_state_record_candidate, !coords$New_domain_record_candidate,
                         coords$State, coords$Domain, coords$Source, na.last = TRUE), , drop = FALSE]
  rownames(coords) <- NULL
  coords
}


#_______________________________________________________________________________
# Column names of what flora_distribution_gap() returns and saves: fields
# taken from the source databases keep their original Darwin Core names
# (e.g. recordedBy, decimalLatitude); columns created by floraR are in
# lower case (e.g. taxon, new_state_record_candidate) ####
.distribution_record_names <- c(
  Taxon = "taxon", Source = "source", Latitude = "decimalLatitude",
  Longitude = "decimalLongitude", State = "state", Domain = "domain",
  Municipality = "municipality", Locality = "locality", RecordedBy = "recordedBy",
  RecordNumber = "recordNumber", EventDate = "eventDate", Institution = "institutionCode",
  CatalogNumber = "catalogNumber", Identified_as = "scientificName", RecordLink = "record_link",
  New_state_record_candidate = "new_state_record_candidate",
  New_domain_record_candidate = "new_domain_record_candidate")

.distribution_output <- function(out) {
  names(out$states) <- tolower(names(out$states))
  names(out$domains) <- tolower(names(out$domains))
  names(out$records) <- unname(.distribution_record_names[names(out$records)])
  out
}


#_______________________________________________________________________________
# Save one taxon's flora_distribution_gap() spreadsheet: an instructions sheet
# for the specialist, then the states, domains and records sheets. The records
# sheet gets the 'validation' and 'comments' columns; the states and domains
# sheets count, for each candidate, the records reviewed and the vouchers
# validated as correct, updated live by Excel as the records are reviewed ####
.save_distribution_spreadsheet <- function(out, taxon, filename, dir, verbose) {
  rec_cols <- c(names(out$records), "validation", "comments")
  n_rec <- nrow(out$records)
  rng <- function(col) .xl_range("records", rec_cols, col, n_rec)

  counters <- function(df, unit, cand) {
    n <- nrow(df)
    if (n == 0) return(list())
    rows <- seq_len(n) + 1
    unit_cell <- paste0(openxlsx::int2col(match(unit, names(df))), rows)
    cand_cell <- paste0(openxlsx::int2col(match(cand, names(df))), rows)
    count <- function(crit) sprintf('IF(%s=TRUE,COUNTIFS(%s,%s,%s,TRUE,%s,"%s"),"")', cand_cell,
                                    rng(unit), unit_cell, rng(cand), rng("validation"), crit)
    list(records_reviewed = count("<>"), vouchers_validated = count("correct"))
  }
  formula_cols <- list(states = counters(out$states, "state", "new_state_record_candidate"),
                       domains = counters(out$domains, "domain", "new_domain_record_candidate"))

  sheet_range <- function(sheet, df, extra, col) {
    .xl_range(sheet, c(names(df), extra), col, nrow(df))
  }
  st_rng <- function(col) sheet_range("states", out$states, names(formula_cols$states), col)
  dm_rng <- function(col) sheet_range("domains", out$domains, names(formula_cols$domains), col)
  progress <- data.frame(
    what = c("Records reviewed (validation filled in)", "  correct", "  incorrect", "  uncertain",
             "Candidate new states", "  with at least one voucher validated",
             "Candidate new domains", "  with at least one voucher validated"),
    value = c(sprintf("COUNTA(%s)", rng("validation")),
              sprintf('COUNTIF(%s,"%s")', rng("validation"), .validation_terms),
              sprintf("COUNTIF(%s,TRUE)", st_rng("new_state_record_candidate")),
              if (length(formula_cols$states)) sprintf('COUNTIF(%s,">0")', st_rng("vouchers_validated")) else "0",
              sprintf("COUNTIF(%s,TRUE)", dm_rng("new_domain_record_candidate")),
              if (length(formula_cols$domains)) sprintf('COUNTIF(%s,">0")', dm_rng("vouchers_validated")) else "0"),
    stringsAsFactors = FALSE)

  .write_spreadsheet(sheets = list(states = out$states, domains = out$domains,
                                   records = out$records),
                     filename = filename, dir = dir, verbose = verbose,
                     help = .distribution_help(taxon, progress),
                     validate_sheet = "records", formula_cols = formula_cols,
                     link_cols = "record_link")
}


#_______________________________________________________________________________
# The instructions sheet of a flora_distribution_gap() spreadsheet ####
.distribution_help <- function(taxon, progress) {
  txt <- function(x, gap = FALSE) list(kind = "text", x = x, gap = gap)
  list(
    list(kind = "title", x = paste0("How to validate the new distribution records of ", taxon)),
    txt(paste0("Spreadsheet written by floraR::flora_distribution_gap() on ", Sys.Date(),
               ", with the Flora e Funga do Brasil (FFB) distribution and the records of the sources queried.")),
    txt("GOAL: find at least ONE voucher specimen supporting each potential new record - a state or a phytogeographic"),
    txt("domain where the taxon has records but that FFB does not list yet. You do NOT need to check every record.", gap = TRUE),

    list(kind = "heading", x = "The sheets"),
    txt("states   - one row per state with evidence; new_state_record_candidate = TRUE marks a potential new state record."),
    txt("domains  - the same for the phytogeographic domains (new_domain_record_candidate = TRUE)."),
    txt("records  - the individual specimens/occurrences, candidates first. This is the ONLY sheet you fill in (columns validation and comments)."),
    txt("In states and domains, records_reviewed and vouchers_validated count your work in the records sheet and update by themselves.", gap = TRUE),

    list(kind = "heading", x = "What to do"),
    txt("1. In states, pick a row with new_state_record_candidate = TRUE."),
    txt("2. In records, filter the column state to that state. Open record_link to see the specimen (image, label, identification)."),
    txt("3. In validation, choose correct, incorrect or uncertain from the drop-down list (see the terms below)."),
    txt("4. As soon as one record is correct, the state is supported: move on to the next candidate state."),
    txt("   If the record is incorrect or uncertain, check the next record of the same state."),
    txt("5. Do the same for the candidate domains: filter records by domain and new_domain_record_candidate = TRUE."),
    txt("   A record validated as correct counts for both its state and its domain."),
    txt("6. Leave validation EMPTY for the records you did not look at: every filled cell counts as a reviewed record."),
    txt("Please do not rename the sheets or the columns - the counters depend on them.", gap = TRUE),

    list(kind = "heading", x = "Terms for the column validation (the same three words for every record)"),
    list(kind = "terms", x = data.frame(
      validation = c(.validation_terms),
      meaning = c("You checked the specimen: the identification is right and the specimen was collected in that state/domain. It is a voucher for the new record.",
                  "You checked the specimen: it is misidentified, or its locality or coordinates are wrong. It does not support the record.",
                  "You checked the specimen but cannot decide (e.g. sterile material, poor image, unreadable label, difficult group)."),
      stringsAsFactors = FALSE)),
    txt("An empty cell means: not reviewed."),
    txt("The column comments is OPTIONAL and free text - fill it in only if you have something to add.", gap = TRUE),

    list(kind = "heading", x = "Example (fictitious records)"),
    list(kind = "table", x = data.frame(
      state = c("Piauí", "Piauí", "Piauí", "Piauí", "Pará", "Pará"),
      catalogNumber = c("XX0000001", "XX0000002", "XX0000003", "XX0000004", "XX0000005", "XX0000006"),
      scientificName = rep("Genus exemplaris", 6),
      validation = c("incorrect", "uncertain", "correct", "", "uncertain", ""),
      comments = c("Misidentified: it is Genus alterus", "Sterile; image too poor to confirm", "",
                   "(not reviewed: Piauí already has a voucher)",
                   "Possibly an undescribed species - needs further study", "(not reviewed yet)"),
      stringsAsFactors = FALSE)),

    list(kind = "heading", x = "Progress (updates by itself)"),
    list(kind = "formulas", x = progress)
  )
}
