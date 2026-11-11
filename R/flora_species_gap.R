#' Find species that occur in Brazil but are missing from FFB
#'
#' @description
#' Given one or more genera of plants or algae, finds the species-level names that global sources
#' record for Brazil but that are not currently registered in the Flora e Funga do
#' Brasil (FFB) - candidates for inclusion in the FFB monograph of the group. Two
#' independent kinds of evidence, from up to four sources, are combined (see
#' \code{sources}):
#' \itemize{
#'   \item \strong{POWO distributions}: species that
#'     \href{https://powo.science.kew.org}{Plants of the World Online} (POWO, Royal
#'     Botanic Gardens, Kew) maps in Brazil;
#'   \item \strong{Type specimens}: species whose type specimens - holotypes,
#'     isotypes, lectotypes, syntypes, paratypes, and so on - were collected in Brazil,
#'     according to the herbaria that publish them through the
#'     \href{https://www.gbif.org}{Global Biodiversity Information Facility} (GBIF),
#'     the \href{https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/}{Reflora
#'     Virtual Herbarium}, and \href{https://jabot.jbrj.gov.br}{JABOT}.
#' }
#' The result is a single table, one row per name, stating which source(s) reported it:
#' the more sources report a name, the stronger the candidate, and combining them
#' retrieves as many gaps as possible. This is not a general diff of
#' the databases: a name missing from FFB is only returned when there is evidence that
#' the species occurs in Brazil.
#'
#' @details
#' \strong{FFB side.} Names are compared against the locally cached FFB dataset (as in
#' \code{\link{flora_download}} and \code{\link{flora_parse}}). A name counts as
#' "already in FFB" if either the name itself or its \emph{accepted} name (in POWO or
#' GBIF) matches any FFB name - accepted or synonym, anywhere in the checklist, since
#' checklists disagree on generic placement (e.g. a \emph{Myrcia} synonym accepted as
#' \emph{Eugenia florida}). Names are compared ignoring case, diacritics, hyphens,
#' \emph{y}/\emph{i}, and the genitive \emph{-ii}/\emph{-i} ending, so orthographic variants such as
#' \emph{Luetzelburgia andrade-limae} and FFB's \emph{L. andradelimae} are not
#' reported. Only species-level names are reported.
#'
#' \strong{POWO} (\code{"powo"}). POWO's names, synonymy, and distribution maps come
#' from the \href{https://powo.science.kew.org/about-wcvp}{World Checklist of Vascular
#' Plants} (WCVP), read from one of two sources chosen with \code{powo_source}:
#' \describe{
#'   \item{\code{"rWCVPdata"} (default)}{The whole WCVP, installed locally by the
#'     \href{https://github.com/matildabrown/rWCVPdata}{rWCVPdata} package: every name
#'     and distribution of the genus is resolved at once, in memory. The data is as
#'     current as the installed release (see \code{rWCVPdata::wcvp_check_version()}).
#'     \pkg{rWCVPdata} is not on CRAN; install it with
#'     \code{install.packages("rWCVPdata", repos = c("https://matildabrown.github.io/drat",
#'     "https://cloud.r-project.org"))}.}
#'   \item{\code{"checklistbank"}}{POWO's own weekly export and WCVP, as hosted on
#'     \href{https://www.checklistbank.org}{ChecklistBank} (POWO's own API blocks
#'     programmatic clients). Always current and needs no extra package, but slower:
#'     one request per FFB-missing name.}
#' }
#' A POWO name has Brazil evidence when its accepted name occurs in at least one of the
#' five TDWG level-3 areas covering Brazil - Brazil North (\code{BZN}), Northeast
#' (\code{BZE}), West-Central (\code{BZC}), Southeast (\code{BZL}), and South
#' (\code{BZS}); native, introduced, or extinct, but not doubtful. Without a mapped
#' distribution, POWO's free-text range (e.g. \code{"Brazil (Bahia)"}) is checked
#' instead. POWO covers vascular plants only, so it finds nothing for algae.
#'
#' \strong{GBIF} (\code{"gbif"}). The genus is looked up among plants and algae only -
#' the GBIF kingdoms Plantae (land plants, red and green algae), Chromista (brown algae,
#' diatoms), and Protozoa (euglenids) - so a genus name shared with an animal (e.g.
#' \emph{Gracilaria}, both a red alga and a moth) resolves to the plant or alga. A
#' single faceted search lists every name in the genus with type specimens collected in
#' Brazil; for the FFB-missing names only, the specimens themselves are then retrieved,
#' many names per request, and summarised. Types are filed in GBIF under their current
#' identification - usually the typified name - which \code{GBIF_Specimen_names} shows.
#'
#' \strong{REFLORA} (\code{"reflora"}) and \strong{JABOT} (\code{"jabot"}). The
#' specimens of the genus are read with \code{\link[refloraR]{reflora_records}} and
#' \code{\link[jabotR]{jabot_records}} (requiring the \pkg{refloraR} and \pkg{jabotR}
#' packages), from their herbaria's Darwin Core archives: the first use downloads them -
#' several gigabytes - to \code{"reflora_download"} and \code{"jabot_download"} in the
#' working directory, where they are reused by later calls. Reading them takes a few
#' minutes, so they are read only once per call, for all the genera in \code{taxon}
#' together: checking many genera in one call is much faster than one call per
#' genus. The species-level names of the specimens collected in
#' Brazil that are types (any type status) are compared with FFB; as in GBIF, a
#' specimen is filed under its current identification. Use \code{sources = c("powo",
#' "gbif")} for a quick check.
#'
#' In all three specimen sources, type statuses are standardised with
#' \code{barRoso::std_types()} (e.g. \code{"HOLOTYPUS"} and \code{"Holotype"} both
#' become \code{"holotype"}) and places with \code{barRoso::std_place()}.
#'
#' \strong{Several genera.} Each genus in \code{taxon} is analysed separately, and its
#' spreadsheet and HTML report are saved under its own name,
#' \code{flora_species_gap_<genus>} (e.g. \code{flora_species_gap_Myrcia.xlsx}). A
#' genus that fails (e.g. a misspelt name) is reported and skipped, without stopping
#' the others.
#'
#' @usage
#' flora_species_gap(
#'   taxon,
#'   sources = c("powo", "gbif", "reflora", "jabot"),
#'   keep_downloads = TRUE,
#'   powo_source = c("rWCVPdata", "checklistbank"),
#'   require_brazil_evidence = TRUE,
#'   type_status = NULL,
#'   fetch_details = TRUE,
#'   max_number = 5000,
#'   version = "latest",
#'   verbose = TRUE,
#'   save = TRUE,
#'   dir = "flora_species_gap",
#'   html_report = TRUE,
#'   open_report = FALSE
#' )
#'
#' @param taxon Character vector. One or more genus names of plants or algae (e.g.
#'   \code{"Myrcia"}, \code{"Gracilaria"}, or \code{c("Myrcia", "Eugenia")}). Each
#'   is analysed separately; see Details. Fungi are not searched.
#'
#' @param sources Character vector. Which sources of evidence to use: any of
#'   \code{"powo"} (POWO distributions), \code{"gbif"}, \code{"reflora"}, and
#'   \code{"jabot"} (Brazilian type specimens in GBIF, REFLORA, and JABOT). Defaults to
#'   all four, combining their evidence; see Details for the requirements of each.
#'
#' @param keep_downloads Logical. Applies to REFLORA and JABOT. If \code{TRUE}
#'   (default), the herbarium archives downloaded to \code{"reflora_download"} and
#'   \code{"jabot_download"} in the working directory are kept, so that later runs -
#'   for this or other taxa - reuse them instead of downloading them again. If
#'   \code{FALSE}, those folders are deleted once the analysis is finished, leaving the
#'   working directory clean (they take several gigabytes).
#'
#' @param powo_source Character. Where POWO's data is read from: \code{"rWCVPdata"}
#'   (default; fast, local) or \code{"checklistbank"} (always current, slower). See
#'   Details.
#'
#' @param require_brazil_evidence Logical. Applies to POWO. If \code{TRUE} (default),
#'   POWO names are only kept when POWO places the species in Brazil. Set to
#'   \code{FALSE} to also return POWO names missing from FFB regardless of distribution,
#'   for manual review. (Names from the specimen sources always have Brazil evidence:
#'   their types were collected there.)
#'
#' @param type_status Character or \code{NULL}. Applies to GBIF. Type statuses counted
#'   as evidence (GBIF's current vocabulary, e.g. \code{"Holotype"}, \code{"Isotype"}).
#'   \code{NULL} (default) uses all primary and secondary type categories: Type,
#'   Holotype, Isotype, Lectotype, Isolectotype, Syntype, Isosyntype, Neotype,
#'   Isoneotype, Epitype, Isoepitype, Paratype, Isoparatype, Paralectotype,
#'   OriginalMaterial, and TypeSeries.
#'
#' @param fetch_details Logical. Applies to POWO with \code{powo_source =
#'   "checklistbank"}: if \code{TRUE} (default), each candidate's protologue citation is
#'   fetched (one request per name). With \code{"rWCVPdata"} it is always included.
#'
#' @param max_number Numeric. Applies to POWO with \code{powo_source =
#'   "checklistbank"}: maximum number of names retrieved. Defaults to \code{5000}.
#'
#' @param version Character. FFB dataset version to compare against. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#'
#' @param save Logical. If \code{TRUE} (default), the result of each genus is saved to
#'   disk as an \code{.xlsx} spreadsheet named after the genus,
#'   \code{flora_species_gap_<genus>.xlsx}.
#'
#' @param dir Character. Directory where the spreadsheets and reports are saved.
#'   Defaults to \code{"flora_species_gap"}.
#'
#' @param html_report Logical. If \code{TRUE} (default), also writes a self-contained
#'   HTML report for each genus (\code{<dir>/flora_species_gap_<genus>.html}) summarizing the results: KPI counts and
#'   the full candidate table as a sortable, filterable \pkg{DT} widget with buttons to
#'   copy or download it as CSV/Excel. Requires the \pkg{rmarkdown}, \pkg{DT}, and
#'   \pkg{htmltools} packages; if any is missing, the report is skipped with a message
#'   (the \code{.xlsx} spreadsheet is unaffected).
#'
#' @param open_report Logical. If \code{TRUE}, opens the HTML report in the
#'   browser once rendered (in interactive sessions only). Default \code{FALSE}:
#'   the report is only saved to \code{dir}.
#'
#' @return A \code{data.frame}, one row per species-level name in the genus (or
#'   genera, stacked one after the other) that is absent from the current FFB checklist and reported by at least one source, sorted
#'   with the names reported by more sources first. Common columns:
#' \describe{
#'   \item{Taxon_name, Authors}{The name and its author citation.}
#'   \item{Accepted_name}{The accepted name (POWO's when available, else GBIF's;
#'     for names only in REFLORA or JABOT, the name itself).}
#'   \item{Found_in}{Which source(s) reported the name, e.g. \code{"POWO; GBIF; REFLORA"},
#'     \code{"GBIF; JABOT"}, or \code{"POWO"}.}
#'   \item{Brazil_evidence}{Logical; \code{TRUE} when POWO places the species in Brazil
#'     or it has Brazilian type specimens in GBIF, REFLORA, or JABOT.}
#'   \item{In_FFB}{Always \code{FALSE} (kept for clarity when combining with other tables).}
#' }
#' POWO columns (when \code{"powo"} is among \code{sources}; \code{NA} for names POWO
#' did not report):
#' \describe{
#'   \item{POWO_ID, POWO_Taxonomic_status, POWO_Accepted_name}{The name's POWO (IPNI)
#'     identifier, status (e.g. \code{"Accepted"}, \code{"Synonym"}), and accepted name.}
#'   \item{POWO_Year, POWO_Published_in}{Year of publication and protologue citation.}
#'   \item{POWO_Range}{POWO's free-text range.}
#'   \item{POWO_Distribution, POWO_Brazil_Areas}{Every TDWG level-3 area of the accepted
#'     name's distribution, and only the Brazilian ones.}
#'   \item{POWO_Brazil_Evidence, POWO_Evidence_Source}{Whether POWO places the species in
#'     Brazil, and whether from the \code{"WCVP distribution"} or the \code{"POWO range
#'     text"}.}
#'   \item{POWO_URL, IPNI_URL}{Links to the name's POWO and IPNI pages.}
#' }
#' GBIF columns (when \code{"gbif"} is among \code{sources}; \code{NA} for names GBIF
#' did not report):
#' \describe{
#'   \item{GBIF_Key, GBIF_Taxonomic_status, GBIF_Accepted_name}{The name's GBIF backbone
#'     key, status (e.g. \code{"ACCEPTED"}, \code{"DOUBTFUL"}), and accepted species.}
#'   \item{GBIF_N_type_specimens, GBIF_Type_statuses}{Number of type specimens collected
#'     in Brazil, and their type categories.}
#'   \item{GBIF_Type_specimens}{Up to ten specimens as herbarium, catalogue number, and
#'     type status (e.g. \code{"RB 00123 (holotype)"}).}
#'   \item{GBIF_Specimen_names}{The names the types are filed under - usually the
#'     typified name, e.g. the basionym of a species.}
#'   \item{GBIF_Collectors, GBIF_Years, GBIF_States}{Collectors, collection years, and
#'     Brazilian states of those specimens.}
#'   \item{GBIF_Published_in, GBIF_URL}{Protologue citation, when GBIF has it, and a link
#'     to the name's GBIF page.}
#' }
#' REFLORA and JABOT columns (when \code{"reflora"} or \code{"jabot"} is among
#' \code{sources}), prefixed \code{REFLORA_} or \code{JABOT_}:
#' \describe{
#'   \item{N_type_specimens, Type_statuses, Type_specimens}{Number of type specimens
#'     collected in Brazil, their type categories, and up to ten of them as herbarium,
#'     catalogue number, and type status.}
#'   \item{Collectors, Years, States}{Collectors, collection years, and Brazilian states
#'     of those specimens.}
#'   \item{Links}{Up to three links to the specimens' pages (REFLORA only; JABOT records
#'     carry no link).}
#' }
#'
#' @seealso \code{\link{flora_distribution_gap}}, \code{\link{flora_download}}
#'
#' @author
#' Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' # Species of Myrcia recorded in Brazil by POWO and/or with Brazilian type
#' # specimens in GBIF, REFLORA, or JABOT, but missing from FFB
#' gap <- flora_species_gap(taxon = "Myrcia")
#'
#' # A quick check, without the REFLORA and JABOT archives
#' gap_quick <- flora_species_gap(taxon = "Myrcia", sources = c("powo", "gbif"))
#'
#' # Several genera in one call: REFLORA and JABOT are read once, and each genus
#' # gets its own spreadsheet and report
#' gap_many <- flora_species_gap(taxon = c("Myrcia", "Eugenia", "Calyptranthes"))
#'
#' # POWO only, read live from ChecklistBank instead of the local WCVP copy
#' gap_powo <- flora_species_gap(taxon = "Myrcia", sources = "powo",
#'                               powo_source = "checklistbank")
#'
#' # An algal genus: GBIF type specimens only (POWO covers vascular plants)
#' gap_algae <- flora_species_gap(taxon = "Gracilaria", sources = "gbif")
#'
#' # Stricter type evidence: holotypes and isotypes only
#' gap_strict <- flora_species_gap(taxon = "Myrcia",
#'                                 type_status = c("Holotype", "Isotype"))
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom openxlsx write.xlsx
#'
#' @export

flora_species_gap <- function(taxon,
                              sources = c("powo", "gbif", "reflora", "jabot"),
                              keep_downloads = TRUE,
                              powo_source = c("rWCVPdata", "checklistbank"),
                              require_brazil_evidence = TRUE,
                              type_status = NULL,
                              fetch_details = TRUE,
                              max_number = 5000,
                              version = "latest",
                              verbose = TRUE,
                              save = TRUE,
                              dir = "flora_species_gap",
                              html_report = TRUE,
                              open_report = FALSE) {

  taxon <- .check_taxa(taxon, "genus name")
  sources <- match.arg(sources, c("powo", "gbif", "reflora", "jabot"), several.ok = TRUE)
  if (!isTRUE(keep_downloads)) {
    on.exit(.rm_herbarium_downloads(sources, verbose), add = TRUE)
  }
  powo_source <- match.arg(powo_source)
  if (is.null(type_status)) type_status <- .gbif_type_status

  # Fail fast on a missing rWCVPdata, before the (slower) FFB and herbarium
  # steps; WCVP, FFB, and the herbarium archives are then read once, for all
  # genera
  wcvp <- if ("powo" %in% sources && powo_source == "rWCVPdata") .wcvp_tables() else NULL

  if (verbose) message("Retrieving the current FFB checklist...")
  ffb <- tryCatch(.ffb_names(taxon, version = version), error = function(e) {
    if (verbose) message("  Could not read the FFB checklist: ", conditionMessage(e))
    list(genus = character(0), all = character(0))
  })
  herb_all <- .herbarium_records_all(sources, taxon, verbose)

  outs <- .run_per_taxon(taxon, verbose, function(tx) {
    .species_gap_taxon(tx, sources = sources, ffb = ffb, wcvp = wcvp, herb_all = herb_all,
                       powo_source = powo_source,
                       require_brazil_evidence = require_brazil_evidence,
                       type_status = type_status, fetch_details = fetch_details,
                       max_number = max_number, verbose = verbose, save = save, dir = dir,
                       html_report = html_report, open_report = open_report)
  })
  if (length(outs) == 0) return(.merge_species_candidates())
  .rbind_fill(outs)
}


#_______________________________________________________________________________
# flora_species_gap() for one genus, given the FFB names of all queried genera
# ('ffb'), the WCVP tables, and the standardised REFLORA/JABOT records of all
# queried genera ('herb_all') ####
.species_gap_taxon <- function(taxon, sources, ffb, wcvp, herb_all, powo_source,
                               require_brazil_evidence, type_status, fetch_details,
                               max_number, verbose, save, dir, html_report, open_report) {

  filename <- paste0("flora_species_gap_", gsub("\\s+", "_", taxon))

  # The genus in GBIF, among plants and algae (an error when GBIF is the only
  # source and does not know the genus)
  genus <- NULL
  if ("gbif" %in% sources) {
    genus <- .gbif_genus_key(taxon)
    if (is.null(genus)) {
      msg <- paste0("Genus '", taxon, "' was not found in GBIF among plants and algae ",
                    "(kingdoms ", paste(.gbif_kingdoms, collapse = ", "), "). ",
                    "flora_species_gap() covers plants and algae only, not fungi or ",
                    "animals; otherwise, check the spelling.")
      if (identical(sources, "gbif")) stop(msg, call. = FALSE)
      if (verbose) message(msg, " Skipping GBIF.")
    } else if (verbose) {
      message("GBIF genus: ", genus$name, " (", genus$kingdom, ")")
    }
  }

  # ------------------------------------------------------------------------
  # 1. FFB side: names already registered (genus species + all of FFB)
  # ------------------------------------------------------------------------
  ffb_genus <- ffb$genus[.ffb_key(sub("\\s.*", "", ffb$genus)) == .ffb_key(taxon)]
  if (verbose && length(ffb_genus) == 0) {
    message("  No species of '", taxon, "' found in the current FFB checklist ",
            "(treating FFB's known species list as empty).")
  }
  ffb_keys <- .name_key(ffb$all)

  # ------------------------------------------------------------------------
  # 2. Candidates from each source
  # ------------------------------------------------------------------------
  powo <- NULL
  if ("powo" %in% sources) {
    powo <- .powo_species_candidates(taxon, ffb_keys, wcvp, powo_source,
                                     require_brazil_evidence, fetch_details,
                                     max_number, verbose)
  }
  gbif <- NULL
  if (!is.null(genus)) {
    gbif <- .gbif_species_candidates(genus, ffb_keys, type_status, verbose)
  }
  herbaria <- list()
  for (src in intersect(c("reflora", "jabot"), sources)) {
    recs <- .herbarium_subset(herb_all[[src]], taxon)
    if (!is.null(recs)) {
      herbaria[[src]] <- .herbarium_species_candidates(recs, taxon, ffb_keys,
                                                       .herbarium_sources[[src]]$label,
                                                       verbose)
    }
  }
  candidates <- function(x) if (is.null(x)) NULL else x$result

  # ------------------------------------------------------------------------
  # 3. One table, one row per name
  # ------------------------------------------------------------------------
  result <- .merge_species_candidates(candidates(powo), candidates(gbif),
                                      candidates(herbaria$reflora),
                                      candidates(herbaria$jabot))
  n_multi <- sum(grepl(";", result$Found_in))

  if (verbose) {
    message(sprintf("\n\u2713 %d species of '%s' found missing from FFB%s", nrow(result),
                    taxon, if (length(sources) > 1) {
                      sprintf(" (%d reported by more than one source)", n_multi)
                    } else ""))
  }

  if (save && nrow(result) > 0) {
    dir <- .arg_check_dir(dir)
    .save_xlsx(result, verbose = verbose, filename = filename, dir = dir)
  }

  if (html_report && nrow(result) > 0) {
    source_lines <- c(
      if (!is.null(powo)) {
        paste0("POWO distributions: ", if (powo_source == "rWCVPdata") {
          paste0("WCVP ", wcvp$version, " (rWCVPdata)")
        } else {
          "POWO/WCVP via ChecklistBank"
        })
      },
      if (!is.null(gbif)) {
        paste0("GBIF type specimens collected in Brazil (", genus$name, ", ",
               genus$kingdom, ")")
      },
      if (!is.null(herbaria$reflora)) "REFLORA type specimens collected in Brazil (refloraR)",
      if (!is.null(herbaria$jabot)) "JABOT type specimens collected in Brazil (jabotR)"
    )
    report_data <- list(
      taxon = taxon,
      sources = c(if (!is.null(powo)) "POWO", if (!is.null(gbif)) "GBIF",
                  if (!is.null(herbaria$reflora)) "REFLORA",
                  if (!is.null(herbaria$jabot)) "JABOT"),
      source_lines = source_lines,
      n_in_ffb = length(ffb_genus),
      n_powo_names = if (is.null(powo)) NA_integer_ else powo$n_names,
      n_gbif_names = if (is.null(gbif)) NA_integer_ else gbif$n_names,
      n_reflora_names = if (is.null(herbaria$reflora)) NA_integer_ else herbaria$reflora$n_names,
      n_jabot_names = if (is.null(herbaria$jabot)) NA_integer_ else herbaria$jabot$n_names,
      n_missing = nrow(result),
      n_multi = n_multi,
      brazil_filtered = require_brazil_evidence,
      result = result
    )

    dir <- .arg_check_dir(dir)
    .flora_render_report(template = "flora_species_gap_report.Rmd",
                         data_list = report_data,
                         taxon = taxon,
                         dir = dir,
                         filename = filename,
                         verbose = verbose,
                         open_report = open_report)
  }

  return(result)
}


#_______________________________________________________________________________
# POWO candidates: species-level POWO names of a genus missing from FFB, with
# their Brazil evidence. Returns list(result, n_names), 'result' holding the
# common columns (Taxon_name, Authors, Accepted_name) plus POWO_* columns. ####
.powo_species_candidates <- function(taxon, ffb_keys, wcvp, powo_source,
                                     require_brazil_evidence, fetch_details,
                                     max_number, verbose) {
  # Every species-level name in the genus. From rWCVPdata, distributions and
  # citations come in the same in-memory join; from ChecklistBank they are
  # fetched below, for FFB-missing names only.
  if (powo_source == "rWCVPdata") {
    if (verbose) message("Reading POWO/WCVP names and distributions for genus '", taxon, "'...")
    pw <- .wcvp_genus_names(taxon, wcvp)
  } else {
    if (verbose) message("Querying POWO (via ChecklistBank) for genus '", taxon, "'...")
    pw <- .powo_name_search(taxon, max_number = max_number)
  }
  if (verbose) message("  Found ", nrow(pw), " species-level name(s) in POWO.")

  # "Already in FFB" if either the name or its accepted name (which may sit
  # in another genus) matches any FFB name, ignoring orthographic variants
  already_in_ffb <- .name_key(pw$Taxon_name) %in% ffb_keys |
    .name_key(pw$Accepted_name) %in% ffb_keys
  missing <- pw[!already_in_ffb, ]
  rownames(missing) <- NULL
  if (verbose) message("  ", nrow(missing), " of those are not currently in FFB.")

  if (powo_source == "checklistbank") {
    missing <- .powo_api_enrich(missing, fetch_details = fetch_details, verbose = verbose)
  }

  # Brazil evidence: the accepted name's distribution (the areas POWO maps),
  # falling back to POWO's free-text range when there is none
  range_brazil <- vapply(missing$POWO_Range, .location_mentions_brazil, logical(1),
                         USE.NAMES = FALSE)
  result <- data.frame(
    Taxon_name = missing$Taxon_name,
    Authors = missing$Authors,
    Accepted_name = missing$Accepted_name,
    POWO_ID = missing$POWO_ID,
    POWO_Taxonomic_status = missing$Taxonomic_status,
    POWO_Accepted_name = missing$Accepted_name,
    POWO_Year = missing$Year,
    POWO_Published_in = missing$POWO_Published_in,
    POWO_Range = missing$POWO_Range,
    POWO_Distribution = missing$POWO_Distribution,
    POWO_Brazil_Areas = missing$POWO_Brazil_Areas,
    POWO_Brazil_Evidence = ifelse(missing$Has_distribution,
                                  !is.na(missing$POWO_Brazil_Areas), range_brazil),
    POWO_Evidence_Source = ifelse(missing$Has_distribution,
                                  "WCVP distribution", "POWO range text"),
    # sprintf(), unlike paste0(), returns nothing for zero missing names
    POWO_URL = ifelse(is.na(missing$POWO_ID), NA_character_,
                      sprintf("https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:%s",
                              missing$POWO_ID)),
    IPNI_URL = ifelse(is.na(missing$IPNI_ID), NA_character_,
                      sprintf("https://www.ipni.org/n/%s", missing$IPNI_ID)),
    stringsAsFactors = FALSE
  )

  if (require_brazil_evidence) {
    if (verbose) {
      message(sprintf("  %d of them have POWO distribution evidence for Brazil.",
                      sum(result$POWO_Brazil_Evidence, na.rm = TRUE)))
    }
    result <- result[result$POWO_Brazil_Evidence %in% TRUE, ]
  }
  rownames(result) <- NULL
  list(result = result, n_names = nrow(pw))
}


#_______________________________________________________________________________
# GBIF candidates: species-level names of a genus with type specimens
# collected in Brazil that are missing from FFB, with a summary of those
# specimens. Returns list(result, n_names), 'result' holding the common
# columns plus GBIF_* columns; NULL when the GBIF search fails. ####
.gbif_species_candidates <- function(genus, ffb_keys, type_status, verbose) {
  if (verbose) message("Searching GBIF for type specimens collected in Brazil...")

  counts <- .gbif_brazil_type_counts(genus$key, type_status)
  if (is.null(counts)) {
    if (verbose) message("  The GBIF occurrence search failed; skipping GBIF.")
    return(NULL)
  }
  names_df <- .gbif_genus_names(genus$key)

  # Keys outside the genus listing (rare) are looked up one by one
  val <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  for (k in setdiff(counts$GBIF_Key, names_df$GBIF_Key)) {
    r <- .get_json(paste0("https://api.gbif.org/v1/species/", k))
    if (is.null(r)) next
    names_df <- rbind(names_df, data.frame(
      GBIF_Key = val(r$key), Taxon_name = val(r$canonicalName), Authors = val(r$authorship),
      Rank = val(r$rank), Taxonomic_status = val(r$taxonomicStatus),
      Accepted_name = val(r$species),
      Accepted_full_name = if (is.null(r$accepted)) val(r$scientificName) else val(r$accepted),
      Genus = val(r$genus), GBIF_Published_in = val(r$publishedIn), stringsAsFactors = FALSE))
  }

  typed <- merge(counts, names_df, by = "GBIF_Key")
  typed <- typed[typed$Rank %in% "SPECIES" & !is.na(typed$Taxon_name), ]
  typed$Accepted_name[is.na(typed$Accepted_name)] <- typed$Taxon_name[is.na(typed$Accepted_name)]
  if (verbose) {
    message("  ", nrow(typed), " species-level name(s) have type specimens collected in Brazil.")
  }

  already_in_ffb <- .name_key(typed$Taxon_name) %in% ffb_keys |
    .name_key(typed$Accepted_name) %in% ffb_keys
  missing <- typed[!already_in_ffb, ]
  rownames(missing) <- NULL
  if (verbose) message("  ", nrow(missing), " of those are not currently in FFB.")

  # Summarise the Brazilian type specimens of the missing names only
  if (nrow(missing) > 0 && verbose) message("  Retrieving their type specimens...")
  specimens <- if (nrow(missing) > 0) {
    .gbif_type_specimens(missing$GBIF_Key, type_status)
  } else {
    .gbif_type_specimens(character(0))
  }
  # Standardise the type statuses (barRoso::std_types(), each of a specimen's
  # statuses separately) and states (barRoso::std_place())
  if (nrow(specimens) > 0) {
    statuses <- strsplit(specimens$Type_status, ",\\s*")
    std <- suppressMessages(barRoso::std_types(data.frame(
      typeStatus = unlist(statuses), stringsAsFactors = FALSE)))$typeStatus
    specimens$Type_status <- vapply(split(std, rep(seq_along(statuses), lengths(statuses))),
                                    .collapse_unique, character(1), USE.NAMES = FALSE)
    # GBIF is queried for Brazil only: the country is set for barRoso::std_place()
    specimens$State <- .normalize_br_state(suppressMessages(barRoso::std_place(data.frame(
      country = "Brazil", stateProvince = specimens$State, locality = NA_character_,
      stringsAsFactors = FALSE)))$stateProvince)
  }

  # A query for a key also returns its synonyms' types, filed under their own
  # keys: assign each specimen to the requested name via its own, accepted,
  # or species key
  specimens$Owner <- ifelse(specimens$GBIF_Key %in% missing$GBIF_Key, specimens$GBIF_Key,
                            ifelse(specimens$Accepted_key %in% missing$GBIF_Key,
                                   specimens$Accepted_key, specimens$Species_key))

  collapse_unique <- .collapse_unique
  summarise <- function(key, field) {
    sp <- specimens[specimens$Owner %in% key, ]
    if (nrow(sp) == 0) return(NA_character_)
    switch(field,
           statuses = collapse_unique(unlist(strsplit(sp$Type_status, ";\\s*"))),
           specimens = collapse_unique(trimws(paste0(
             ifelse(is.na(sp$Institution), "", sp$Institution), " ",
             ifelse(is.na(sp$Catalog_number), "", sp$Catalog_number),
             ifelse(is.na(sp$Type_status), "", paste0(" (", sp$Type_status, ")")))),
             max = 10),
           names = collapse_unique(sp$Specimen_name),
           collectors = collapse_unique(sp$Collector, max = 5),
           years = .year_range(sp$Year),
           states = collapse_unique(sp$State))
  }
  per_name <- function(field) {
    vapply(missing$GBIF_Key, summarise, character(1), field = field, USE.NAMES = FALSE)
  }

  result <- data.frame(
    Taxon_name = missing$Taxon_name,
    Authors = missing$Authors,
    Accepted_name = missing$Accepted_name,
    GBIF_Key = missing$GBIF_Key,
    GBIF_Taxonomic_status = missing$Taxonomic_status,
    GBIF_Accepted_name = missing$Accepted_name,
    GBIF_N_type_specimens = missing$N_type_specimens,
    GBIF_Type_statuses = per_name("statuses"),
    GBIF_Type_specimens = per_name("specimens"),
    GBIF_Specimen_names = per_name("names"),
    GBIF_Collectors = per_name("collectors"),
    GBIF_Years = per_name("years"),
    GBIF_States = per_name("states"),
    GBIF_Published_in = missing$GBIF_Published_in,
    # sprintf(), unlike paste0(), returns nothing for zero missing names
    GBIF_URL = sprintf("https://www.gbif.org/species/%s", missing$GBIF_Key),
    stringsAsFactors = FALSE
  )
  list(result = result, n_names = nrow(typed))
}


#_______________________________________________________________________________
# Herbarium candidates (REFLORA, JABOT): species-level names of the genus
# whose specimens collected in Brazil include types, not registered in FFB.
# 'recs' are records standardised by .std_herbarium_records(), so the type
# statuses are already standardised with barRoso::std_types(). Returns
# list(result, n_names) - one row per missing name, with <LABEL>_* columns -
# matching the shape of the GBIF candidates. ####
.herbarium_species_candidates <- function(recs, genus, ffb_keys, label, verbose = TRUE) {
  typed <- recs[!is.na(recs$Type_status) & !is.na(recs$Taxon_name) &
                  .in_brazil(recs$Country, recs$State) &
                  .name_key(sub("\\s.*", "", recs$Taxon_name)) == .name_key(genus), ,
                drop = FALSE]
  typed$.key <- .name_key(typed$Taxon_name)
  if (verbose) {
    message("  ", length(unique(typed$.key)), " species-level name(s) have type specimens ",
            "collected in Brazil in ", label, ".")
  }
  missing <- typed[!typed$.key %in% ffb_keys, , drop = FALSE]
  keys <- unique(missing$.key)

  first <- function(x) { x <- x[!is.na(x) & nzchar(x)]; if (length(x)) x[1] else NA_character_ }
  per_name <- function(f) vapply(keys, function(k) f(missing[missing$.key == k, ]),
                                 character(1), USE.NAMES = FALSE)
  taxon_name <- per_name(function(sp) first(sp$Taxon_name))
  result <- data.frame(
    Taxon_name = taxon_name,
    Authors = per_name(function(sp) first(sp$Authors)),
    Accepted_name = taxon_name,
    N_type_specimens = vapply(keys, function(k) sum(missing$.key == k), integer(1),
                              USE.NAMES = FALSE),
    Type_statuses = per_name(function(sp) .collapse_unique(sp$Type_status)),
    Type_specimens = per_name(function(sp) .collapse_unique(trimws(paste0(
      ifelse(is.na(sp$Institution), "", sp$Institution), " ",
      ifelse(is.na(sp$CatalogNumber), "", sp$CatalogNumber),
      " (", sp$Type_status, ")")), max = 10)),
    Collectors = per_name(function(sp) .collapse_unique(sp$RecordedBy, max = 5)),
    Years = per_name(function(sp) .year_range(sp$EventDate)),
    States = per_name(function(sp) .collapse_unique(sp$State)),
    Links = per_name(function(sp) .collapse_unique(sp$RecordLink, max = 3)),
    stringsAsFactors = FALSE
  )
  if (verbose && nrow(result) > 0) {
    message("  ", nrow(result), " of them not in FFB.")
  }
  prefixed <- setdiff(names(result), c("Taxon_name", "Authors", "Accepted_name"))
  names(result)[names(result) %in% prefixed] <- paste0(label, "_", prefixed)
  list(result = result, n_names = length(unique(typed$.key)))
}


# Columns contributed by each source to the merged table
.species_gap_columns <- list(
  POWO = c("POWO_ID", "POWO_Taxonomic_status", "POWO_Accepted_name", "POWO_Year",
           "POWO_Published_in", "POWO_Range", "POWO_Distribution", "POWO_Brazil_Areas",
           "POWO_Brazil_Evidence", "POWO_Evidence_Source", "POWO_URL", "IPNI_URL"),
  GBIF = c("GBIF_Key", "GBIF_Taxonomic_status", "GBIF_Accepted_name",
           "GBIF_N_type_specimens", "GBIF_Type_statuses", "GBIF_Type_specimens",
           "GBIF_Specimen_names", "GBIF_Collectors", "GBIF_Years", "GBIF_States",
           "GBIF_Published_in", "GBIF_URL"),
  REFLORA = paste0("REFLORA_", c("N_type_specimens", "Type_statuses", "Type_specimens",
                                 "Collectors", "Years", "States", "Links")),
  JABOT = paste0("JABOT_", c("N_type_specimens", "Type_statuses", "Type_specimens",
                             "Collectors", "Years", "States", "Links"))
)


#_______________________________________________________________________________
# Merge the candidates of each source (POWO, GBIF, REFLORA, JABOT) into one
# row per name (matched by .name_key(), so spelling variants merge), adding
# Found_in (the sources reporting the name, e.g. "POWO; GBIF; REFLORA") and
# Brazil_evidence, and sorting the names reported by more sources first. The
# name, authors, and accepted name come from the first source, in that order,
# that reports them. Any input may be NULL (source not used). ####
.merge_species_candidates <- function(powo = NULL, gbif = NULL, reflora = NULL, jabot = NULL) {
  common <- c("Taxon_name", "Authors", "Accepted_name")
  tabs <- list(POWO = powo, GBIF = gbif, REFLORA = reflora, JABOT = jabot)
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]

  m <- NULL
  for (lab in names(tabs)) {
    t <- tabs[[lab]]
    t$.key <- .name_key(t$Taxon_name)
    t[[paste0(".in_", lab)]] <- rep(TRUE, nrow(t))
    if (is.null(m)) {
      m <- t
      next
    }
    names(t)[names(t) %in% common] <- paste0(".new_", names(t)[names(t) %in% common])
    m <- merge(m, t, by = ".key", all = TRUE, sort = FALSE)
    for (col in common) {
      m[[col]] <- ifelse(is.na(m[[col]]), m[[paste0(".new_", col)]], m[[col]])
      m[[paste0(".new_", col)]] <- NULL
    }
  }
  if (is.null(m)) {
    m <- data.frame(Taxon_name = character(0), Authors = character(0),
                    Accepted_name = character(0), stringsAsFactors = FALSE)
  }

  inside <- do.call(cbind, c(list(matrix(logical(0), nrow = nrow(m), ncol = 0)),
                             lapply(names(tabs), function(lab) m[[paste0(".in_", lab)]] %in% TRUE)))
  m$Found_in <- if (nrow(m) == 0) character(0) else {
    apply(inside, 1, function(r) paste(names(tabs)[r], collapse = "; "))
  }
  powo_evidence <- if ("POWO_Brazil_Evidence" %in% names(m)) m$POWO_Brazil_Evidence %in% TRUE else FALSE
  specimen_evidence <- if (ncol(inside) > 0) {
    rowSums(inside[, names(tabs) != "POWO", drop = FALSE]) > 0
  } else logical(0)
  m$Brazil_evidence <- powo_evidence | specimen_evidence
  m$In_FFB <- logical(nrow(m))

  cols <- c(common, "Found_in", "Brazil_evidence",
            unlist(.species_gap_columns[names(tabs)], use.names = FALSE), "In_FFB")
  m <- m[order(-rowSums(inside), m$Taxon_name), intersect(cols, names(m)), drop = FALSE]
  rownames(m) <- NULL
  m
}
