#' Match two plant name lists using the Flora e Funga do Brasil database
#'
#' @description
#' Matches and compares two lists of plant names based on their taxonomic
#' resolution in the Flora e Funga do Brasil (FFB) database. Both lists are
#' independently searched with \code{\link{flora_search}}, and names that
#' resolve to the same accepted taxon are considered a match. The function
#' returns a merged data frame that aligns names across both lists.
#'
#' @details
#' Each list is searched independently via \code{\link{flora_search}}. The
#' accepted taxon IDs (\code{Accepted.taxon.ID}) are then used to determine
#' which names in \code{splist2} correspond to names in \code{splist1}. Two
#' names match when they resolve to the same accepted taxon — regardless of
#' whether one or both are synonyms.
#'
#' The resulting data frame is ordered by \code{splist1}. When
#' \code{include_all = TRUE}, names present only in \code{splist2} are appended
#' at the end with \code{NA} in the \code{Species.List.1} column.
#'
#' The \code{Match.Position.2to1} column gives the row index of the matched
#' name in \code{splist2}; use it to re-order \code{splist2} to align with
#' \code{splist1}:
#' \code{splist2[result$Match.Position.2to1]}.
#'
#' @usage
#' flora_match(
#'   splist1,
#'   splist2,
#'   version = "latest",
#'   max_distance = 0.2,
#'   genus_fuzzy = FALSE,
#'   include_all = TRUE,
#'   identify_dups = TRUE,
#'   show_correct = TRUE,
#'   progress_bar = TRUE,
#'   rm_flora_database = FALSE,
#'   verbose = TRUE
#' )
#'
#' @param splist1 A character vector of plant names — the reference list.
#'   Each element should include at least a genus and a specific epithet, and
#'   optionally infraspecific rank, infraspecific epithet, and author name.
#'   Only valid characters are allowed (see
#'   \code{\link[base:validEnc]{base::validEnc}}).
#'
#' @param splist2 A character vector of plant names — the list to match against
#'   \code{splist1}. Same format requirements as \code{splist1}.
#'
#' @param version Character. FFB dataset version to use. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}} and
#'   \code{\link{flora_parse}}.
#'
#' @param max_distance Numeric. Maximum string distance for fuzzy matching.
#'   Can be a positive integer (e.g., \code{2} allows up to 2 edits) or a
#'   fraction of the name length (e.g., \code{0.1} allows 1 edit per 10
#'   characters). Default is \code{0.2}.
#'
#' @param genus_fuzzy Logical. If \code{TRUE}, the fuzzy match also applies to
#'   the genus (slower). If \code{FALSE} (default), the genus must match exactly
#'   and only the epithet is matched fuzzily.
#'
#' @param include_all Logical. If \code{TRUE} (default), names found only in
#'   \code{splist2} are appended to the result with \code{NA} in the
#'   \code{Species.List.1} column. If \code{FALSE}, only names from
#'   \code{splist1} appear in the output.
#'
#' @param identify_dups Logical. If \code{TRUE} (default), a
#'   \code{Duplicated.Output.Position} column is added indicating the row
#'   position of the first other occurrence of the same \code{Accepted.taxon.Name}.
#'   This occurs when two input names are synonyms of the same accepted taxon.
#'   \code{NA} is returned when there is no duplicate.
#'
#' @param show_correct Logical. If \code{TRUE}, a \code{Correct} column is
#'   appended indicating whether the input was exactly matched (\code{TRUE}) or
#'   fuzzily matched (\code{FALSE}) in the \code{flora_search} results for
#'   \code{splist1}. Default is \code{TRUE}.
#'
#' @param progress_bar Logical. If \code{TRUE}, a progress bar is printed during
#'   name searches. Default is \code{TRUE}.
#'
#' @param rm_flora_database Logical. If \code{TRUE}, the downloaded FFB database
#'   folder (\code{"flora_download"}) is deleted after the search is complete.
#'   If \code{FALSE} (default), the database is kept on disk. Keeping the
#'   database is **recommended** because subsequent searches will reuse the
#'   existing download, checking if the stored version matches the requested
#'   \code{version}. If the stored version is outdated or different from the
#'   requested version, the function automatically re-downloads the correct
#'   version. This caching behavior significantly speeds up repeated searches.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative progress
#'   messages during parsing and download. If \code{FALSE}, runs quietly.
#'
#' @return
#' A data frame with one row per input name (from \code{splist1} first, then
#' any unmatched names from \code{splist2} when \code{include_all = TRUE}) and
#' the following columns:
#' \describe{
#'   \item{Species.List.1}{Names from \code{splist1} (\code{NA} for rows that
#'     come only from \code{splist2}).}
#'   \item{Species.List.2}{The matched name from \code{splist2}, or \code{NA}
#'     if no match was found.}
#'   \item{FFB.taxon.ID}{Taxon ID in the FFB database.}
#'   \item{Input.Genus}{Genus extracted from the input name.}
#'   \item{Input.Epithet}{Specific epithet extracted from the input name.}
#'   \item{taxonRank}{Taxonomic rank (e.g., \code{"ESPECIE"}, \code{"VARIEDADE"}).}
#'   \item{Input.InfraspecificEpithet}{Infraspecific epithet if present.}
#'   \item{scientificNameAuthorship}{Author citation of the matched taxon.}
#'   \item{taxonomicStatus}{FFB taxonomic status: \code{"NOME_ACEITO"}
#'     (accepted), \code{"SINONIMO"} (synonym), or \code{"NOME_DUVIDOSO"}.}
#'   \item{Accepted.taxon.ID}{ID of the accepted name.}
#'   \item{Accepted.taxon.Name}{Accepted taxon name resolved from the FFB
#'     database.}
#'   \item{family}{Plant family.}
#'   \item{order}{Plant order (when available in the FFB data).}
#'   \item{Correct}{(Only when \code{show_correct = TRUE}) Logical indicating
#'     an exact match (\code{TRUE}) vs. a fuzzy match (\code{FALSE}) for
#'     \code{splist1}.}
#'   \item{Match.Position.2to1}{Row index of the matched name in
#'     \code{splist2}.}
#'   \item{Duplicated.Output.Position}{(Only when \code{identify_dups = TRUE})
#'     Row index of the first other occurrence of the same
#'     \code{Accepted.taxon.Name}, or \code{NA} if unique.}
#' }
#'
#' @section Database caching behavior:
#' The FFB dataset is downloaded only once and cached locally in the
#' \code{"flora_download"} folder. On subsequent calls:
#' \itemize{
#'   \item If \code{rm_flora_database = FALSE} (default), the function checks if
#'     the cached version matches the requested \code{version}. If yes, it
#'     reuses the existing download; if not, it downloads the correct version.
#'   \item If \code{rm_flora_database = TRUE}, the database is deleted after
#'     each search, forcing a fresh download on every call (not recommended
#'     for repeated searches).
#' }
#'
#' @seealso
#' \code{\link{flora_search}} for single-list searching.
#' \code{\link{flora_download}} to manually download the DwC-A dataset.
#' \code{\link{flora_parse}} to manually parse the downloaded dataset.
#'
#' @author
#' Domingos Cardoso & Kelmer Martins-Cunha
#'
#' @references
#' BFG — The Brazil Flora Group (2022). Brazilian Flora 2020: Leveraging the
#' power of a collaborative scientific network. \emph{Taxon}, 71(1), 178–198.
#' \doi{10.1002/tax.12640}
#'
#' @examples
#' \dontrun{
#' # Two lists that may overlap or contain synonyms for the same accepted names
#' splist1 <- c("Mimosa sensitiva", "Swartzia simplex", "Inga edulis")
#' splist2 <- c("Swartzia simplex var. grandiflora", "Inga edulis",
#'               "Mimosa pyrenea")
#'
#' # Match including all names from both lists (downloads + parses automatically)
#' result <- flora_match(splist1, splist2, include_all = TRUE)
#'
#' # Match including only names from splist1
#' result2 <- flora_match(splist1, splist2, include_all = FALSE)
#'
#' # Re-order splist2 to align with splist1
#' splist2[result2$Match.Position.2to1]
#'
#' # Use a specific FFB version stored in a custom folder
#' flora_match(splist1, splist2, version = "393.418")
#'
#' # Force fresh download by removing cached database after match
#' flora_match(splist1, splist2, rm_flora_database = TRUE)
#' }
#'
#' @export

flora_match <- function(splist1,
                        splist2,
                        version = "latest",
                        max_distance = 0.2,
                        genus_fuzzy = FALSE,
                        include_all = TRUE,
                        identify_dups = TRUE,
                        show_correct = TRUE,
                        progress_bar = TRUE,
                        rm_flora_database = FALSE,
                        verbose = TRUE) {

  if (is.factor(splist1)) splist1 <- as.character(splist1)
  if (is.factor(splist2)) splist2 <- as.character(splist2)
  .flora_names_check(splist1, "splist1")
  .flora_names_check(splist2, "splist2")

  # Download and parse once; reuse for both searches
  ffb <- .flora_prepare_taxon(version = version,
                              verbose = verbose,
                              rm_flora_database = rm_flora_database)

  search1 <- .flora_search_impl(splist1,
                                ffb$taxon_df, ffb$genus_index, ffb$id_lookup,
                                max_distance, genus_fuzzy,
                                show_correct, progress_bar)

  if (is.null(search1)) {
    stop("No match found for splist1. Try increasing 'max_distance'.",
         call. = FALSE)
  }

  search2 <- .flora_search_impl(splist2,
                                ffb$taxon_df, ffb$genus_index, ffb$id_lookup,
                                max_distance, genus_fuzzy,
                                show_correct, progress_bar)
  if (is.null(search2)) {
    stop("No match found for splist2. Try increasing 'max_distance'.",
         call. = FALSE)
  }

  match_pos <- match(search1$Accepted.taxon.ID, search2$Accepted.taxon.ID,
                     incomparables = NA)

  ffb_cols <- names(search1)[-1L]   # all columns except "Search"

  result <- cbind(
    data.frame(
      Species.List.1 = splist1,
      Species.List.2 = splist2[match_pos],
      stringsAsFactors = FALSE
    ),
    search1[, ffb_cols, drop = FALSE],
    data.frame(
      Match.Position.2to1 = match_pos,
      stringsAsFactors    = FALSE
    )
  )

  if (include_all) {
    matched_in_sp2  <- match_pos[!is.na(match_pos)]
    unmatched_in_sp2 <- setdiff(seq_along(splist2), matched_in_sp2)

    if (length(unmatched_in_sp2) > 0L) {
      extra <- cbind(
        data.frame(
          Species.List.1 = NA_character_,
          Species.List.2 = splist2[unmatched_in_sp2],
          stringsAsFactors = FALSE
        ),
        search2[unmatched_in_sp2, ffb_cols, drop = FALSE],
        data.frame(
          Match.Position.2to1 = unmatched_in_sp2,
          stringsAsFactors = FALSE
        )
      )
      result <- rbind(result, extra)
    }
  }

  if (identify_dups) {
    result$Duplicated.Output.Position <- .flora_find_dups(result)
  }

  rownames(result) <- NULL
  result
}
