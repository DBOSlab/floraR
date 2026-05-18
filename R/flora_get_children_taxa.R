#' Get child taxa from Flora e Funga do Brasil
#'
#' Returns all child taxa (species, subspecies, varieties, etc.) for a given
#' taxonomic name at a specified rank. Useful for getting all species in a genus,
#' all genera in a family, etc.
#'
#' @param taxon_name Character. Scientific name to search for (e.g.,
#'   "Luetzelburgia", "Fabaceae", "Magnoliopsida").
#'
#' @param rank Character. The taxonomic rank of the input name. Options:
#'   \code{"class"}, \code{"order"}, \code{"family"}, \code{"genus"},
#'   \code{"species"}. Default is \code{"genus"}.
#'
#' @param child_rank Character or NULL. The taxonomic rank(s) to return as children.
#'   If a single rank is provided (e.g., \code{"species"}), returns only that rank.
#'   If \code{NULL} (default), returns ALL descendant ranks (e.g., for a genus:
#'   species, subspecies, varieties, etc.). Options: \code{"class"}, \code{"order"},
#'   \code{"family"}, \code{"genus"}, \code{"species"}, \code{"subspecies"}.
#'
#' @param include_synonyms Logical. If \code{TRUE}, includes synonym names
#'   (\code{taxonomicStatus == "SINONIMO"}) in addition to accepted names.
#'   Default is \code{FALSE}.
#'
#' @param version Character. FFB dataset version to use. Defaults to
#'   \code{"latest"}. Passed to \code{\link{flora_download}} and
#'   \code{\link{flora_parse}}.
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
#'   messages during parsing. If \code{FALSE}, runs quietly.
#'
#' @return A data frame containing child taxa that match the requested parent
#'   taxon after the applied filters.
#'
#' @details
#' The function first finds the parent taxon, then returns all children
#' that have the parent's \code{taxonID} in their lineage.
#'
#' **Child rank behavior:**
#' \itemize{
#'   \item If \code{child_rank = NULL}: Returns ALL descendant ranks
#'   \item If \code{child_rank = "species"}: Returns only species (direct children)
#'   \item If \code{child_rank = "subspecies"}: Returns only subspecies
#' }
#'
#' @section Database caching behavior:
#' The Brazilian flora dataset is downloaded only once and cached locally in the
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
#' @examples
#' \dontrun{
#' # Get all species in a genus (direct children)
#' species <- flora_get_children_taxa(
#'   taxon_name = "Luetzelburgia",
#'   rank = "genus",
#'   child_rank = "species"
#' )
#'
#' # Get ALL descendants (species, subspecies, varieties)
#' all_descendants <- flora_get_children_taxa(
#'   taxon_name = "Luetzelburgia",
#'   rank = "genus",
#'   child_rank = NULL
#' )
#'
#' # Get all genera in a family (including synonyms)
#' genera <- flora_get_children_taxa(
#'   taxon_name = "Fabaceae",
#'   rank = "family",
#'   child_rank = "genus",
#'   include_synonyms = TRUE
#' )
#' }
#'
#' @export

flora_get_children_taxa <- function(taxon_name = NULL,
                                    rank = c("class", "order", "family", "tribe", "genus", "species"),
                                    child_rank = NULL,
                                    include_synonyms = FALSE,
                                    version = "latest",
                                    rm_flora_database = FALSE,
                                    verbose = TRUE) {

  # ============================================================
  # INPUT VALIDATION
  # ============================================================

  if (is.null(taxon_name)) {
    stop("'taxon_name' must be provided.", call. = FALSE)
  }

  rank <- match.arg(rank)

  # Valid child ranks
  valid_child_ranks <- c("class", "order", "family", "tribe", "genus", "species", "subspecies", "variety", "form")

  if (!is.null(child_rank) && !child_rank %in% valid_child_ranks) {
    stop(sprintf("Invalid child_rank '%s'. Valid options: %s",
                 child_rank, paste(valid_child_ranks, collapse = ", ")),
         call. = FALSE)
  }

  # ============================================================
  # DEFINE TAXONOMIC HIERARCHY
  # ============================================================

  rank_map <- c(
    class = "CLASSE",
    order = "ORDEM",
    family = "FAMILIA",
    genus = "GENERO",
    tribe = "TRIBO",
    species = "ESPECIE",
    subspecies = "SUBESPECIE",
    variety = "VARIEDADE",
    form = "FORMA"
  )

  rank_order <- c("class", "order", "family", "tribe", "genus", "species", "subspecies", "variety", "form")

  # Find index of parent rank and child rank
  parent_index <- which(rank_order == rank)

  # Determine child ranks to retrieve
  if (is.null(child_rank)) {
    # Get ALL descendant ranks (below parent)
    child_ranks_to_get <- rank_order[(parent_index + 1):length(rank_order)]

    if (length(child_ranks_to_get) == 0) {
      warning(sprintf("No descendant ranks found for rank '%s'", rank), call. = FALSE)
      return(data.frame())
    }

    if (verbose) {
      message(sprintf("Retrieving ALL descendant ranks: %s",
                      paste(child_ranks_to_get, collapse = " → ")))
    }
  } else {
    # Get only the specified child rank
    child_index <- which(rank_order == child_rank)

    if (child_index <= parent_index) {
      stop(sprintf("child_rank '%s' must be lower than parent rank '%s'",
                   child_rank, rank), call. = FALSE)
    }

    child_ranks_to_get <- child_rank
  }

  # ============================================================
  # DOWNLOAD AND PARSE FFB DATA
  # ============================================================

  if (verbose) message("\nDownloading and parsing Flora e Funga do Brasil data...")

  floraR::flora_download(version = version,
                         dir = "flora_download",
                         verbose = verbose)
  dwca <- floraR::flora_parse(path = "flora_download",
                              version = version,
                              verbose = verbose)

  ffb_taxa <- dwca[[list.files("flora_download")]][["data"]][["taxon.txt"]]

  # ============================================================
  # FIND THE PARENT TAXON
  # ============================================================

  ffb_parent_rank <- rank_map[rank]

  # Find parent taxon
  if (rank == "species") {
    parent_rows <- which(ffb_taxa$taxonName == taxon_name &
                           ffb_taxa$taxonRank == ffb_parent_rank)
  } else if (rank %in% c("class")) {
    parent_rows <- which(ffb_taxa$class == taxon_name &
                           ffb_taxa$taxonRank == ffb_parent_rank)
  } else if (rank == "order") {
    parent_rows <- which(ffb_taxa$order == taxon_name &
                           ffb_taxa$taxonRank == ffb_parent_rank)
  } else if (rank == "family") {
    parent_rows <- which(ffb_taxa$family == taxon_name &
                           ffb_taxa$taxonRank == ffb_parent_rank)
  } else if (rank == "genus") {
    parent_rows <- which(ffb_taxa$genus == taxon_name &
                           ffb_taxa$taxonRank == ffb_parent_rank)
  }

  if (length(parent_rows) == 0) {
    stop(sprintf("Taxon '%s' with rank '%s' not found in FFB database.",
                 taxon_name, rank), call. = FALSE)
  }

  parent_taxon <- ffb_taxa[parent_rows[1], ]
  parent_id <- parent_taxon$id

  if (verbose) {
    message(sprintf("\nFound parent: %s (%s [%s])",
                    parent_taxon$taxonName,
                    parent_taxon$taxonRank,
                    rank))
  }

  # ============================================================
  # FIND CHILD TAXA (RECURSIVELY IF NEEDED)
  # ============================================================

  # Function to find children recursively
  find_children_recursive <- function(parent_ids, target_ranks, current_depth = 0) {
    if (length(target_ranks) == 0 || length(parent_ids) == 0) {
      return(data.frame())
    }

    # Get the next rank to retrieve
    next_rank <- target_ranks[1]
    ffb_next_rank <- rank_map[next_rank]

    # Find direct children of current parents
    direct_children <- ffb_taxa[ffb_taxa$parentNameUsageID %in% parent_ids &
                                ffb_taxa$taxonRank == ffb_next_rank, ]

    if (nrow(direct_children) == 0) {
      return(data.frame())
    }

    if (verbose && current_depth == 0) {
      message(sprintf("  Found %d %s", nrow(direct_children), next_rank))
    }

    # If there are more ranks to retrieve, continue recursively
    if (length(target_ranks) > 1) {
      remaining_ranks <- target_ranks[-1]
      child_ids <- as.character(direct_children$id)
      deeper_children <- find_children_recursive(child_ids, remaining_ranks, current_depth + 1)

      if (nrow(deeper_children) > 0) {
        direct_children <- rbind(direct_children, deeper_children)
      }
    }

    return(direct_children)
  }

  # Start recursive search
  children <- find_children_recursive(parent_id, child_ranks_to_get)

  # Fallback: if no children found via parentNameUsageID, try direct filter
  if (nrow(children) == 0 && rank == "family" && "genus" %in% child_ranks_to_get) {
    children <- ffb_taxa[ffb_taxa$family == taxon_name &
                         ffb_taxa$taxonRank == rank_map["genus"], ]
    if (verbose && nrow(children) > 0) {
      message(sprintf("  Found %d genera via family column fallback", nrow(children)))
    }
  }

  if (nrow(children) == 0 && rank == "genus" && "species" %in% child_ranks_to_get) {
    children <- ffb_taxa[ffb_taxa$genus == taxon_name &
                         ffb_taxa$taxonRank == rank_map["species"], ]
    if (verbose && nrow(children) > 0) {
      message(sprintf("  Found %d species via genus column fallback", nrow(children)))
    }
  }

  if (nrow(children) == 0) {
    warning(sprintf("No children found for %s '%s'", rank, taxon_name), call. = FALSE)
    if (rm_flora_database) {
      unlink("flora_download", recursive = TRUE)
    }
    return(data.frame())
  }

  # ============================================================
  # FILTER CHILDREN
  # ============================================================

  # Filter by taxonomic status (accepted vs synonyms)
  if (!include_synonyms) {
    n_before <- nrow(children)
    children <- children[children$taxonomicStatus == "NOME_ACEITO", ]

    if (verbose && nrow(children) < n_before) {
      message(sprintf("  Filtered out %d synonyms", n_before - nrow(children)))
    }
  }

  # ============================================================
  # REMOVE EMPTY ROWS
  # ============================================================

  n_before <- nrow(children)
  rows_with_data <- rowSums(!is.na(children)) > 0
  children <- children[rows_with_data, ]

  if (verbose && nrow(children) < n_before) {
    message(sprintf("  Removed %d completely empty rows", n_before - nrow(children)))
  }

  if (rm_flora_database) {
  unlink("flora_download", recursive = TRUE)
  }

  if (verbose) {
    message(sprintf("\n✓ Returned %d child taxa for %s '%s'",
                    nrow(children), rank, taxon_name))
  }

  return(children)
}
