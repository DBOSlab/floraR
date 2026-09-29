# Auxiliary functions to support main functions
# Author: Domingos Cardoso

#_______________________________________________________________________________
# Function to filter occurrence data ####
.filter_occur_df <- function(occur_df, taxon, state, verbose) {

  temp_occur_df <- data.frame(matrix(ncol = length(names(occur_df)), nrow = 0))
  colnames(temp_occur_df) <- names(occur_df)

  # Filter by taxon only

  if (!is.null(taxon)) {
    if (verbose) {
      message("\nFiltering taxon names... ")
    }

    .check_taxon_match(occur_df, taxon, verbose)

    tf_fam <- grepl("aceae$", taxon)
    if (any(tf_fam)) {
      taxon_fam <- taxon[tf_fam]
      tf <- occur_df$family %in% taxon_fam
      if (any(tf)) {
        occur_df_fam <- occur_df[tf, ]
        temp_occur_df <- occur_df_fam
      }
    }

    tf_gen <- grepl("^[^ ]+$", taxon) & !grepl("aceae$", taxon)
    if (any(tf_gen)) {
      taxon_gen <- taxon[tf_gen]
      tf <- occur_df$genus %in% taxon_gen
      if (any(tf)) {
        occur_df_gen <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_gen)
      }
    }

    tf_spp <- grepl("\\s", taxon)
    if (any(tf_spp)) {
      taxon_spp <- taxon[tf_spp]
      tf <- occur_df$taxonName %in% taxon_spp
      if (any(tf)) {
        occur_df_spp <- occur_df[tf, ]
        temp_occur_df <- rbind(temp_occur_df, occur_df_spp)
      }
    }

    if (nrow(temp_occur_df) != 0){
      occur_df <- temp_occur_df
    }

  }

  # Filter by state only ####

  if (!is.null(state)) {
    if (verbose) {
      message("\nFiltering states... ")
    }

    .check_state_match(occur_df, state, verbose)

    tf <- occur_df$stateProvince %in% state
    if (any(tf)) {
      occur_df <- occur_df[tf, ]
    }
  }

  return(occur_df)
}


#_______________________________________________________________________________
# Function to save csv files ####
.save_csv <- function(df,
                      verbose = TRUE,
                      filename = NULL,
                      dir = dir) {

  # Save the data frame if param save is TRUE
  # Create a new directory to save the results with current date
  # If there is no directory... make one!

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  filename <- paste0(filename, ".csv")
  # Create and save the spreadsheet in .csv format
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  utils::write.csv(df, file = paste0(dir, "/", filename), row.names = FALSE)
}


#_______________________________________________________________________________
# Function to save log.txt file ####
.save_log <- function(df,
                      filename = NULL,
                      dir = dir) {

  log_line <- sprintf("[%s] Downloaded: %s | Records saved to: %s/%s.csv\n",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      nrow(df),
                      dir,
                      filename)

  # Add summary statistics
  count_total <- nrow(df)
  by_family <- utils::capture.output(print(table(df$family)))
  by_genus <- utils::capture.output(print(table(df$genus)))
  by_country <- utils::capture.output(print(table(df$country)))
  by_state <- utils::capture.output(print(table(df$stateProvince)))

  stats_summary <- c(
    sprintf("Total records: %d", count_total),
    "\nRecords per family:", by_family,
    "\nRecords per genus:", by_genus,
    "\nRecords per country:", by_country,
    "\nRecords per stateProvince:", by_state,
    "--------------------------------------------------\n"
  )

  write(c(log_line, stats_summary), file = file.path(dir, "log.txt"), append = TRUE)
}


#_______________________________________________________________________________
# Extract taxon data.frame from the first (most recent) version in a dwca object ####
.flora_get_taxon <- function(dwca) {
  if (!is.list(dwca) || length(dwca) == 0L) {
    stop("'dwca' must be a non-empty named list returned by flora_parse().",
         call. = FALSE)
  }
  taxon_df <- dwca[[1L]][["data"]][["taxon.txt"]]
  if (is.null(taxon_df)) {
    stop("No 'taxon.txt' table found in the dwca object. Run flora_parse() first.",
         call. = FALSE)
  }
  taxon_df
}


#_______________________________________________________________________________
# Safely access a column from a data.frame, returning NA vector if absent ####
.flora_get_col <- function(df, col) {
  if (col %in% colnames(df)) df[[col]] else rep(NA_character_, nrow(df))
}


#_______________________________________________________________________________
# Trim whitespace and collapse internal spaces in a name vector ####
.flora_names_standardize <- function(splist) {
  gsub("\\s+", " ", trimws(splist))
}


#_______________________________________________________________________________
# Parse a standardized name vector into genus / epithet / infra components. ####
# Returns a data.frame with columns: original, genus, epithet,
# infra_rank, infra_epithet, author.
.flora_splist_classify <- function(splist_std) {
  infra_markers <- c("subsp.", "var.", "f.", "fo.", "subvar.", "subf.", "forma")

  rows <- lapply(splist_std, function(name) {
    if (is.na(name) || !nzchar(trimws(name))) {
      return(list(original = NA_character_, genus = NA_character_,
                  epithet = NA_character_, infra_rank = NA_character_,
                  infra_epithet = NA_character_, author = NA_character_))
    }
    parts <- strsplit(name, " ")[[1L]]
    n <- length(parts)
    genus  <- if (n >= 1L) parts[1L] else NA_character_
    epithet <- if (n >= 2L) parts[2L] else NA_character_
    infra_rank <- NA_character_
    infra_epithet <- NA_character_
    author <- NA_character_

    if (n > 2L) {
      mk_rel <- which(parts[3L:n] %in% infra_markers)
      if (length(mk_rel) > 0L) {
        mk <- mk_rel[1L] + 2L     # re-index into full parts
        infra_rank <- parts[mk]
        infra_epithet <- if (mk < n) parts[mk + 1L] else NA_character_
        if (mk + 1L < n) author <- paste(parts[(mk + 2L):n], collapse = " ")
      } else {
        author <- paste(parts[3L:n], collapse = " ")
      }
    }

    list(original = name, genus = genus, epithet = epithet,
         infra_rank = infra_rank, infra_epithet = infra_epithet, author = author)
  })

  as.data.frame(
    do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
}


#_______________________________________________________________________________
# Build a genus -> row-index lookup list for fast genus-restricted searching ####
.flora_build_genus_index <- function(taxon_df) {
  tapply(seq_len(nrow(taxon_df)), taxon_df$genus, identity, simplify = FALSE)
}


#_______________________________________________________________________________
# Compute Levenshtein threshold: integer if max_distance >= 1, ####
# else fraction of name length (minimum 1)
.flora_get_threshold <- function(max_distance, name_len) {
  if (max_distance >= 1) as.integer(max_distance)
  else max(1L, floor(max_distance * name_len))
}


#_______________________________________________________________________________
# Search for a single classified species against the FFB taxon table. ####
# Returns list(rows, exact, distances) or NULL when nothing is within threshold.
# Set return_all = TRUE to get every match; keep_closest = TRUE returns only
# the best distance(s).
.flora_search_ind <- function(sc, taxon_df, genus_index,
                              max_distance, genus_fuzzy,
                              return_all = FALSE, keep_closest = TRUE) {
  if (is.na(sc$epithet)) return(NULL)

  search_name <- paste(sc$genus, sc$epithet)
  if (!is.na(sc$infra_epithet) && nzchar(sc$infra_epithet)) {
    search_name <- paste(search_name, sc$infra_epithet)
  }

  # --- Exact match (fastest path) ---
  exact_pos <- which(taxon_df$taxonName == search_name)
  if (length(exact_pos) > 0L) {
    return(list(rows = exact_pos, exact = TRUE,
                distances = rep(0L, length(exact_pos))))
  }

  # --- Fuzzy match ---
  threshold <- .flora_get_threshold(max_distance, nchar(search_name))

  if (genus_fuzzy) {
    cand_idx <- which(!is.na(taxon_df$taxonName))
    dists <- utils::adist(search_name, taxon_df$taxonName[cand_idx])[1L, ]
    within <- which(dists <= threshold)
    if (length(within) == 0L) return(NULL)
    rows <- cand_idx[within]
    d    <- dists[within]
  } else {
    genus_rows <- genus_index[[sc$genus]]
    if (is.null(genus_rows)) return(NULL)

    ep_search <- if (!is.na(sc$infra_epithet) && nzchar(sc$infra_epithet)) {
      paste(sc$epithet, sc$infra_epithet)
    } else sc$epithet

    cand_ep <- trimws(ifelse(
      !is.na(taxon_df$infraspecificEpithet[genus_rows]),
      paste(taxon_df$specificEpithet[genus_rows],
            taxon_df$infraspecificEpithet[genus_rows]),
      taxon_df$specificEpithet[genus_rows]
    ))
    ep_thresh <- .flora_get_threshold(max_distance, nchar(ep_search))
    dists <- utils::adist(ep_search, cand_ep)[1L, ]
    within <- which(dists <= ep_thresh)
    if (length(within) == 0L) return(NULL)
    rows <- genus_rows[within]
    d <- dists[within]
  }

  if (!return_all || keep_closest) {
    min_d <- min(d)
    keep <- which(d == min_d)
    rows <- rows[keep]
    d <- d[keep]
  }

  list(rows = rows, exact = FALSE, distances = d)
}


#_______________________________________________________________________________
# Resolve a single taxon row index to its accepted name and ID. ####
# id_lookup is a list mapping FFB id strings to row positions in taxon_df.
.flora_resolve_accepted <- function(row_idx, taxon_df, id_lookup) {
  status <- taxon_df$taxonomicStatus[row_idx]

  if (!is.na(status) && status == "SINONIMO" &&
      !is.na(taxon_df$acceptedNameUsageID[row_idx])) {
    acc_id <- as.character(taxon_df$acceptedNameUsageID[row_idx])

    # Verificar se o acc_id existe no id_lookup
    acc_pos <- id_lookup[[acc_id]]

    if (!is.null(acc_pos) && length(acc_pos) > 0) {
      # Verificar se acc_pos[1] existe no taxon_df
      if (acc_pos[1] <= nrow(taxon_df)) {
        return(list(id = taxon_df$id[acc_pos[1]],
                    name = taxon_df$taxonName[acc_pos[1]]))
      }
    }

    warning(paste("Accepted ID", acc_id, "not found in id_lookup for row", row_idx),
            call. = FALSE)
    return(list(id = NA_character_, name = NA_character_))

  } else if (!is.na(status) && status == "NOME_ACEITO") {
    return(list(id = taxon_df$id[row_idx],
                name = taxon_df$taxonName[row_idx]))
  }

  list(id = NA_character_, name = NA_character_)
}


#_______________________________________________________________________________
# Build an NA-filled result row for an unmatched input name ####
.flora_na_row <- function(spname, include_correct = FALSE) {
  df <- data.frame(
    Search = spname,
    FFB.taxon.ID = NA_character_,
    taxonRank = NA_character_,
    Input.InfraspecificEpithet = NA_character_,
    scientificNameAuthorship = NA_character_,
    taxonomicStatus = NA_character_,
    Accepted.taxon.ID = NA_character_,
    Accepted.taxon.Name = NA_character_,
    family = NA_character_,
    order = NA_character_,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (include_correct) {
    df$Correct.Spelling <- NA
    df <- df[, c("Search", "Correct.Spelling",
                 names(df)[!names(df) %in% c("Search", "Correct.Spelling")])]
  }

  df
}

#_______________________________________________________________________________
# Build result row(s) from one or more matched row indices in taxon_df. ####
.flora_build_rows <- function(spname, rows, dists, taxon_df, id_lookup,
                              include_distance = FALSE,
                              include_correct = FALSE) {

  acc_list <- tryCatch({
    lapply(rows, .flora_resolve_accepted,
           taxon_df = taxon_df, id_lookup = id_lookup)
  }, error = function(e) {
    warning(paste("Error resolving accepted name for", spname, ":", e$message),
            call. = FALSE)
    lapply(rows, function(x) list(id = NA_character_, name = NA_character_))
  })

  df <- data.frame(
    Search = spname,
    FFB.taxon.ID = taxon_df$id[rows],
    taxonRank = taxon_df$taxonRank[rows],
    Input.InfraspecificEpithet = .flora_get_col(taxon_df, "infraspecificEpithet")[rows],
    scientificNameAuthorship = .flora_get_col(taxon_df, "scientificNameAuthorship")[rows],
    taxonomicStatus = taxon_df$taxonomicStatus[rows],
    Accepted.taxon.ID = vapply(acc_list, function(x) {
      val <- x$id
      if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
    }, character(1)),
    Accepted.taxon.Name = vapply(acc_list, `[[`, character(1), "name"),
    family = taxon_df$family[rows],
    order = .flora_get_col(taxon_df, "order")[rows],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (include_distance) df$Name.Distance <- dists
  if (include_correct) {
    df$Correct.Spelling <- NA
    df <- df[, c("Search", "Correct.Spelling",
                 names(df)[!names(df) %in% c("Search", "Correct.Spelling")])]
  }

  df
}


#_______________________________________________________________________________
# Flag duplicated Accepted.taxon.Name entries in a result data.frame. ####
# Returns an integer vector: position of the first other occurrence, or NA.
.flora_find_dups <- function(result) {
  nms <- result$Accepted.taxon.Name
  dups <- rep(NA_integer_, nrow(result))
  for (i in seq_len(nrow(result))) {
    nm <- nms[i]
    if (!is.na(nm)) {
      hits <- which(nms == nm)
      if (length(hits) > 1L) dups[i] <- hits[hits != i][1L]
    }
  }
  dups
}


#_______________________________________________________________________________
# Download (if needed) and parse the FFB dataset, returning a list with the ####
# three pre-built structures shared by the search functions.
.flora_prepare_taxon <- function(version, verbose, rm_flora_database) {
  floraR::flora_download(version = version, dir = "flora_download", verbose = verbose)
  dwca <- floraR::flora_parse(path = "flora_download", version = version, verbose = verbose)
  taxon_df <- .flora_get_taxon(dwca)

  id_columns <- c("id", "acceptedNameUsageID", "parentNameUsageID", "originalNameUsageID")

  all_rows <- c()
  all_ids <- c()
  for (col in id_columns) {
    col_data <- taxon_df[[col]]
    valid_idx <- which(!is.na(col_data) & col_data != "")

    if (length(valid_idx) > 0) {
      all_rows <- c(all_rows, valid_idx)
      all_ids <- c(all_ids, as.character(col_data[valid_idx]))
    }
  }

  id_lookup <- tapply(all_rows, all_ids, unique, simplify = FALSE)

  # Remove the downloaded FFB folder flora_download
  if (rm_flora_database) {
    unlink("flora_download", recursive = TRUE)
  }

  list(
    taxon_df = taxon_df,
    genus_index = .flora_build_genus_index(taxon_df),
    id_lookup = id_lookup
  )
}


#_______________________________________________________________________________
# Core search loop shared by flora_search() and flora_match(). ####
.flora_search_impl <- function(splist, taxon_df, genus_index, id_lookup,
                               max_distance, genus_fuzzy,
                               show_correct, progress_bar) {

  splist_std <- .flora_names_standardize(splist)
  splist_class <- .flora_splist_classify(splist_std)

  n_sps <- length(splist)
  results <- vector("list", n_sps)
  homonyms <- logical(n_sps)
  is_exact <- logical(n_sps)

  if (progress_bar) pb <- utils::txtProgressBar(min = 0, max = n_sps, style = 3)

  for (i in seq_len(n_sps)) {
    sc <- splist_class[i, ]

    if (is.na(sc$epithet)) {
      warning(paste0("'", splist[i],
                     "' does not include an epithet and will be skipped."),
              call. = FALSE)
      results[[i]] <- .flora_na_row(splist[i], include_correct = show_correct)
      if (progress_bar) utils::setTxtProgressBar(pb, i)
      next
    }

    match_res <- .flora_search_ind(sc, taxon_df, genus_index,
                                   max_distance, genus_fuzzy)

    if (is.null(match_res)) {
      warning(paste0("No match found for '", splist[i], "'."), call. = FALSE)
      results[[i]] <- .flora_na_row(splist[i], include_correct = show_correct)
      if (progress_bar) utils::setTxtProgressBar(pb, i)
      next
    }

    is_exact[i] <- match_res$exact
    rows <- match_res$rows
    dists <- match_res$distances

    if (length(rows) > 1L) {
      homonyms[i] <- TRUE
      acc_idx <- rows[taxon_df$taxonomicStatus[rows] == "NOME_ACEITO"]
      chosen <- if (length(acc_idx) > 0L) acc_idx[1L] else rows[1L]
    } else {
      chosen <- rows[1L]
    }

    d_chosen <- dists[match(chosen, rows)]
    results[[i]] <- .flora_build_rows(spname = splist[i],
                                      rows = chosen,
                                      dists = d_chosen,
                                      taxon_df,
                                      id_lookup,
                                      include_distance = FALSE,
                                      include_correct = show_correct)
    if (progress_bar) utils::setTxtProgressBar(pb, i)
  }

  if (progress_bar) close(pb)

  # Verify whether all lines have the same column
  result_final <- do.call(rbind, results)
  rownames(result_final) <- NULL

  if (all(is.na(result_final$FFB.taxon.ID))) {
    warning("No match found for any input name. Try increasing 'max_distance'.")
    return(NULL)
  }

  if (show_correct) {
    result_final$Correct.Spelling <- is_exact
    result_final <- result_final[, c("Search", "Correct.Spelling",
                                     names(result_final)[!names(result_final) %in% c("Search", "Correct.Spelling")])]
  }

  if (any(homonyms)) {
    warning(paste0(
      "More than one name was matched for some inputs. ",
      "Only the first accepted name was returned. "),
      call. = FALSE)
    attr(result_final, "matched_mult") <- splist[homonyms]
  }

  result_final
}


#_______________________________________________________________________________
# Function to save xlsx spreadsheets (used by the *_gap() functions, which return a
# richer, multi-source table better suited to a proper spreadsheet than a
# plain CSV) ####
.save_xlsx <- function(df,
                       verbose = TRUE,
                       filename = NULL,
                       dir = dir) {

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  filename <- paste0(filename, ".xlsx")
  if (verbose) {
    message(paste0("Writing spreadsheet '",
                   filename, "' within '",
                   dir, "' folder on disk."))
  }
  openxlsx::write.xlsx(df, file = paste0(dir, "/", filename))
}


#_______________________________________________________________________________
# The 26 Brazilian states plus the Federal District, used by
# .location_mentions_brazil() to recognise a Brazilian state named in POWO's
# free-text range (e.g. "Brazil (Bahia)") ####
.br_states <- c("Acre", "Alagoas", "Amap\u00e1", "Amazonas", "Bahia", "Cear\u00e1",
                "Distrito Federal", "Esp\u00edrito Santo", "Goi\u00e1s",
                "Maranh\u00e3o", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais",
                "Par\u00e1", "Para\u00edba", "Paran\u00e1", "Pernambuco", "Piau\u00ed",
                "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul",
                "Rond\u00f4nia", "Roraima", "Santa Catarina", "S\u00e3o Paulo",
                "Sergipe", "Tocantins")


#_______________________________________________________________________________
# The five TDWG (WGSRPD) level-3 botanical areas that make up Brazil - the
# areas POWO/WCVP use to map a species' distribution ####
.tdwg_brazil <- c(BZC = "Brazil West-Central", BZE = "Brazil Northeast",
                  BZL = "Brazil Southeast", BZN = "Brazil North",
                  BZS = "Brazil South")


#_______________________________________________________________________________
# Whether a free-text range string mentions Brazil - either the country name
# itself, or any Brazilian state. Diacritics are stripped on both sides so
# e.g. "Sao Paulo" still matches. Used only as a fallback Brazil-evidence
# signal (POWO's own range text) when no WCVP distribution is available. ####
.location_mentions_brazil <- function(location) {
  if (is.null(location) || is.na(location) || !nzchar(location)) return(FALSE)

  loc_ascii <- stringi::stri_trans_general(location, "Latin-ASCII")
  if (grepl("\\bbrazil\\b|\\bbrasil\\b", loc_ascii, ignore.case = TRUE)) return(TRUE)

  # Word-boundary match is essential: an unanchored match would, for example,
  # wrongly flag "Paraguay" because it contains "Para".
  states_ascii <- stringi::stri_trans_general(.br_states, "Latin-ASCII")
  any(vapply(states_ascii, function(st) grepl(paste0("\\b", st, "\\b"), loc_ascii,
                                              ignore.case = TRUE),
             logical(1)))
}


#_______________________________________________________________________________
# Names currently registered in FFB, for the *_gap() comparisons: 'genus'
# holds every species-level name (accepted and synonym) filed under 'taxon';
# 'all' holds every name in FFB at any rank, so that a candidate whose
# accepted name sits in a different genus (e.g. POWO accepts a Myrcia
# synonym as Eugenia florida) is still recognised as already in FFB. The
# dataset is downloaded (or reused from the local cache) and parsed once. ####
.ffb_names <- function(taxon, version = "latest") {
  flora_download(version = version, dir = "flora_download", verbose = FALSE)
  dwca <- flora_parse(path = "flora_download", version = version, verbose = FALSE)
  tx <- .flora_get_taxon(dwca)
  list(genus = unique(tx$taxonName[tx$genus %in% taxon & tx$taxonRank %in% "ESPECIE"]),
       all = unique(tx$taxonName[!is.na(tx$taxonName)]))
}


#_______________________________________________________________________________
# Return the World Checklist of Vascular Plants (WCVP) names and distribution
# tables from the 'rWCVPdata' package. WCVP is the checklist behind Plants of
# the World Online (POWO): POWO's names, synonymy and distribution maps all
# come from it. Reading the whole checklist locally, rather than querying
# POWO name by name (its API also blocks non-browser clients), lets
# flora_species_gap() resolve every name and distribution of a genus in a single
# in-memory join. 'rWCVPdata' is not on CRAN, so it is a suggested package
# with installation instructions given here. ####
.wcvp_tables <- function() {
  if (!requireNamespace("rWCVPdata", quietly = TRUE)) {
    stop("Package 'rWCVPdata' is required to compare FFB against POWO/WCVP. ",
         "Install it with:\n",
         "  install.packages(\"rWCVPdata\", repos = c(",
         "\"https://matildabrown.github.io/drat\", \"https://cloud.r-project.org\"))",
         call. = FALSE)
  }
  list(names = rWCVPdata::wcvp_names,
       distributions = rWCVPdata::wcvp_distributions,
       version = tryCatch(as.character(rWCVPdata::wcvp_version()),
                          error = function(e) NA_character_))
}


#_______________________________________________________________________________
# List every species-level WCVP/POWO name in a genus (accepted names,
# synonyms and other statuses, except misapplied names, which are not names
# of the taxon itself), joined to its accepted name and to the accepted
# name's TDWG level-3 distribution. Returns one row per name with the
# columns flora_species_gap() needs. ####
.wcvp_genus_names <- function(taxon, tables) {
  wn <- tables$names
  wd <- tables$distributions

  empty <- data.frame(POWO_ID = character(0), IPNI_ID = character(0),
                      Taxon_name = character(0), Authors = character(0),
                      SpecificEpithet = character(0), Year = character(0),
                      Taxonomic_status = character(0), Accepted_name = character(0),
                      Accepted_authors = character(0), POWO_Range = character(0),
                      POWO_Distribution = character(0), POWO_Brazil_Areas = character(0),
                      Has_distribution = logical(0), POWO_Published_in = character(0),
                      stringsAsFactors = FALSE)

  sp <- wn[wn$genus %in% taxon & wn$taxon_rank %in% "Species" &
             !wn$taxon_status %in% "Misapplied", ]
  if (nrow(sp) == 0) return(empty)
  sp <- as.data.frame(sp, stringsAsFactors = FALSE)

  acc <- wn[match(sp$accepted_plant_name_id, wn$plant_name_id), ]

  # Distribution of each accepted name, as "Area; Area (introduced); ..."
  dist <- as.data.frame(wd[wd$plant_name_id %in% sp$accepted_plant_name_id, ],
                        stringsAsFactors = FALSE)
  flag <- ifelse(dist$location_doubtful %in% 1, " (doubtful)",
                 ifelse(dist$extinct %in% 1, " (extinct)",
                        ifelse(dist$introduced %in% 1, " (introduced)", "")))
  dist$label <- paste0(dist$area, flag)
  collapse <- function(ids, keep) {
    if (!any(keep)) return(stats::setNames(character(0), character(0)))
    tapply(dist$label[keep], dist$plant_name_id[keep],
           function(x) paste(sort(unique(x)), collapse = "; "))
  }
  all_areas <- collapse(dist$plant_name_id, rep(TRUE, nrow(dist)))
  br_areas <- collapse(dist$plant_name_id,
                       dist$area_code_l3 %in% names(.tdwg_brazil) &
                         !dist$location_doubtful %in% 1)

  key <- as.character(sp$accepted_plant_name_id)
  na_chr <- function(x) { x <- trimws(x); x[!nzchar(x)] <- NA_character_; x }

  published <- na_chr(paste(ifelse(is.na(sp$place_of_publication), "", sp$place_of_publication),
                            ifelse(is.na(sp$volume_and_page), "", trimws(sp$volume_and_page)),
                            ifelse(is.na(sp$first_published), "", sp$first_published)))

  result <- data.frame(
    POWO_ID = sp$powo_id,
    IPNI_ID = sp$ipni_id,
    Taxon_name = sp$taxon_name,
    Authors = sp$taxon_authors,
    SpecificEpithet = sp$species,
    Year = gsub("\\D", "", sp$first_published),
    Taxonomic_status = sp$taxon_status,
    Accepted_name = acc$taxon_name,
    Accepted_authors = acc$taxon_authors,
    POWO_Range = ifelse(is.na(acc$geographic_area), sp$geographic_area, acc$geographic_area),
    POWO_Distribution = unname(all_areas[key]),
    POWO_Brazil_Areas = unname(br_areas[key]),
    Has_distribution = key %in% names(all_areas),
    POWO_Published_in = gsub("\\s+", " ", published),
    stringsAsFactors = FALSE
  )
  result$Year[!nzchar(result$Year)] <- NA_character_
  rownames(result) <- NULL
  result
}


#_______________________________________________________________________________
# GET a JSON web API URL and parse the response. Used for GBIF
# (flora_species_gap(), flora_distribution_gap()) and ChecklistBank
# (flora_species_gap(powo_source = "checklistbank")).
# POWO's own API
# (powo.science.kew.org/api) sits behind a Cloudflare browser challenge that
# blocks non-browser clients, but ChecklistBank (api.checklistbank.org) hosts
# both POWO's own weekly export (dataset 2000) and WCVP (dataset 2232), the
# checklist behind POWO's distribution maps. Never errors - returns NULL on
# any failure. ####
.get_json <- function(url) {
  tryCatch({
    con <- url(url, headers = c(Accept = "application/json"))
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    txt <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) NULL)
}


#_______________________________________________________________________________
# List every species-level name POWO holds for a genus (accepted names and
# synonyms), from POWO's own weekly export on ChecklistBank (dataset 2000).
# The full-text search is paginated (ChecklistBank caps a page at 1000) and
# then restricted to names whose genus part is exactly 'taxon', since the
# search also matches e.g. epithets containing the genus name. Returns the
# same columns as .wcvp_genus_names() minus the distribution/publication
# ones, which .powo_api_enrich() adds for the FFB-missing candidates only.
# Never errors - a failed request returns an empty (but correctly shaped)
# data.frame. ####
.powo_name_search <- function(taxon, max_number = 5000) {

  empty <- data.frame(POWO_ID = character(0), IPNI_ID = character(0),
                      Taxon_name = character(0), Authors = character(0),
                      SpecificEpithet = character(0), Year = character(0),
                      Taxonomic_status = character(0), Accepted_name = character(0),
                      Accepted_authors = character(0), Accepted_ID = character(0),
                      POWO_Range = character(0), Published_in_ID = character(0),
                      stringsAsFactors = FALSE)

  base <- "https://api.checklistbank.org/dataset/2000/nameusage/search"
  page_size <- min(1000, max_number)
  offset <- 0
  hits <- list()

  repeat {
    url <- sprintf("%s?q=%s&rank=species&limit=%d&offset=%d", base,
                   utils::URLencode(taxon, reserved = TRUE), page_size, offset)
    res <- .get_json(url)
    if (is.null(res) || length(res$result) == 0) break
    hits <- c(hits, res$result)
    offset <- offset + page_size
    if (isTRUE(res$last) || offset >= max_number) break
  }

  if (length(hits) == 0) return(empty)

  val <- function(x) {
    if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) {
      NA_character_
    } else {
      as.character(x)
    }
  }
  status <- function(x) {
    x <- val(x)
    if (is.na(x)) x else paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }

  result <- do.call(rbind, lapply(hits, function(h) {
    u <- h$usage
    acc <- u$accepted
    id <- sub("^.*names:", "", val(u$id))
    data.frame(
      POWO_ID = id,
      IPNI_ID = id,
      Taxon_name = val(u$name$scientificName),
      Authors = val(u$name$authorship),
      Genus = val(u$name$genus),
      SpecificEpithet = val(u$name$specificEpithet),
      Year = val(u$name$publishedInYear),
      Taxonomic_status = status(u$status),
      Accepted_name = if (is.null(acc)) val(u$name$scientificName) else val(acc$name$scientificName),
      Accepted_authors = if (is.null(acc)) val(u$name$authorship) else val(acc$name$authorship),
      Accepted_ID = if (is.null(acc)) val(u$id) else val(acc$id),
      POWO_Range = val(u$remarks),
      Published_in_ID = val(u$name$publishedInId),
      stringsAsFactors = FALSE
    )
  }))

  result <- result[result$Genus %in% taxon & !is.na(result$Taxon_name), ]
  result <- result[!duplicated(result$POWO_ID), names(empty)]
  rownames(result) <- NULL
  result
}


#_______________________________________________________________________________
# Look up the WCVP distribution (TDWG level-3 areas - the areas POWO maps) of
# one accepted species, from WCVP on ChecklistBank (dataset 2232). The record
# is matched by IPNI ID where possible, falling back to an exact name match.
# Returns list(found, areas, brazil) where 'areas' is every area name
# (suffixed "(introduced)" where applicable) and 'brazil' only the Brazilian
# ones. Never errors - 'found' is FALSE when the name or its distribution
# could not be retrieved. ####
.wcvp_distribution <- function(accepted_name, ipni_id = NA_character_) {
  empty <- list(found = FALSE, areas = NA_character_, brazil = NA_character_)
  if (is.null(accepted_name) || is.na(accepted_name) || !nzchar(accepted_name)) {
    return(empty)
  }

  base <- "https://api.checklistbank.org/dataset/2232"
  res <- .get_json(sprintf("%s/nameusage/search?q=%s&rank=species&limit=20", base,
                               utils::URLencode(accepted_name, reserved = TRUE)))
  if (is.null(res) || length(res$result) == 0) return(empty)

  hits <- Filter(function(h) identical(h$usage$status, "accepted"), res$result)
  ipni_num <- sub("^.*names:", "", ipni_id)
  by_ipni <- Filter(function(h) paste0("ipni:", ipni_num) %in% unlist(h$usage$name$identifier),
                    hits)
  by_name <- Filter(function(h) identical(h$usage$name$scientificName, accepted_name), hits)
  hit <- if (length(by_ipni) > 0) by_ipni[[1]] else if (length(by_name) > 0) by_name[[1]] else NULL
  if (is.null(hit)) return(empty)

  dist <- .get_json(sprintf("%s/taxon/%s/distribution", base,
                                utils::URLencode(hit$id, reserved = TRUE)))
  if (is.null(dist)) return(empty)
  if (length(dist) == 0) return(list(found = TRUE, areas = NA_character_, brazil = NA_character_))

  field <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  codes <- vapply(dist, function(d) field(d$area$id), character(1))
  area_names <- vapply(dist, function(d) field(d$area$name), character(1))
  intro <- vapply(dist, function(d) identical(tolower(field(d$establishmentMeans)), "introduced"),
                  logical(1))
  labels <- ifelse(intro, paste0(area_names, " (introduced)"), area_names)

  is_br <- codes %in% names(.tdwg_brazil)
  list(found = TRUE,
       areas = paste(sort(labels), collapse = "; "),
       brazil = if (any(is_br)) paste(sort(labels[is_br]), collapse = "; ") else NA_character_)
}


#_______________________________________________________________________________
# Fetch the protologue citation of a name (e.g. "Arch. Jard. Bot. Rio de
# Janeiro 6: 32 (1933)") from its POWO reference record on ChecklistBank
# (dataset 2000). Never errors - returns NA on failure. ####
.powo_publication <- function(published_in_id) {
  if (is.null(published_in_id) || is.na(published_in_id) || !nzchar(published_in_id)) {
    return(NA_character_)
  }
  res <- .get_json(paste0("https://api.checklistbank.org/dataset/2000/reference/",
                              utils::URLencode(published_in_id, reserved = TRUE)))
  if (is.null(res) || is.null(res$citation) || !nzchar(res$citation)) NA_character_ else res$citation
}


#_______________________________________________________________________________
# Add the distribution and publication columns that .wcvp_genus_names()
# provides to names retrieved with .powo_name_search() - one distribution
# request per distinct accepted name (synonyms share it), plus, when
# fetch_details is TRUE, one publication request per name. Only called on
# the FFB-missing candidates, to keep the number of requests small. ####
.powo_api_enrich <- function(missing, fetch_details = TRUE, verbose = TRUE) {
  acc_keys <- unique(missing[, c("Accepted_name", "Accepted_ID")])
  dist <- vector("list", nrow(acc_keys))
  for (i in seq_len(nrow(acc_keys))) {
    if (verbose) {
      message(sprintf("  Fetching POWO distribution %d/%d: %s",
                      i, nrow(acc_keys), acc_keys$Accepted_name[i]))
    }
    dist[[i]] <- .wcvp_distribution(acc_keys$Accepted_name[i], acc_keys$Accepted_ID[i])
  }
  idx <- match(paste(missing$Accepted_name, missing$Accepted_ID),
               paste(acc_keys$Accepted_name, acc_keys$Accepted_ID))

  missing$POWO_Distribution <- vapply(idx, function(j) dist[[j]]$areas, character(1))
  missing$POWO_Brazil_Areas <- vapply(idx, function(j) dist[[j]]$brazil, character(1))
  missing$Has_distribution <- vapply(idx, function(j) isTRUE(dist[[j]]$found) &&
                                       !is.na(dist[[j]]$areas), logical(1))

  missing$POWO_Published_in <- NA_character_
  if (fetch_details) {
    for (i in seq_len(nrow(missing))) {
      if (verbose) {
        message(sprintf("  Fetching POWO publication details %d/%d: %s",
                        i, nrow(missing), missing$Taxon_name[i]))
      }
      missing$POWO_Published_in[i] <- .powo_publication(missing$Published_in_ID[i])
    }
  }
  missing
}


#_______________________________________________________________________________
# Comparison key for scientific names across databases: case, diacritics,
# hyphens, "y"/"i", and the genitive "-ii"/"-i" ending are ignored, since
# these are orthographic variants of one name that databases spell
# differently (e.g. FFB's "Luetzelburgia andradelimae" vs POWO's
# "Luetzelburgia andrade-limae", "freire-allemani" vs "freire-allemanii", or
# POWO's "Myrcia aegiphylloides" vs GBIF's "Myrcia aegyphylloides"). Used
# only for matching; output tables keep each database's own spelling. ####
.name_key <- function(x) {
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("-", "", x, fixed = TRUE)
  x <- gsub("y", "i", x, fixed = TRUE)
  x <- gsub("ii\\b", "i", x)
  gsub("\\s+", " ", trimws(x))
}


#_______________________________________________________________________________
# GBIF type-status values (GBIF's current vocabulary) counted as evidence
# that a name's type material was collected in Brazil. Excludes "NotAType"
# and non-type categories such as topotypes or plesiotypes. ####
.gbif_type_status <- c("Type", "Holotype", "Isotype", "Lectotype", "Isolectotype",
                       "Syntype", "Isosyntype", "Neotype", "Isoneotype", "Epitype",
                       "Isoepitype", "Paratype", "Isoparatype", "Paralectotype",
                       "OriginalMaterial", "TypeSeries")


#_______________________________________________________________________________
# Resolve a genus to its GBIF backbone key. Genus names are often shared
# across kingdoms (e.g. Gracilaria is both a red alga and a moth), so only
# the GBIF kingdoms holding plants and algae are searched, in turn: Plantae
# (land plants, red and green algae), Chromista (brown algae, diatoms), and
# Protozoa (euglenids). Fungi and animals are never searched. Returns
# list(key, name, kingdom) or NULL when no exact genus match is found. ####
.gbif_kingdoms <- c("Plantae", "Chromista", "Protozoa")

.gbif_genus_key <- function(taxon, rank = "GENUS") {
  doubtful <- NULL
  foreign_accepted <- FALSE
  for (k in .gbif_kingdoms) {
    res <- .get_json(sprintf("https://api.gbif.org/v1/species/match?name=%s%s&kingdom=%s&strict=true",
                             utils::URLencode(taxon, reserved = TRUE),
                             if (is.null(rank)) "" else paste0("&rank=", rank),
                             utils::URLencode(k, reserved = TRUE)))
    if (is.null(res) || !identical(res$matchType, "EXACT") ||
        (!is.null(rank) && !identical(res$rank, rank))) next
    if (!identical(res$kingdom, k)) {
      # GBIF fell back to a genus of this name in another kingdom (e.g. Fungi)
      if (identical(res$status, "ACCEPTED")) foreign_accepted <- TRUE
      next
    }
    hit <- list(key = res$usageKey, name = res$scientificName, kingdom = res$kingdom)
    if (!identical(res$status, "DOUBTFUL")) return(hit)
    if (is.null(doubtful)) doubtful <- hit
  }
  # A DOUBTFUL plant/algal entry is only used when no accepted genus of that
  # name exists elsewhere: GBIF holds spurious doubtful placeholders, such as
  # an "Agaricus" in Plantae alongside the accepted fungal genus Agaricus.
  if (!is.null(doubtful) && !foreign_accepted) doubtful else NULL
}


#_______________________________________________________________________________
# Every GBIF name within a genus that has type specimens collected in Brazil,
# with the number of such specimens - one faceted occurrence request for the
# whole genus, however many specimens it has. Returns a data.frame
# (GBIF_Key, N_type_specimens); NULL when the request fails. ####
.gbif_brazil_type_counts <- function(genus_key, type_status = .gbif_type_status) {
  url <- sprintf(paste0("https://api.gbif.org/v1/occurrence/search?taxonKey=%s&country=BR%s",
                        "&limit=0&facet=taxonKey&facetLimit=100000"),
                 genus_key, paste0("&typeStatus=", type_status, collapse = ""))
  res <- .get_json(url)
  if (is.null(res)) return(NULL)
  counts <- if (length(res$facets) > 0) res$facets[[1]]$counts else list()
  data.frame(GBIF_Key = vapply(counts, function(x) as.character(x$name), character(1)),
             N_type_specimens = vapply(counts, function(x) as.integer(x$count), integer(1)),
             stringsAsFactors = FALSE)
}


#_______________________________________________________________________________
# Every name in the GBIF backbone within a genus (accepted names and
# synonyms, all ranks), paginated 1000 at a time, with its accepted species.
# Used to turn the numeric keys of .gbif_brazil_type_counts() into names.
# Never errors - returns what could be retrieved. ####
.gbif_genus_names <- function(genus_key) {
  backbone <- "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  val <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  rows <- list()
  offset <- 0
  repeat {
    res <- .get_json(sprintf("https://api.gbif.org/v1/species/search?highertaxonKey=%s&datasetKey=%s&limit=1000&offset=%d",
                             genus_key, backbone, offset))
    if (is.null(res) || length(res$results) == 0) break
    rows <- c(rows, lapply(res$results, function(r) {
      data.frame(GBIF_Key = val(r$key),
                 Taxon_name = val(r$canonicalName),
                 Authors = val(r$authorship),
                 Rank = val(r$rank),
                 Taxonomic_status = val(r$taxonomicStatus),
                 Accepted_name = val(r$species),
                 Accepted_full_name = if (is.null(r$accepted)) val(r$scientificName) else val(r$accepted),
                 Genus = val(r$genus),
                 GBIF_Published_in = val(r$publishedIn),
                 stringsAsFactors = FALSE)
    }))
    offset <- offset + 1000
    if (isTRUE(res$endOfRecords)) break
  }
  if (length(rows) == 0) {
    return(data.frame(GBIF_Key = character(0), Taxon_name = character(0),
                      Authors = character(0), Rank = character(0),
                      Taxonomic_status = character(0), Accepted_name = character(0),
                      Accepted_full_name = character(0), Genus = character(0),
                      GBIF_Published_in = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out[!duplicated(out$GBIF_Key), ]
}


#_______________________________________________________________________________
# The Brazilian type specimens of a set of GBIF name keys, fetched many keys
# per request (GBIF accepts repeated taxonKey parameters) rather than one
# request per name. At most 'max_per_batch' specimens are read per batch of
# keys - enough to summarize each name's types, since the total counts come
# from .gbif_brazil_type_counts(). A query for a key also returns the types
# of that name's synonyms (filed under their own keys), so each specimen
# keeps its own, accepted, and species keys for flora_species_gap() to assign it to
# the requested name. Returns one row per specimen. ####
.gbif_type_specimens <- function(keys, type_status = .gbif_type_status,
                                 batch_size = 10, max_per_batch = 900) {
  val <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  batches <- split(keys, ceiling(seq_along(keys) / batch_size))
  rows <- list()
  for (b in batches) {
    offset <- 0
    repeat {
      url <- sprintf("https://api.gbif.org/v1/occurrence/search?country=BR%s%s&limit=300&offset=%d",
                     paste0("&taxonKey=", b, collapse = ""),
                     paste0("&typeStatus=", type_status, collapse = ""), offset)
      res <- .get_json(url)
      if (is.null(res) || length(res$results) == 0) break
      rows <- c(rows, lapply(res$results, function(r) {
        data.frame(GBIF_Key = val(r$taxonKey),
                   Accepted_key = val(r$acceptedTaxonKey),
                   Species_key = val(r$speciesKey),
                   Specimen_name = val(r$scientificName),
                   Occurrence_ID = val(r$key),
                   Type_status = val(paste(unlist(r$typeStatus), collapse = ", ")),
                   Typified_name = val(r$typifiedName),
                   Institution = val(r$institutionCode),
                   Catalog_number = val(r$catalogNumber),
                   Collector = val(r$recordedBy),
                   Year = val(r$year),
                   State = val(r$stateProvince),
                   stringsAsFactors = FALSE)
      }))
      offset <- offset + 300
      if (isTRUE(res$endOfRecords) || offset >= max_per_batch) break
    }
  }
  if (length(rows) == 0) {
    return(data.frame(GBIF_Key = character(0), Accepted_key = character(0),
                      Species_key = character(0), Specimen_name = character(0),
                      Occurrence_ID = character(0),
                      Type_status = character(0), Typified_name = character(0),
                      Institution = character(0), Catalog_number = character(0),
                      Collector = character(0), Year = character(0), State = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}


#_______________________________________________________________________________
# Normalise free-text Brazilian state values, as found in GBIF and other
# occurrence databases, to FFB's full state names. Handles case, missing
# diacritics, two-letter acronyms in any case ("Go", "df"), "Est./Estado
# do/da/de ..." and "State of ..." prefixes, and "Brasilia" for the Federal
# District. Values that are not a Brazilian state are returned as NA. ####
.normalize_br_state <- function(x) {
  states <- c("Acre" = "AC", "Alagoas" = "AL", "Amap\u00e1" = "AP", "Amazonas" = "AM",
              "Bahia" = "BA", "Cear\u00e1" = "CE", "Distrito Federal" = "DF",
              "Esp\u00edrito Santo" = "ES", "Goi\u00e1s" = "GO", "Maranh\u00e3o" = "MA",
              "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
              "Par\u00e1" = "PA", "Para\u00edba" = "PB", "Paran\u00e1" = "PR", "Pernambuco" = "PE",
              "Piau\u00ed" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
              "Rio Grande do Sul" = "RS", "Rond\u00f4nia" = "RO", "Roraima" = "RR",
              "Santa Catarina" = "SC", "S\u00e3o Paulo" = "SP", "Sergipe" = "SE",
              "Tocantins" = "TO")
  key <- function(s) {
    s <- tolower(stringi::stri_trans_general(s, "Latin-ASCII"))
    s <- gsub("^\\s*(est\\.?|estado|state)\\s+(do|da|de|of)\\s+", "", s)
    s <- gsub("\\s+state\\s*$", "", s)
    gsub("\\s+", " ", trimws(s))
  }
  lookup <- c(stats::setNames(names(states), key(names(states))),
              stats::setNames(names(states), tolower(states)),
              "brasilia" = "Distrito Federal")
  out <- unname(lookup[key(x)])
  out[is.na(x)] <- NA_character_
  out
}


#_______________________________________________________________________________
# Resolve a plant or algal name (genus, species, or infraspecific) to its GBIF
# backbone key, searching only the kingdoms holding plants and algae - see
# .gbif_genus_key(), whose rules this follows. Returns list(key, name,
# kingdom) or NULL. ####
.gbif_plant_key <- function(taxon) {
  n_words <- length(strsplit(trimws(taxon), "\\s+")[[1]])
  rank <- if (n_words == 1) "GENUS" else if (n_words == 2) "SPECIES" else NULL
  .gbif_genus_key(taxon, rank = rank)
}


#_______________________________________________________________________________
# Brazilian occurrence records of a GBIF taxon (including its synonyms and
# infraspecific taxa), counted by state in a single faceted request. GBIF's
# free-text state values are normalised to FFB state names, keeping the raw
# values of each state so that its individual records can be fetched later.
# Returns list(n_records, state_counts = data.frame(State, GBIF_records),
# raw_states = named list State -> raw values), or NULL on failure. ####
.gbif_brazil_state_counts <- function(taxon_key) {
  res <- .get_json(sprintf(paste0("https://api.gbif.org/v1/occurrence/search?taxonKey=%s",
                                  "&country=BR&limit=0&facet=stateProvince&facetLimit=500"),
                           taxon_key))
  if (is.null(res)) return(NULL)
  counts <- if (length(res$facets) > 0) res$facets[[1]]$counts else list()
  raw <- vapply(counts, function(x) as.character(x$name), character(1))
  n <- vapply(counts, function(x) as.integer(x$count), integer(1))
  state <- .normalize_br_state(raw)
  keep <- !is.na(state)
  if (!any(keep)) {
    return(list(n_records = as.integer(res$count),
                state_counts = data.frame(State = character(0), GBIF_records = integer(0),
                                          stringsAsFactors = FALSE),
                raw_states = list()))
  }
  totals <- tapply(n[keep], state[keep], sum)
  list(n_records = as.integer(res$count),
       state_counts = data.frame(State = names(totals), GBIF_records = as.integer(totals),
                                 stringsAsFactors = FALSE),
       raw_states = split(raw[keep], state[keep]))
}


#_______________________________________________________________________________
# Individual (not aggregated) GBIF occurrence records of a taxon in one
# Brazilian state, given all the raw state values GBIF uses for it (e.g.
# "Bahia", "Ba", "Est. do Bahia"), each with a link to its GBIF occurrence
# page. Used to list the records behind a new-state-record candidate. Never
# errors - returns a zero-row data.frame on failure. ####
.gbif_brazil_state_records <- function(taxon_key, raw_states, limit = 50) {
  val <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
  empty <- data.frame(Municipality = character(0), Locality = character(0),
                      RecordedBy = character(0), RecordNumber = character(0),
                      EventDate = character(0), Institution = character(0),
                      CatalogNumber = character(0), Identified_as = character(0),
                      RecordLink = character(0), stringsAsFactors = FALSE)
  if (length(raw_states) == 0) return(empty)
  res <- .get_json(sprintf("https://api.gbif.org/v1/occurrence/search?taxonKey=%s&country=BR%s&limit=%d",
                           taxon_key,
                           paste0("&stateProvince=", vapply(raw_states, utils::URLencode,
                                                           character(1), reserved = TRUE),
                                  collapse = ""),
                           limit))
  if (is.null(res) || length(res$results) == 0) return(empty)
  do.call(rbind, lapply(res$results, function(r) {
    data.frame(Municipality = val(r$municipality), Locality = val(r$locality),
               RecordedBy = val(r$recordedBy), RecordNumber = val(r$recordNumber),
               EventDate = val(r$eventDate),
               Institution = if (is.null(r$institutionCode)) val(r$collectionCode) else val(r$institutionCode),
               CatalogNumber = val(r$catalogNumber), Identified_as = val(r$scientificName),
               RecordLink = paste0("https://www.gbif.org/occurrence/", val(r$key)),
               stringsAsFactors = FALSE)
  }))
}


#_______________________________________________________________________________
# Brazilian speciesLink records of a plant name, counted by state. The
# speciesLink API (https://specieslink.net/ws/1.0/) is open but needs a free
# API key, passed as 'key'. Records are read up to 'max_records' (5000 per
# page) and their state values normalised to FFB state names. Returns
# data.frame(State, speciesLink_records), or NULL on failure. ####
.splink_brazil_state_counts <- function(taxon, key, max_records = 20000) {
  states <- character(0)
  offset <- 0
  repeat {
    res <- .get_json(sprintf(paste0("https://specieslink.net/ws/1.0/search?scientificname=%s",
                                    "&country=Brazil&kingdom=Plantae&limit=5000&offset=%d",
                                    "&apikey=%s"),
                             utils::URLencode(taxon, reserved = TRUE), offset,
                             utils::URLencode(key, reserved = TRUE)))
    if (is.null(res)) return(if (offset == 0) NULL else break)
    feats <- res$features
    if (length(feats) == 0) break
    states <- c(states, vapply(feats, function(f) {
      p <- f$properties
      nm <- names(p)[tolower(names(p)) == "stateprovince"]
      if (length(nm) == 0 || is.null(p[[nm[1]]])) NA_character_ else as.character(p[[nm[1]]])
    }, character(1)))
    offset <- offset + 5000
    if (length(feats) < 5000 || offset >= max_records) break
  }
  st <- .normalize_br_state(states)
  st <- st[!is.na(st)]
  if (length(st) == 0) {
    return(data.frame(State = character(0), speciesLink_records = integer(0),
                      stringsAsFactors = FALSE))
  }
  tab <- table(st)
  data.frame(State = names(tab), speciesLink_records = as.integer(tab),
             stringsAsFactors = FALSE)
}


#_______________________________________________________________________________
# FFB's officially listed states for a taxon, from the parsed FFB taxon and
# distribution tables. For a genus, the states of all its species and
# infraspecific taxa are combined (FFB does not list a distribution for the
# genus itself); for a species, those of the species and its infraspecific
# taxa. A synonym is replaced by its accepted name first. Returns
# list(found, name, states). ####
.ffb_taxon_states <- function(taxon, taxon_df, distribution_df) {
  rows <- which(taxon_df$taxonName %in% taxon)
  if (length(rows) == 0) {
    return(list(found = FALSE, name = taxon, states = character(0)))
  }
  row <- rows[1]
  name <- taxon
  if ("acceptedNameUsageID" %in% names(taxon_df) &&
      !is.na(taxon_df$acceptedNameUsageID[row]) &&
      nzchar(taxon_df$acceptedNameUsageID[row]) &&
      taxon_df$acceptedNameUsageID[row] != taxon_df$id[row]) {
    acc <- which(taxon_df$id %in% taxon_df$acceptedNameUsageID[row])
    if (length(acc) > 0) name <- taxon_df$taxonName[acc[1]]
  }
  one_word <- !grepl("\\s", name)
  ids <- if (one_word) {
    taxon_df$id[taxon_df$genus %in% name]
  } else {
    taxon_df$id[taxon_df$taxonName %in% name |
                  startsWith(taxon_df$taxonName, paste0(name, " "))]
  }
  loc <- gsub("^BR-", "", distribution_df$locationID[distribution_df$id %in% ids])
  loc <- loc[!is.na(loc) & nzchar(loc)]
  states <- .normalize_br_state(loc)
  list(found = TRUE, name = name, states = sort(unique(states[!is.na(states)])))
}


#_______________________________________________________________________________
# Render the gap-checker (flora_species_gap(), flora_distribution_gap()) HTML
# report (inst/rmd/<template>) from a
# pre-computed data list: KPI boxes plus a filterable/downloadable DT table.
# Degrades gracefully (message + skip) if the reporting packages or the
# template are unavailable, so a missing Suggests dependency never breaks the
# underlying analysis. ####
.flora_render_report <- function(template, data_list, taxon, dir, filename,
                                 verbose, open_report) {

  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("DT", quietly = TRUE) ||
      !requireNamespace("htmltools", quietly = TRUE)) {
    if (verbose) {
      message("  Packages 'rmarkdown', 'DT', and 'htmltools' are required for the HTML ",
              "report; skipping it (the spreadsheet was still saved). Install them with ",
              "install.packages(c('rmarkdown', 'DT', 'htmltools')).")
    }
    return(invisible(FALSE))
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  rmd_template <- system.file("rmd", template, package = "floraR")
  if (!nzchar(rmd_template)) {
    if (verbose) message("  HTML report template not found; skipping.")
    return(invisible(FALSE))
  }

  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(data_list, tmp_rds)
  on.exit(unlink(tmp_rds), add = TRUE)

  html_out <- file.path(normalizePath(dir), paste0(filename, ".html"))

  if (verbose) message("Rendering HTML report...")
  render_ok <- tryCatch({
    rmarkdown::render(
      input = rmd_template,
      output_file = html_out,
      params = list(data_path = tmp_rds, taxon = taxon),
      envir = new.env(parent = globalenv()),
      quiet = !verbose
    )
    TRUE
  }, error = function(e) {
    if (verbose) message("  HTML report rendering failed: ", conditionMessage(e))
    FALSE
  })

  if (render_ok) {
    if (verbose) message("Report saved: ", html_out)
    if (open_report && interactive()) utils::browseURL(html_out)
  }

  invisible(render_ok)
}
