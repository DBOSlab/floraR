# Auxiliary functions to support main functions
# Author: Domingos Cardoso


#_______________________________________________________________________________
# Function to reorder retrieved data based on specific columns ####
.reorder_df <- function(df, reorder) {

  if (!is.null(reorder)) {
    columns_to_order <- reorder
  } else {
    columns_to_order <- c("herbarium", "taxa", "collector", "area", "year")
  }

  tf <- columns_to_order %in% "herbarium"
  if (any(tf)) {
    columns_to_order[tf] <- "collectionCode"
  }
  tf <- columns_to_order %in% "taxa"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("family", "genus", "specificEpithet"),
                                      columns_to_replace = "taxa")
  }
  tf <- columns_to_order %in% "collector"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("recordedBy", "recordNumber"),
                                      columns_to_replace = "collector")
  }
  tf <- columns_to_order %in% "area"
  if (any(tf)) {
    columns_to_order <- .replace_cols(columns_to_order,
                                      replace = c("country", "stateProvince", "municipality"),
                                      columns_to_replace = "area")
  }

  # Arrange the dataframe based on the vector of column names
  df <- df %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(columns_to_order)))

  return(df)
}

.replace_cols <- function (columns_to_order,
                           replace,
                           columns_to_replace) {
  index <- which(columns_to_order == columns_to_replace)
  # Perform the replacement based on the index position
  if (index == 1) {
    # Special handling if 'herbarium' is the first element
    columns_to_order <- c(replace, columns_to_order[(index+1):length(columns_to_order)])
  } else if (index == length(columns_to_order)) {
    # Special handling if the element to replace is the last element
    columns_to_order <- c(columns_to_order[1:(index-1)], replace)
  } else {
    # Standard replacement for elements in the middle
    columns_to_order <- c(columns_to_order[1:(index-1)], replace,
                          columns_to_order[(index+1):length(columns_to_order)])
  }

  return(columns_to_order)
}


#_______________________________________________________________________________
# Function to filter occurrence data ####
.filter_occur_df <- function(occur_df, taxon, state, recordYear, verbose) {

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

  # Filter by record year only ####

  if (!is.null(recordYear)) {
    if (verbose) {
      message("\nFiltering recordYear... ")
    }

    .check_year_match(occur_df, recordYear, verbose)

    if (length(recordYear) == 1) {
      # If only one year is given, filter for that specific year
      occur_df <- occur_df[occur_df$year == recordYear, ]
    } else if (length(recordYear) == 2) {
      # If a range is given, filter for records within that range
      occur_df <- occur_df[!is.na(occur_df$year) &
                             occur_df$year >= as.numeric(recordYear[1]) &
                             occur_df$year <= as.numeric(recordYear[2]), ]
    }
  }
  return(occur_df)
}

.check_taxon_match <- function(occur_df, taxon, verbose) {

  all_names <- unique(c(occur_df$family, occur_df$genus, occur_df$taxonName))
  matched_taxa <- taxon[taxon %in% all_names]
  unmatched_taxa <- setdiff(taxon, matched_taxa)

  if (verbose) {
    if (length(unmatched_taxa) > 0) {
      message("The following taxa were not found in any column: ", paste(unmatched_taxa, collapse = ", "))
    }
  }
  matches <- occur_df$family %in% matched_taxa |
    occur_df$genus %in% matched_taxa |
    occur_df$taxonName %in% matched_taxa
  matches <- any(matches)

  if (!matches) {
    stop(paste0(
      "Your input 'taxon' list must contain at least one name existing within the REFLORA collections.\n",
      "Check whether the input taxon list has any typo: ",
      paste(unmatched_taxa, collapse = ", ")
    ))
  }
}

.check_state_match <- function(occur_df, state, verbose) {

  matched_state <- state[state %in% unique(occur_df$stateProvince)]
  unmatched_state <- setdiff(state, matched_state)

  if (verbose && length(unmatched_state) > 0) {
    message("The following states were not found: ", paste(unmatched_state, collapse = ", "))
  }

  if (length(matched_state) == 0) {
    stop(paste0(
      "Your input 'state' list must contain at least one name existing within the REFLORA collections.\n",
      "Check whether the input state list has any typo: ",
      paste(unmatched_state, collapse = ", ")
    ))
  }
}

.check_year_match <- function(occur_df, recordYear, verbose) {

  matched_year <- recordYear[recordYear %in% unique(occur_df$year)]
  unmatched_year <- setdiff(recordYear, matched_year)

  if (verbose && length(unmatched_year) > 0) {
    message("The following recordYear were not found: ", paste(unmatched_year, collapse = ", "))
  }

  if (length(matched_year) == 0) {
    stop(paste0(
      "Your input 'recordYear' list must contain at least one year existing within the REFLORA collections.\n",
      "Check whether the input recordYear list has any typo: ",
      paste(unmatched_year, collapse = ", ")
    ))
  }
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
    dists    <- utils::adist(search_name, taxon_df$taxonName[cand_idx])[1L, ]
    within   <- which(dists <= threshold)
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
    acc_id  <- as.character(taxon_df$acceptedNameUsageID[row_idx])

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
.flora_prepare_taxon <- function(version, verbose) {
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
    rows  <- match_res$rows
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

  # Verificar se todas as linhas têm as mesmas colunas
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
