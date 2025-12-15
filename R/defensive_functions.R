# Small functions to evaluate user input for the main functions and
# return meaningful errors.
# Author: Domingos Cardoso

#_______________________________________________________________________________
# Check if the dir input is "character" type and if it has a "/" in the end
.arg_check_dir <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"character" %in% class_x) {
    stop(paste0("The argument dir should be a character, not '", class_x, "'."),
         call. = FALSE)
  }
  if (grepl("[/]$", x)) {
    x <- gsub("[/]$", "", x)
  }
  return(x)
}


#_______________________________________________________________________________
# Check path
.arg_check_path <- function(path, dwca_folders, dwca_filenames) {
  # Check classes
  class_path <- class(path)
  if (!"character" %in% class_path) {
    stop(paste0("The argument path should be a character, not '", class_path, "'."),
         call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(paste0("There is no folder '", path, "' in the working directory."),
         call. = FALSE)
  } else {
    if (!any(grepl("^dwca", dwca_folders))) {
      stop(paste0("There is no FFB-downloaded dwca folder within the directory '", path, "'."),
           call. = FALSE)
    } else {
      tf <- 0 == unlist(lapply(dwca_filenames, length))
      if (any(tf)) {
        if (length(which(tf)) == 1) {
          stop(paste0(paste0("The dwca folder ",
                             paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "),
                             " within the directory '", path, "' is fully empty.\n\n"),
                      "Either download such dwca folder again or exclude it."),
               call. = FALSE)
        } else {
          stop(paste0(paste0("The dwca folders ",
                             paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "),
                             " within the directory '", path, "' are fully empty.\n\n"),
                      "Either download such dwca folders again or exclude them."),
               call. = FALSE)
        }
      }

      tf <- lapply(dwca_filenames, function(x) c("taxon.txt", "distribution.txt") %in% x)
      tf <- !unlist(lapply(tf, any))

      if (any(tf)) {
        stop(paste0(paste0("A 'taxon.txt' and/or 'distribution.txt' files are missing in the dwca folders ",
                           paste0("'", paste0(dwca_folders[tf]), "'", collapse = ", "), ".\n\n"),
                    "Either download such dwca folders again or exclude them."),
             call. = FALSE)
      }

    }
  }
}


#_______________________________________________________________________________
# Check the recordYear input
.arg_check_recordYear <- function(x) {
  if (length(x) > 2) {
    stop("The argument 'recordYear' should be either a single year or a range of two years.")
  }

  # Check if all elements have exactly four digits
  if (!all(nchar(x) == 4 & grepl("^[0-9]{4}$", x))) {
    stop("All elements must be 4-digit numbers.")
  }

  # If there are two elements, check if the first is less than the second
  if (length(x) == 2 && as.numeric(x[1]) > as.numeric(x[2])) {
    stop("If a range is provided, the first year must be less than the second year.")
  }
}


#_______________________________________________________________________________
# Check the state input
.arg_check_state <- function(x) {

  valid_states <- c("Acre" = "AC", "Alagoas" = "AL", "Amap\u00e1" = "AP", "Amazonas" = "AM",
                    "Bahia" = "BA", "Cear\u00e1" = "CE", "Distrito Federal" = "DF",
                    "Esp\u00edrito Santo" = "ES", "Goi\u00e1s" = "GO", "Maranh\u00e3o" = "MA",
                    "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
                    "Par\u00e1" = "PA", "Para\u00edba" = "PB", "Paran\u00e1" = "PR", "Pernambuco" = "PE",
                    "Piau\u00ed" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
                    "Rio Grande do Sul" = "RS", "Rond\u00f4nia" = "RO", "Roraima" = "RR",
                    "Santa Catarina" = "SC", "S\u00e3o Paulo" = "SP", "Sergipe" = "SE",
                    "Tocantins" = "TO")

  valid_states_full <- names(valid_states)
  valid_states_acronyms <- unname(valid_states)

  states_no_diacritics <- stringi::stri_trans_general(x, "Latin-ASCII")
  valid_states_full_no_diacritics <- stringi::stri_trans_general(valid_states_full, "Latin-ASCII")
  valid_states_acronyms_no_diacritics <- stringi::stri_trans_general(valid_states_acronyms, "Latin-ASCII")

  corrected_states <- character(length(x))

  for (i in seq_along(x)) {
    match_full <- match(states_no_diacritics[i], valid_states_full_no_diacritics)
    match_acronym <- match(states_no_diacritics[i], valid_states_acronyms_no_diacritics)

    if (!is.na(match_full)) {
      corrected_states[i] <- valid_states_full[match_full]
    } else if (!is.na(match_acronym)) {
      corrected_states[i] <- names(valid_states)[match_acronym]
    } else {
      corrected_states[i] <- x[i]  # return as-is
    }
  }

  return(corrected_states)
}

