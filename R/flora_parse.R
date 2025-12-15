#' Parse Darwin Core Archive (DwC-A) files from Flora e Funga do Brasil
#'
#' @description
#' Reads and organizes locally downloaded Darwin Core Archive (DwC-A) datasets from the
#' *Flora e Funga do Brasil* (FFB) repository, available via the
#' \href{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}{IPT Data Portal}
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#'
#' This function processes and cleans FFB DwC-A data by:
#' \itemize{
#'   \item Reading all data tables (e.g., \code{taxon.txt}, \code{distribution.txt}, \code{speciesprofile.txt})
#'         within each versioned DwC-A folder.
#'   \item Generating a unified and standardized taxon name from available name components.
#'   \item Extracting structured data such as endemism status, phytogeographic domains,
#'         life forms, habitats, and vegetation types.
#'   \item Returning all cleaned and parsed tables as a list for further analysis with
#'         \code{\link{flora_records}} or other downstream functions.
#' }
#'
#' @details
#' The function works entirely **offline**, provided that the DwC-A datasets have been
#' previously downloaded with \code{\link{flora_download}}.
#' It is designed as the second step in the workflow:
#' \enumerate{
#'   \item \code{\link{flora_download}} — to download and store versioned DwC-A files.
#'   \item \code{flora_parse()} — to parse and structure those files into a ready-to-use list.
#'   \item \code{\link{flora_records}} — to summarize or query the parsed data.
#' }
#'
#' @usage
#' flora_parse(
#'   path = NULL,
#'   version = "latest",
#'   verbose = TRUE
#' )
#'
#' @param path Character. Path to the local directory where the versioned FFB DwC-A
#' folders were downloaded (usually created by \code{flora_download()}).
#'
#' @param version Character. One or more specific dataset versions to parse.
#' Accepts:
#' \itemize{
#'   \item \code{"latest"} — parses the most recent dataset available in \code{path}.
#'   \item \code{"all"} — parses all downloaded versions within the specified path.
#'   \item A version string or vector of versions (e.g., \code{"393.418"}).
#' }
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative progress messages
#' during parsing. If \code{FALSE}, runs quietly.
#'
#' @return
#' A named list of parsed DwC-A datasets, where each element corresponds to a versioned
#' folder (e.g., \code{"dwca_ffb_v393_418"}).
#' Each list element includes:
#' \itemize{
#'   \item \code{$meta} — metadata describing the dataset structure.
#'   \item \code{$data} — a nested list of parsed Darwin Core tables, including:
#'     \itemize{
#'       \item \code{taxon.txt} — with a new standardized \code{taxonName} field.
#'       \item \code{distribution.txt} — with separate \code{endemism} and
#'             \code{phytogeographicDomain} columns.
#'       \item \code{speciesprofile.txt} — with parsed \code{lifeForm}, \code{habitat},
#'             and \code{vegetationType} information.
#'     }
#' }
#'
#' @seealso
#' \code{\link{flora_version}} to inspect available dataset versions.
#' \code{\link{flora_download}} to download the corresponding DwC-A archives.
#' \code{\link{flora_records}} to extract and summarize parsed records.
#'
#' @note
#' This function can operate **without an active internet connection**, as long as
#' the DwC-A datasets were downloaded beforehand using \code{\link{flora_download}}.
#'
#' @author
#' Domingos Cardoso & Kelmer Martins-Cunha
#'
#' @examples
#' \dontrun{
#' # Step 1 — Download latest FFB DwC-A dataset
#' flora_download(version = "latest",
#'                dir = "flora_download")
#'
#' # Step 2 — Parse local DwC-A dataset (works offline)
#' dwca <- flora_parse(path = "flora_download",
#'                     version = "latest")
#'
#' # Step 3 — Explore parsed data
#' names(dwca)
#' names(dwca[["dwca_ffb_v393_418"]][["data"]])
#'
#' # Step 4 — Use with flora_records() for summaries
#' records <- flora_records(dwca)
#' }
#'
#' @importFrom finch dwca_read
#' @importFrom dplyr mutate select relocate filter rename
#' @importFrom tidyr unnest
#' @importFrom stringr str_match
#' @importFrom purrr pmap_chr
#' @importFrom magrittr "%>%"
#'
#' @export

flora_parse <- function(path = NULL,
                        version = "latest",
                        verbose = TRUE) {

  dwca_folders <- list.files(path)
  dwca_filenames <- lapply(paste0(path, "/", dwca_folders), list.files)

  # path check
  .arg_check_path(path, dwca_folders, dwca_filenames)

  downloaded_files <- gsub("[.]latest", "", gsub("_", ".", gsub("dwca_ffb_v", "", dwca_folders)))

  if ("latest" %in% version) {
    tf <- grepl("latest", dwca_folders)
    if (any(tf)) {
      latest_version <- downloaded_files[tf]
    } else {
      stop(paste0("❌️ The latest version is not not available within the provided path: '", path, "'\n\n",
                  "Try downloading again using flora_download()"))
    }
    dwca_folders <- dwca_folders[downloaded_files %in% latest_version]
  } else if (!"all" %in% version) {
    dwca_folders <- dwca_folders[downloaded_files %in% version]
  }

  # Calling all dwca files

  if (verbose) {
    message("Parsing data from dwca folders...\n\n")
  }
  dwca_files <- lapply(dwca_folders,
                       function(x) finch::dwca_read(input = paste0(path, "/", x),
                                                    read = TRUE,
                                                    encoding = "UTF-8",
                                                    na.strings = ""))
  names(dwca_files) <- dwca_folders

  for (i in seq_along(dwca_files)) {

    dwca_files[[i]][["data"]][["taxon.txt"]][["family"]][dwca_files[[i]][["data"]][["taxon.txt"]][["family"]] %in% "NA"] <- NA
    dwca_files[[i]][["data"]][["taxon.txt"]][["genus"]][dwca_files[[i]][["data"]][["taxon.txt"]][["genus"]] %in% "NA"] <- NA

    taxon_df <- dwca_files[[i]][["data"]][["taxon.txt"]]
    distribution_df <- dwca_files[[i]][["data"]][["distribution.txt"]]
    resourcerelationship_df <- dwca_files[[i]][["data"]][["resourcerelationship.txt"]]
    vernacularname_df <- dwca_files[[i]][["data"]][["vernacularname.txt"]]
    typesandspecimen_df <- dwca_files[[i]][["data"]][["typesandspecimen.txt"]]
    reference_df <- dwca_files[[i]][["data"]][["reference.txt"]]
    speciesprofile_df <- dwca_files[[i]][["data"]][["speciesprofile.txt"]]

    # Remove "Fungi" to keep just the "Plantae"
    tf <- taxon_df$kingdom %in% "Plantae"
    plantae_ids <- taxon_df$id[tf]

    dwca_files[[i]][["data"]][["taxon.txt"]] <- taxon_df[tf, ]
    dwca_files[[i]][["data"]][["distribution.txt"]] <- distribution_df[distribution_df$id %in% plantae_ids, ]
    dwca_files[[i]][["data"]][["resourcerelationship.txt"]] <- resourcerelationship_df[resourcerelationship_df$id %in% plantae_ids, ]
    dwca_files[[i]][["data"]][["vernacularname.txt"]] <- vernacularname_df[vernacularname_df$id %in% plantae_ids, ]
    dwca_files[[i]][["data"]][["typesandspecimen.txt"]] <- typesandspecimen_df[typesandspecimen_df$id %in% plantae_ids, ]
    dwca_files[[i]][["data"]][["reference.txt"]] <- reference_df[reference_df$id %in% plantae_ids, ]
    dwca_files[[i]][["data"]][["speciesprofile.txt"]] <- speciesprofile_df[speciesprofile_df$id %in% plantae_ids, ]

    dwca_files[[i]][["data"]][["taxon.txt"]] <- dwca_files[[i]][["data"]][["taxon.txt"]] %>%
      dplyr::mutate(
        taxonName = purrr::pmap_chr(
          list(genus, specificEpithet, infraspecificEpithet),
          function(genus, specificEpithet, infraspecificEpithet) {
            parts <- c(genus, specificEpithet, infraspecificEpithet)
            parts <- parts[!is.na(parts) & parts != ""]
            if (length(parts) == 0) NA_character_ else paste(parts, collapse = " ")
          }
        )
      ) %>%
      dplyr::relocate(taxonName, .before = taxonRank)

    # Organize distribution and domains data
    temp_df <- dwca_files[[i]][["data"]][["distribution.txt"]]

    endemism <- stringr::str_match(temp_df$occurrenceRemarks, '"endemism":"([^"]+)"')[,2]
    domains_raw <- stringr::str_match(temp_df$occurrenceRemarks, '"phytogeographicDomain":\\[(.*)\\]')[,2]
    phytogeographicDomain <- strsplit(gsub('"', "", domains_raw), ",")

    # Assemble data frame
    temp_df_fast <- data.frame(
      endemism = endemism,
      phytogeographicDomain = I(phytogeographicDomain), # keep as list-column
      stringsAsFactors = FALSE
    )
    temp_df <- cbind(temp_df, temp_df_fast)
    temp_df_long <- tidyr::unnest(temp_df, phytogeographicDomain)

    dwca_files[[i]][["data"]][["distribution.txt"]] <- temp_df_long

    # Organize speciesprofile data frame
    temp_df <- dwca_files[[i]][["data"]][["speciesprofile.txt"]]

    # Handle NAs safely
    text <- temp_df$lifeForm
    text[is.na(text)] <- "{}"

    # Extract parts using regex
    lifeForm_raw <- stringr::str_match(text, '"lifeForm":\\[(.*?)\\]')[,2]
    habitat_raw <- stringr::str_match(text, '"habitat":\\[(.*?)\\]')[,2]
    vegetation_raw <- stringr::str_match(text, '"vegetationType":\\[(.*?)\\]')[,2]

    # Split into character vectors (remove quotes)
    lifeForm_list <- strsplit(gsub('"', "", lifeForm_raw), ",")
    habitat_list <- strsplit(gsub('"', "", habitat_raw), ",")
    vegetation_list <- strsplit(gsub('"', "", vegetation_raw), ",")

    # Combine into a new clean data frame
    temp_df_fast <- dplyr::tibble(
      lifeForm_new = I(lifeForm_list),
      habitat_new = I(habitat_list),
      vegetationType = I(vegetation_list)
    )

    temp_df_long <- cbind(temp_df, temp_df_fast)

    temp_df_long <- temp_df_long %>%
      tidyr::unnest(cols = c(lifeForm_new)) %>%
      tidyr::unnest(cols = c(habitat_new)) %>%
      tidyr::unnest(cols = c(vegetationType)) %>%
      dplyr::filter(!if_all(-1, ~ is.na(.) | . == "")) %>%
      dplyr::select("id", "lifeForm_new",  "habitat_new", "vegetationType", "lifeForm") %>%
      dplyr::rename(lifeForm_json = lifeForm) %>%
      dplyr::rename(lifeForm = lifeForm_new) %>%
      dplyr::rename(habitat = habitat_new)

    # Save back into your structure
    dwca_files[[i]][["data"]][["speciesprofile.txt"]] <- temp_df_long

  }

  if (verbose) {
    message("Versioned FFB datasets and associated metadata were parsed from the following dwca folders: \n\n",
            paste0(names(dwca_files), "\n"))
  }

  return(dwca_files)
}
