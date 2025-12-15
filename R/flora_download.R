#' Download Plant Datasets from the Flora e Funga do Brasil Platform
#'
#' @description
#' Downloads plant taxonomic and distributional records in Darwin Core Archive
#' (DwC-A) format from the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}'s
#' \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)} platform.
#' The function allows downloading the latest release, specific dataset versions,
#' or all available versions.
#'
#' @details
#' Each dataset version is retrieved directly from the \href{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}{FFB IPT data portal}
#' and saved locally in a structured folder. The function checks whether the requested
#' version is already downloaded and avoids re-downloading it unnecessarily.
#'
#' To download a specific version, it is recommended to use \code{\link{flora_version}()}
#' to inspect available versions first and extract the exact version string.
#'
#' @usage
#' flora_download(
#'   version = "latest",
#'   verbose = TRUE,
#'   dir = "flora_download"
#' )
#'
#' @param version Character. Specifies which dataset version(s) to download:
#' \itemize{
#'   \item \code{"latest"} (default): Download the most recent dataset version.
#'   \item \code{"all"}: Download all available dataset versions (may require large disk space).
#'   \item A specific version string (e.g., \code{"393.001"}) or a character vector of versions.
#' }
#' Use \code{\link{flora_version}()} to view and select valid version numbers.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative messages during download.
#' If \code{FALSE}, runs silently.
#'
#' @param dir Character. Local directory path to save the downloaded files.
#' Defaults to creating a folder named \code{"flora_download"} in the working directory.
#' Each dataset version is extracted into its own subdirectory.
#'
#' @return
#' One or more folders containing the Darwin Core Archive (DwC-A) files for the selected
#' version(s) of the *Flora e Funga do Brasil* dataset.
#' If no internet connection is available, previously downloaded local datasets
#' will be used automatically (if found).
#'
#' @seealso
#' \code{\link{flora_version}} to retrieve metadata and available versions.
#'
#' @author
#' Domingos Cardoso & Kelmer Martins-Cunha
#'
#' @examples
#' \dontrun{
#' # View available dataset versions
#' versions_df <- flora_version()
#'
#' # Download the latest dataset
#' flora_download()
#'
#' # Download a specific version
#' flora_download(version = versions_df$Version[1])
#'
#' # Download multiple versions
#' flora_download(version = c("393.001", "392.002"))
#'
#' # Download all versions (large download!)
#' flora_download(version = "all")
#'
#' # Work offline (uses previously downloaded data if available)
#' flora_download(version = "latest", dir = "flora_download")
#' }
#'
#' @importFrom stringr str_split
#' @importFrom utils download.file unzip write.csv
#'
#' @export

flora_download <- function(version = "latest",
                           verbose = TRUE,
                           dir = "flora_download") {

  # dir check
  dir <- .arg_check_dir(dir)

  # Create a new directory to save the dataframe
  # If there is no directory create one in the working directory
  if (!dir.exists(dir)) {
    if (verbose) {
      message(paste0("Creating directory '", dir, "' in working directory..."))
    }
    dir.create(dir)
  }

  summary_df <- tryCatch({
    suppressWarnings(flora_version())
  }, error = function(e) {
    if (verbose) {
      message("No internet connection, skipping flora_version()")
    }
    return(NULL)
  })

  if ("latest" %in% version) {

    downloaded_files <- gsub("_", ".", gsub("dwca_ffb_v", "", list.files(dir)))
    tf <- grepl("latest", downloaded_files)

    if (!is.null(summary_df)) {
      tftf <- summary_df$Version %in% gsub("[.]latest", "", downloaded_files[tf])
      tf_latest_version <- summary_df$Latest[tftf]
    } else {
      tf_latest_version <- FALSE
    }

    if (any(tf) && tf_latest_version | any(tf) && is.null(summary_df)) {

      if (verbose) {
        message(paste0("Using latest FFB version '", gsub("[.]latest", "", downloaded_files[tf]), "' previously downloaded!"))
      }

    } else {

      # Get raw metadata from FFB repository
      download_success <- try({
        summary_df <- flora_version()
        TRUE
      }, silent = TRUE)

      if (inherits(download_success, "try-error")) {
        stop("❌️ No internet connection or no valid local dataset found")
      }

      ex_dwca_folder = gsub("[.]", "_", paste0("dwca_ffb_v", summary_df$Version[1], "_latest"))
      flora_database = paste0(dir, "/", ex_dwca_folder)

      dwca_file = paste0("https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v=",
                         summary_df$Version[1])

      # Temporary zip file path
      destzipfile <- tempfile(fileext = ".zip")

      if (verbose) {
        message(paste0("Downloading DwC-A files for the latest FFB version '",
                       summary_df$Version[1], "'"))
      }

      utils::download.file(url =  dwca_file,
                           destfile = destzipfile,
                           method = "curl")

      utils::unzip(destzipfile, exdir = flora_database)
      unlink(destzipfile)

      if (verbose) {
        message(paste0("Latest FFB version '", summary_df$Version[1], "' sucessfully downloaded!"))
      }

      downloaded_files <- list.files(dir)
      tf <- grepl("latest", downloaded_files)
      if (length(which(tf) > 1)) {
        # Get all TRUE indices except the last one
        true_indices <- which(tf)
        result_indices <- true_indices[-length(true_indices)]
        old_name <- file.path(dir, downloaded_files[result_indices])
        new_name <- gsub("_latest", "", old_name)
        invisible(file.rename(old_name, new_name))
      }

    }

  } else {

    downloaded_files <- gsub("[.]latest", "", gsub("_", ".", gsub("dwca_ffb_v", "", list.files(dir))))
    tf <- any(version %in% downloaded_files) | "all" %in% version && length(downloaded_files) > 0

    # Get raw metadata from FFB repository
    download_success <- try({
      summary_df <- flora_version()
      latest_version <- summary_df$Version[1]
      TRUE
    }, silent = TRUE)

    if (inherits(download_success, "try-error") && !tf) {
      stop("❌️ No internet connection or no valid local dataset found")

    } else {

      if (inherits(download_success, "try-error") && tf) {

        if ("all" %in% version) {
          message(paste0("There is no internet conection but the following previously downloaded FFB dataset version will be considered:\n"),
                  paste0(downloaded_files, collapse = ", "))
        } else {
          message(paste0("There is no internet conection but the following requested FFB dataset version were already previously downloaded and will be considered:\n"),
                  paste0(version[version %in% downloaded_files], collapse = ", "))
        }

      } else {

        if (!"all" %in% version && length(downloaded_files) > 0) {
          version <- version[!version %in% downloaded_files]

        }
        if ("all" %in% version && length(downloaded_files) > 0) {
          version <- summary_df$Version
          version <- version[!version %in% downloaded_files]
        }

        summary_df <- summary_df[summary_df$Version %in% version, ]

        if (nrow(summary_df) > 0) {

          for (i in seq_along(summary_df$Version)) {

            if (summary_df$Version[i] %in% latest_version) {
              ex_dwca_folder = gsub("[.]", "_", paste0("dwca_ffb_v", summary_df$Version[i], "_latest"))
            } else {
              ex_dwca_folder = gsub("[.]", "_", paste0("dwca_ffb_v", summary_df$Version[i]))
            }

            flora_database = paste0(dir, "/", ex_dwca_folder)

            dwca_file = paste0("https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v=",
                               summary_df$Version[i])

            # Temporary zip file path
            destzipfile <- tempfile(fileext = ".zip")

            if (verbose) {
              message(paste0("Downloading DwC-A files for the FFB version '",
                             summary_df$Version[i], "'... ", i, "/",
                             length(summary_df$Version)))
            }

            utils::download.file(url =  dwca_file,
                                 destfile = destzipfile,
                                 method = "curl")

            utils::unzip(destzipfile, exdir = flora_database)
            unlink(destzipfile)

            if (verbose) {
              message(paste0("FFB version '", summary_df$Version[i], "' sucessfully downloaded!\n\n"))
            }

          }

        }

      }

    }

  }

}

