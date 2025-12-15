#' Summarize Available Plant Taxonomic and Distributional Records from Flora e Funga do Brasil
#'
#' @description
#' Retrieves and summarizes metadata of the currently available versions of plant
#' taxonomic and distributional records from the \href{https://floradobrasil.jbrj.gov.br/consulta/}{Flora e Funga do Brasil (FFB)},
#' hosted by the \href{https://www.gov.br/jbrj/pt-br}{Rio de Janeiro Botanical Garden}.
#' The metadata includes version number, publication date, number of records, and
#' direct access URLs.
#'
#' @details
#' This function accesses the \href{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}{IPT metadata for the FFB dataset}
#' and extracts version information for each published data release. It returns
#' a cleaned and structured summary as a data frame.
#'
#' @return
#' A `data.frame` with the following columns:
#' \itemize{
#'   \item \code{Version}: Version number of the dataset release.
#'   \item \code{Published_on}: Timestamp of the dataset release.
#'   \item \code{Records}: Number of plant specimen records in the release.
#'   \item \code{URL}: Direct link to the dataset version on the IPT portal.
#' }
#'
#' @author
#' Domingos Cardoso & Kelmer Martins-Cunha
#'
#' @seealso
#' \code{\link{flora_download}} for downloading the data of a specific version.
#'
#' @examples
#' \dontrun{
#'   ffb_version <- flora_version()
#'   head(ffb_version)
#' }
#'
#' @importFrom stringr str_match str_replace
#' @importFrom dplyr tibble bind_rows filter mutate arrange select if_all
#' @importFrom magrittr "%>%"
#'
#' @export

flora_version <- function() {

  # Get raw metadata from FFB repository
  ipt_metadata <- readLines("https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil",
                            encoding = "UTF-8",
                            warn = F)

  # Split the text wherever the marker appears
  blocks <- unlist(strsplit(paste(ipt_metadata, collapse = "\n"),
                            "/\\* only show released versions.*?\\*/"))

  # Clean out empty entries
  blocks <- blocks[nzchar(trimws(blocks))]

  # Parse each block
  df <- lapply(blocks, function(b) {
    dplyr::tibble(
      Version = stringr::str_match(b, ">(\\d{3}\\.\\d{3})<")[,2],
      Latest = NA,
      Published_on = stringr::str_match(b, "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})")[,2],
      Records = stringr::str_match(b, "'([0-9,]+)'")[,2],
      URL = stringr::str_match(b, "href=\\\"([^\\\"]+)")[,2] %>% stringr::str_replace("&amp;", "&")
    )
  }) %>% dplyr::bind_rows()

  # Drop empty rows
  df <- df %>%
    dplyr::filter(!(if_all(everything(), ~ is.na(.) | . == ""))) %>%
    dplyr::mutate(Version_num = as.numeric(Version))

  df <- df[!grepl("bootstrap", df$URL), ]
  latest_version <- sprintf("%.3f", max(df$Version_num[!is.na(df$Version_num)])+0.001)

  df$Version[is.na(df$URL)] <- latest_version
  df$Latest[is.na(df$URL)] <- TRUE
  df$Latest[is.na(df$Latest)] <- FALSE
  df$URL[is.na(df$URL)] <- paste0("https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil&v=", latest_version)

  df$Version <- as.character(gsub(".*lista_especies_flora_brasil[&]v[=]", "", df$URL))

  df <- df %>% dplyr::arrange(desc(Published_on)) %>%
    dplyr::select(-Version_num)

  return(df)

}
