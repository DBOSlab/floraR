#' Extract species descriptions from Flora e Funga do Brasil pages
#'
#' Downloads printable PDF versions of Flora e Funga do Brasil (FFB) taxon pages
#' for a set of species and extracts text from the description sections. The
#' function attempts to recover two kinds of descriptions from each page:
#' controlled descriptions ("Descrição com campos controlados") and free
#' descriptions ("Descrição livre").
#'
#' For each taxon, the function:
#' \enumerate{
#'   \item builds a printable page URL from the \code{references} column;
#'   \item saves the page temporarily as \code{temp.pdf};
#'   \item extracts text with \pkg{pdftools};
#'   \item searches for the controlled-description section;
#'   \item searches for the free-description section;
#'   \item stores extracted text in named lists using the scientific name.
#' }
#'
#' Failed extractions are returned together with the extracted descriptions.
#'
#' @param taxa A data frame containing the taxa to process. It must include at
#'   least the columns \code{genus}, \code{specificEpithet}, and
#'   \code{references}. Each row is treated as one taxon page to download and
#'   parse.
#'
#' @param delay Numeric. Delay in seconds passed to \code{webshot::webshot()} to
#'   allow the page to load before the PDF is captured. Default is \code{10}.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints informative progress messages
#' during parsing. If \code{FALSE}, runs quietly.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{descriptions_controlled}}{A named list of controlled
#'   descriptions. If any failures occur, this element becomes a list of length
#'   two: extracted descriptions and a character vector of failed taxon names.}
#'   \item{\code{descriptions_free}}{A named list of free descriptions. If any
#'   failures occur, this element becomes a list of length two: extracted
#'   descriptions and a character vector of failed taxon names.}
#' }
#'
#' @details
#' The function assumes that the printable FFB page contains recognizable
#' section headers in Portuguese and/or English, such as:
#' \itemize{
#'   \item \code{"Descrição com campos controlados|Description with controlled"}
#'   \item \code{"Descrição livre|Free description"}
#'   \item \code{"Comentários|Commentaries"}
#'   \item \code{"Vouchers"}
#'   \item \code{"Origem|Origin"}
#'   \item \code{"Bibliografia Referência|Reference"}
#' }
#'
#' Text is extracted from the first PDF page only. Temporary output is always
#' written to a file named \code{temp.pdf} in the working directory.
#'
#' @seealso \code{\link{flora_get_taxa}}, \code{\link{flora_build_matrix}}
#'
#' @examples
#' \dontrun{
#' taxa <- flora_get_taxa(
#'   family = "Fabaceae",
#'   genus = "Luetzelburgia",
#'   data = flora,
#'   check.correct = FALSE
#' )
#'
#' descriptions <- flora_get_descriptions(taxa, delay = 10)
#' }
#'
#' @importFrom webshot webshot
#' @importFrom pdftools pdf_text
#'
#' @export

flora_get_descriptions <- function(taxa=NULL,
                                   delay=10,
                                   verbose = TRUE) {
  ## If there is any error, change the timeout in the javascript

  scientificName <- paste(taxa$genus, taxa$specificEpithet)

  vector("list", length = nrow(taxa)) -> descriptions_free -> descriptions_control
  names(descriptions_free) <- scientificName
  names(descriptions_control) <- scientificName
  vector() -> failures_free -> failures_control

  for (i in 1:nrow(taxa)) {

    try(unlink("temp.pdf"))

    taxa$references[i] -> u0
    paste(u0, "&action=print", sep="") -> u0

    try(webshot::webshot(u0, file="temp.pdf", delay=delay, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X"))

    if (file.exists("temp.pdf")) {

      try(pdftools::pdf_text("temp.pdf")) -> txt
      unlist(strsplit(as.character(txt[[1]]), "\n", fixed=T)) -> d0

      # Description within controlled fields
      ini.m = "Descrição com campos controlados|Description with controlled"
      final.m1 = "Descrição livre|Free description"
      final.m2 = "Comentários|Commentaries"
      final.m3 = "Vouchers"
      final.m5 = "Origem|Origin"
      final.m4 = "Bibliografia Referência|Reference"

      grep(ini.m, d0)+1 -> i0
      if (length(i0) > 0) {
        grep(final.m1, d0)-1 -> i1
        if (length(i1) == 0) {
          grep(final.m2, d0)-1 -> i1
        }
        if (length(i1) == 0) {
          grep(final.m3, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        if (length(i1) == 0) {
          grep(final.m4, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        if (length(i1) == 0) {
          grep(final.m5, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        d0[i0:i1] -> d0
        gsub("PT\r", "", d0, fixed=T) -> d0
        gsub("EN\r", "", d0, fixed=T) -> d0
        gsub("\r", "", d0, fixed=T) -> d0
        trimws(d0) -> d0
        paste(d0, collapse = " ") -> d0
        trimws(d0) -> d0
        d0 -> descriptions_control[[i]]
      } else {
        c(failures_control, scientificName[i]) -> failures_control
      }

      # Description within free field
      ini.m = "Descrição livre|Free description"
      final.m1 = "xxxxxxxxxx"
      final.m2 = "Comentários|Commentaries"
      final.m3 = "Vouchers"
      final.m5 = "Origem|Origin"
      final.m4 = "Bibliografia Referência|Reference"
      unlist(strsplit(as.character(txt[[1]]), "\n", fixed=T)) -> d0

      grep(ini.m, d0)+1 -> i0
      if (length(i0) > 0) {
        grep(final.m1, d0)-1 -> i1
        if (length(i1) == 0) {
          grep(final.m2, d0)-1 -> i1
        }
        if (length(i1) == 0) {
          grep(final.m3, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        if (length(i1) == 0) {
          grep(final.m4, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        if (length(i1) == 0) {
          grep(final.m5, d0)[1]-1 -> i1
          na.omit(i1) -> i1
        }
        d0[i0:i1] -> d0
        gsub("PT\r", "", d0, fixed=T) -> d0
        gsub("EN\r", "", d0, fixed=T) -> d0
        gsub("\r", "", d0, fixed=T) -> d0
        trimws(d0) -> d0
        paste(d0, collapse = " ") -> d0
        trimws(d0) -> d0
        d0 -> descriptions_free[[i]]
      } else {
        c(failures_free, scientificName[i]) -> failures_free
      }

    } else {
      c(failures_control, scientificName[i]) -> failures_control
      c(failures_free, scientificName[i]) -> failures_free
    }

    if (verbose) {
    message(paste0("Extracted ", i, "/", length(scientificName),": ", scientificName[i]))
    }
    # end for loop
  }

  if (length(failures_control) > 0) {
    list(descriptions_control, failures_control) -> descriptions_control
  }

  if (length(failures_free) > 0) {
    list(descriptions_free, failures_free) -> descriptions_free
  }

  descriptions <- list(descriptions_control, descriptions_free)
  names(descriptions) <- c("descriptions_controlled", "descriptions_free")

  return(descriptions)
}

