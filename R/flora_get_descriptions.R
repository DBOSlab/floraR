#' Extract species descriptions from Flora e Funga do Brasil pages
#'
#' Downloads printable PDF versions of Flora e Funga do Brasil (FFB) taxon pages
#' for a set of species and extracts text from the description sections. The
#' function attempts to recover two kinds of descriptions from each page:
#' controlled descriptions ("Descricao com campos controlados") and free
#' descriptions ("Descricao livre").
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
#' @param delay Numeric. Delay in seconds passed to \code{webshot2::webshot()} to
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
#'   \item \code{"Descricao com campos controlados|Description with controlled"}
#'   \item \code{"Descricao livre|Free description"}
#'   \item \code{"Comentarios|Commentaries"}
#'   \item \code{"Vouchers"}
#'   \item \code{"Origem|Origin"}
#'   \item \code{"Bibliografia Referencia|Reference"}
#' }
#'
#' Text is extracted from every page of the PDF, so a description that runs
#' onto a following page is read in full. Each section ends at the next section
#' header found at the start of a line; if no closing header is found, the taxon
#' is recorded as a failure rather than stopping the whole run. Temporary output
#' is always written to a file named \code{temp.pdf} in the working directory.
#'
#' @seealso \code{\link{flora_get_children_taxa}}, \code{\link{flora_build_matrix}}
#'
#' @author
#' Marcelo Reginato & Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' taxa <- flora_records(taxon = "Luetzelburgia",
#'                       taxonRank = "ESPECIE",
#'                       taxonomicStatus = "NOME_ACEITO")
#'
#' descriptions <- flora_get_descriptions(taxa, delay = 10)
#' }
#'
#' @importFrom webshot2 webshot
#' @importFrom pdftools pdf_text
#' @importFrom stats na.omit
#'
#' @export

flora_get_descriptions <- function(taxa = NULL,
                                   delay = 10,
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

    try(webshot2::webshot(u0,
                          file = "temp.pdf",
                          delay = delay,
                          useragent = .ffb_http_headers()))

    if (file.exists("temp.pdf")) {

      try(pdftools::pdf_text("temp.pdf")) -> txt

      # Read every page: a description often runs onto the next page, where
      # its closing section header then sits
      d0 <- unlist(strsplit(paste(as.character(txt), collapse = "\n"), "\n", fixed = TRUE))

      # Section headers (Portuguese or English pages)
      h_control <- "Descri\u00e7\u00e3o com campos controlados|Description with controlled"
      h_free <- "Descri\u00e7\u00e3o livre|Free description"
      h_after <- c("Coment\u00e1rios|Commentaries", "Vouchers",
                   "Bibliografia Refer\u00eancia|Reference", "Origem|Origin")

      # Description within controlled fields (ends at the free description,
      # or at whichever later section comes first)
      control <- .ffb_description_section(d0, h_control, c(h_free, h_after))
      if (!is.na(control)) {
        control -> descriptions_control[[i]]
      } else {
        c(failures_control, scientificName[i]) -> failures_control
      }

      # Description within free field
      free <- .ffb_description_section(d0, h_free, h_after)
      if (!is.na(free)) {
        free -> descriptions_free[[i]]
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

.ffb_http_headers <- function() {
  c(`User-Agent` = "Mozilla/5.0 (compatible; floraR/1; Linux)")
}


#_______________________________________________________________________________
# Extract one description section from the text lines of an FFB print page:
# the lines after the header matching 'start', up to the nearest following
# line that starts with any header in 'ends'. Headers are matched at the start
# of a line only, so the same words inside the description text do not cut it
# short. Language tag lines ("EN"/"PT") and blank lines are dropped. Returns
# the section as one string, or NA when the section or its end is not found. ####
.ffb_description_section <- function(lines, start, ends) {
  at_line_start <- function(pattern) paste0("^\\s*(", pattern, ")")
  i0 <- grep(at_line_start(start), lines)[1]
  if (is.na(i0)) return(NA_character_)

  next_headers <- unlist(lapply(ends, function(e) grep(at_line_start(e), lines)))
  next_headers <- next_headers[next_headers > i0]
  if (length(next_headers) == 0) return(NA_character_)
  i1 <- min(next_headers) - 1
  if (i1 <= i0) return(NA_character_)

  section <- trimws(gsub("\r", "", lines[(i0 + 1):i1], fixed = TRUE))
  section <- section[nzchar(section) & !section %in% c("EN", "PT")]
  if (length(section) == 0) return(NA_character_)
  trimws(paste(section, collapse = " "))
}
