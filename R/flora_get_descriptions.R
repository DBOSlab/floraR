#' Extract species descriptions from Flora e Funga do Brasil pages
#'
#' Reads the printable versions of Flora e Funga do Brasil (FFB) taxon pages for
#' a set of species and extracts the text of their description sections. The
#' function attempts to recover two kinds of descriptions from each page:
#' controlled descriptions ("Descricao com campos controlados") and free
#' descriptions ("Descricao livre").
#'
#' For each taxon, the function:
#' \enumerate{
#'   \item builds a printable page URL from the \code{references} column;
#'   \item loads the page in a headless Chrome browser (via \pkg{chromote}) and
#'     reads its text;
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
#' @param delay Numeric. Seconds to wait after each page has loaded before its
#'   text is read, to let the page finish rendering. Default is \code{10}.
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
#' The text is read directly from the rendered page rather than from a PDF of it,
#' so no line can be cut off at a page margin (a PDF of the page clipped the
#' right end of every line, silently losing words), and a description is read in
#' full however long it is. Each section ends at the next section header found
#' at the start of a line; if no closing header is found, the taxon is recorded
#' as a failure rather than stopping the whole run. The language tag lines
#' ("EN"/"PT") are dropped.
#'
#' A Chrome or Chromium browser must be installed, as for \pkg{chromote}; one
#' browser session is opened for the whole run and closed at the end.
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
#' @importFrom chromote ChromoteSession
#' @importFrom stats na.omit
#'
#' @export

flora_get_descriptions <- function(taxa = NULL,
                                   delay = 10,
                                   verbose = TRUE) {

  scientificName <- paste(taxa$genus, taxa$specificEpithet)

  vector("list", length = nrow(taxa)) -> descriptions_free -> descriptions_control
  names(descriptions_free) <- scientificName
  names(descriptions_control) <- scientificName
  vector() -> failures_free -> failures_control

  # One headless browser session for the whole run, closed on exit
  session <- .ffb_browser_session()
  on.exit(try(session$close(), silent = TRUE), add = TRUE)

  # Section headers (Portuguese or English pages)
  h_control <- "Descri\u00e7\u00e3o com campos controlados|Description with controlled"
  h_free <- "Descri\u00e7\u00e3o livre|Free description"
  h_after <- c("Coment\u00e1rios|Commentaries", "Vouchers",
               "Bibliografia Refer\u00eancia|Reference", "Origem|Origin")

  for (i in 1:nrow(taxa)) {

    u0 <- paste(taxa$references[i], "&action=print", sep = "")
    page <- .ffb_page_text(session, u0, delay = delay)

    if (!is.na(page)) {

      d0 <- unlist(strsplit(page, "\n", fixed = TRUE))

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

#_______________________________________________________________________________
# Open a headless Chrome session for reading FFB pages, identifying floraR
# with its user agent. Stops with a clear message when no Chrome or Chromium
# browser is available. ####
.ffb_browser_session <- function() {
  session <- tryCatch(chromote::ChromoteSession$new(), error = function(e) {
    stop("flora_get_descriptions() needs a Chrome or Chromium browser to read the ",
         "FFB pages, and none could be started: ", conditionMessage(e), call. = FALSE)
  })
  try(session$Network$setUserAgentOverride(userAgent = unname(.ffb_http_headers())),
      silent = TRUE)
  session
}


#_______________________________________________________________________________
# Load one FFB page in the browser session and return its rendered text
# (document.body.innerText), after waiting 'delay' seconds for the page to
# finish rendering. Reading the text directly, rather than from a PDF of the
# page, keeps every word: the PDF clipped the right end of each line. Never
# errors - returns NA when the page cannot be loaded or read. ####
.ffb_page_text <- function(session, url, delay = 10) {
  tryCatch({
    loaded <- session$Page$loadEventFired(wait_ = FALSE)
    session$Page$navigate(url, wait_ = FALSE)
    session$wait_for(loaded)
    if (delay > 0) Sys.sleep(delay)
    txt <- session$Runtime$evaluate("document.body.innerText")$result$value
    if (is.null(txt) || !nzchar(txt)) NA_character_ else txt
  }, error = function(e) NA_character_)
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

  section <- gsub("\r", "", lines[(i0 + 1):i1], fixed = TRUE)
  # FFB pages use non-breaking spaces in place of ordinary ones in some descriptions
  section <- trimws(gsub("[[:space:]\u00a0]+", " ", section))
  section <- section[nzchar(section) & !section %in% c("EN", "PT")]
  if (length(section) == 0) return(NA_character_)
  trimws(paste(section, collapse = " "))
}
