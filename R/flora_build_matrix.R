#' Build a character matrix from extracted descriptions
#'
#' Converts extracted Flora e Funga do Brasil description text into a character
#' matrix, where rows correspond to taxa and columns correspond to character
#' states parsed from the description strings.
#'
#' This function is a wrapper around an internal parser that removes
#' \code{NULL} entries and handles the nested output structure returned by
#' \code{flora_get_descriptions()} when failures are present.
#'
#' @param descriptions A named list of description strings, usually one of the
#'   outputs from \code{flora_get_descriptions()}, such as
#'   \code{descriptions[[1]]}. If failures were returned together with
#'   descriptions, the function uses only the extracted-description component.
#'
#' @return A character matrix with taxa as rows and parsed character states as
#'   columns.
#'
#' @details
#' If \code{descriptions} has length 2, the function assumes the first element
#' contains the actual description list and the second element contains failed
#' taxon names. In both cases, \code{NULL} descriptions are removed before the
#' matrix is built.
#'
#' Description strings are expected to follow a semi-structured format in which
#' character categories are separated by periods, category names are separated
#' from their values by colons, and multiple states are separated by semicolons.
#' The first description is used to define the expected matrix structure and
#' column names.
#'
#' @seealso \code{\link{flora_get_descriptions}}
#'
#' @examples
#' \dontrun{
#' descriptions <- flora_get_descriptions(taxa, delay = 10)
#' mat_control <- flora_build_matrix(descriptions[[1]])
#' }
#' 
#' @export 

flora_build_matrix <- function(descriptions=NULL) {
  
  if (length(descriptions) == 2) {
    descriptions[[1]] -> x
    #length(x)
    x[which(lapply(x, is.null) == F)] -> descriptions[[1]]
    #length(descriptions[[1]])
    descriptions[[1]] -> descriptions
    .buildMatrix(descriptions) -> mat
  } else {
    #length(descriptions) 
    descriptions[which(lapply(descriptions, is.null) == F)] -> descriptions
    #length(descriptions) 
    .buildMatrix(descriptions) -> mat
  }
  
  return(mat)
}


# Internal parser to build a matrix from semi-structured descriptions
.buildMatrix <- function(descriptions=NULL) {
  descriptions[[1]] -> d0
  trimws(unlist(strsplit(d0, ".", fixed=T))) -> d0
  unlist(lapply(strsplit(d0, ":"), "[", 1)) -> chars.main
  unlist(lapply(strsplit(d0, ";"), length)) -> n
  rep(chars.main, n) -> chars.all
  
  dat <- matrix(ncol=length(chars.all), nrow=length(descriptions))
  colnames(dat) <- chars.all
  rownames(dat) <- names(descriptions)
  
  for (i in 1:length(descriptions)) {
    descriptions[[i]] -> d0
    trimws(unlist(strsplit(d0, ". ", fixed=T))) -> d0
    d0[1:length(chars.main)] -> d0
    unlist(lapply(strsplit(d0, ": "), "[", 2)) -> d0
    trimws(unlist(strsplit(d0, ";"))) -> d0
    if (length(d0) == ncol(dat)) {
      d0 -> dat[i, ]
    }
  }
  return(dat)
}
