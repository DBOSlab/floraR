#' Filter Flora e Funga do Brasil taxa by family and genus
#'
#' Filters a Flora e Funga do Brasil taxon table to return accepted species of a
#' given family and genus.
#'
#' @param family Character. Family name used to filter \code{data$family}.
#' @param genus Character. Genus name used to filter \code{data$genus}.
#' @param data A data frame containing Flora e Funga do Brasil taxon records.
#'   Expected columns include \code{family}, \code{taxonomicStatus},
#'   \code{nomenclaturalStatus}, \code{taxonRank}, and \code{genus}.
#' @param check.correct Logical. If \code{TRUE} (default), the function keeps
#'   only taxa with \code{nomenclaturalStatus == "NOME_CORRETO"}. If
#'   \code{FALSE}, this filter is skipped.
#'
#' @return A data frame containing taxa that match the requested family and
#'   genus after the applied filters.
#'
#' @details
#' The function always filters to:
#' \itemize{
#'   \item \code{family == family}
#'   \item \code{taxonomicStatus == "NOME_ACEITO"}
#'   \item \code{taxonRank == "ESPECIE"}
#'   \item \code{genus == genus}
#' }
#'
#' If \code{check.correct = TRUE}, an additional filter is applied:
#' \itemize{
#'   \item \code{nomenclaturalStatus == "NOME_CORRETO"}
#' }
#'
#' @seealso \code{\link{extractDescriptions}}
#'
#' @examples
#' \dontrun{
#' taxa <- flora_get_taxa(
#'   family = "Fabaceae",
#'   genus = "Luetzelburgia",
#'   data = flora,
#'   check.correct = FALSE
#' )
#' }
#' 
#' @export

flora_get_taxa <- function(family=NULL, genus=NULL, data=NULL, check.correct=T) {
  data -> taxons
  taxons[which(taxons$family == family),] -> taxons
  taxons[which(taxons$taxonomicStatus == "NOME_ACEITO"),] -> taxons
  if (check.correct) {
    taxons[which(taxons$nomenclaturalStatus == "NOME_CORRETO"),] -> taxons
  }
  taxons[which(taxons$taxonRank == "ESPECIE"),] -> taxons
  taxons[which(taxons$genus == genus),] -> taxa
  return(taxa)
}
