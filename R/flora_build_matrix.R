#' Build a character matrix from extracted descriptions
#'
#' Converts extracted Flora e Funga do Brasil description text into a matrix with
#' taxa as rows, from either kind of description returned by
#' \code{\link{flora_get_descriptions}}:
#' \itemize{
#'   \item \strong{controlled descriptions} (\code{descriptions[[1]]}) become a
#'     taxon-by-character-state matrix, one column per state;
#'   \item \strong{free descriptions} (\code{descriptions[[2]]}) become a
#'     taxon-by-organ matrix, one column per organ (e.g. Habit, Leaf, Panicle,
#'     Flower, Samara, Seed), each cell holding that organ's description.
#' }
#'
#' @param descriptions A named list of description strings, usually one of the
#'   outputs from \code{flora_get_descriptions()}, such as \code{descriptions[[1]]}
#'   or \code{descriptions[[2]]}. If failures were returned together with
#'   descriptions, the function uses only the extracted-description component.
#'
#' @param type Character. Which kind of descriptions these are: \code{"auto"}
#'   (default) detects it from the text, or set \code{"controlled"} or
#'   \code{"free"} explicitly.
#'
#' @return A character matrix with taxa as rows: for controlled descriptions, one
#'   column per character state; for free descriptions, one column per organ.
#'
#' @details
#' If \code{descriptions} is the list returned by \code{flora_get_descriptions()}
#' when some taxa failed - an unnamed list of the extracted descriptions and the
#' failed taxon names - only the extracted descriptions are used. In both cases,
#' \code{NULL} descriptions are removed before the matrix is built.
#'
#' \strong{Controlled descriptions} follow a fixed template, in which character
#' categories are separated by periods, category names are separated from their
#' values by colons, and multiple states are separated by semicolons (e.g.
#' \code{"Leaf: leaflet number 3 to 7; leaflet margin entire. Flower: ..."}). The
#' first description defines the matrix columns; a taxon whose description does
#' not match that template is left empty.
#'
#' \strong{Free descriptions} are prose written one sentence (or more) per organ,
#' each starting with the organ's name (e.g. \code{"Tree to 10 m; branchlets
#' sericeous. Leaves 12-30 cm long, ... Panicles erect, ... Seeds 12-25 mm long."}).
#' The first sentence is the \code{Habit}; every later sentence is assigned to the
#' organ named by its first word, singularised so that e.g. "Seeds" and "Seed"
#' share one column. Columns are aligned by organ name across all taxa, so taxa
#' that describe different organs or the same organs in another order still line
#' up; an organ a taxon does not describe is \code{NA}. Sentences are split at a
#' period followed by a capital letter, but not after a person's initial (as in a
#' cited specimen, "M. Nee 34997") or a common abbreviation ("cf.", "ca.",
#' "e.g.", "var."), and decimals ("1.5 m") never split. As a safeguard, a sentence
#' starting with a word that opens sentences in fewer than a third of the taxa is
#' not treated as an organ, and is appended to the preceding organ's text.
#'
#' @seealso \code{\link{flora_get_descriptions}}
#'
#' @author
#' Marcelo Reginato & Domingos Cardoso
#'
#' @examples
#' \dontrun{
#' taxa <- flora_records(taxon = "Luetzelburgia",
#'                       taxonRank = "ESPECIE",
#'                       taxonomicStatus = "NOME_ACEITO")
#' descriptions <- flora_get_descriptions(taxa, delay = 10)
#'
#' # Taxon-by-character-state matrix from the controlled descriptions
#' mat_control <- flora_build_matrix(descriptions[[1]])
#'
#' # Taxon-by-organ matrix from the free descriptions
#' mat_free <- flora_build_matrix(descriptions[[2]])
#' }
#'
#' @export

flora_build_matrix <- function(descriptions = NULL, type = c("auto", "controlled", "free")) {

  type <- match.arg(type)

  # flora_get_descriptions() returns list(descriptions, failed_names) when some
  # taxa failed: an unnamed list whose second element is a character vector.
  # (A plain list of exactly two descriptions is named, so it is not mistaken
  # for that.)
  if (length(descriptions) == 2 && is.null(names(descriptions)) &&
      is.list(descriptions[[1]]) && is.character(descriptions[[2]])) {
    descriptions <- descriptions[[1]]
  }
  descriptions <- descriptions[!vapply(descriptions, is.null, logical(1))]
  if (length(descriptions) == 0) {
    stop("'descriptions' contains no extracted description.", call. = FALSE)
  }
  descriptions <- lapply(descriptions, .clean_description_text)

  if (type == "auto") type <- .description_type(descriptions)

  if (type == "controlled") {
    mat <- .buildMatrix(descriptions)
    mat[, dim(mat)[2]] <- gsub("[.]$", "", mat[, dim(mat)[2]])
  } else {
    mat <- .buildMatrixFree(descriptions)
  }

  return(mat)
}


# Normalise whitespace in a description: FFB pages use non-breaking spaces
# (U+00A0) in place of ordinary ones in some descriptions, including after
# sentence-ending periods
.clean_description_text <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}


# Controlled descriptions label each category with a colon at the start of
# its sentence ("Leaf: ..."); free descriptions are prose without such labels
.description_type <- function(descriptions) {
  labelled <- vapply(descriptions, function(x) {
    s <- .split_sentences(x)
    mean(grepl("^[^.:;]{1,40}:", s))
  }, numeric(1))
  if (mean(labelled) >= 0.5) "controlled" else "free"
}


# Split prose into sentences: at a period followed by a space and a capital
# letter, so decimals ("1.5 m") and abbreviations before a number ("c. 3 m")
# do not end a sentence. A person's initial ("M. Nee 34997") and common
# abbreviations ("cf.", "ca.", "e.g.", "var.") do not end one either.
.split_sentences <- function(x) {
  not_after <- paste0("(?<!\\b\\p{Lu}\\.)", "(?<!\\bcf\\.)(?<!\\bca\\.)(?<!\\bvs\\.)",
                      "(?<!\\bsp\\.)(?<!\\bvar\\.)(?<!\\bsubsp\\.)(?<!\\bssp\\.)",
                      "(?<!\\be\\.g\\.)(?<!\\bi\\.e\\.)(?<!\\bet al\\.)")
  s <- trimws(strsplit(x, paste0(not_after, "(?<=\\.)\\s+(?=\\p{Lu})"), perl = TRUE)[[1]])
  s[nzchar(s)]
}


# Singular, capitalised organ label from a sentence's first word ("Leaves" ->
# "Leaf", "Samaras" -> "Samara", "Seeds" -> "Seed")
.organ_label <- function(word) {
  w <- tolower(gsub("[^[:alpha:]-]", "", word))
  w <- sub("ves$", "f", w)
  w <- sub("ies$", "y", w)
  w <- sub("([^s])s$", "\\1", w)
  paste0(toupper(substr(w, 1, 1)), substring(w, 2))
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


# Internal parser to build a taxon-by-organ matrix from free descriptions:
# the first sentence is the habit, each later sentence belongs to the organ
# named by its first word, and columns are aligned by organ across taxa
.buildMatrixFree <- function(descriptions = NULL) {

  parsed <- lapply(descriptions, function(x) {
    s <- .split_sentences(x)
    first <- vapply(strsplit(s, "[[:space:],;:]"), `[`, character(1), 1)
    data.frame(organ = c("Habit", vapply(first[-1], .organ_label, character(1))),
               text = s, stringsAsFactors = FALSE)
  })

  # An organ is a label that opens sentences in at least a third of the taxa;
  # rarer labels (e.g. a collector cited mid-description) are not organs
  n_taxa <- length(parsed)
  taxa_with <- table(unlist(lapply(parsed, function(p) unique(p$organ))))
  organs <- names(taxa_with)[taxa_with >= max(1, ceiling(n_taxa / 3))]
  organs <- union("Habit", organs)

  # Column order: the order in which organs are described, on average
  # (unname() so that unlist() keeps the organ labels as names, without
  # prefixing the taxon names)
  pos <- unlist(unname(lapply(parsed, function(p) {
    stats::setNames(seq_len(nrow(p)) / nrow(p), p$organ)
  })))
  mean_pos <- tapply(pos, names(pos), mean)
  organs <- c("Habit", setdiff(organs[order(mean_pos[organs])], "Habit"))

  dat <- matrix(NA_character_, nrow = n_taxa, ncol = length(organs),
                dimnames = list(names(descriptions), organs))

  for (i in seq_len(n_taxa)) {
    p <- parsed[[i]]
    # Sentences that do not start an organ belong to the preceding one
    current <- "Habit"
    for (k in seq_len(nrow(p))) {
      if (p$organ[k] %in% organs) current <- p$organ[k]
      text <- sub("[.]$", "", p$text[k])
      dat[i, current] <- if (is.na(dat[i, current])) text else paste0(dat[i, current], ". ", text)
    }
  }
  dat
}
