.flora_match_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:4),
    family = c("Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae"),
    genus = c("Inga", "Mimosa", "Mimosa", "Swartzia"),
    specificEpithet = c("edulis", "pudica", "pudica", "simplex"),
    infraspecificEpithet = c(NA, NA, NA, "grandiflora"),
    taxonName = c("Inga edulis", "Mimosa pudica", "Mimosa pyrenea",
                  "Swartzia simplex var. grandiflora"),
    taxonRank = c("ESPECIE", "ESPECIE", "ESPECIE", "VARIEDADE"),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "SINONIMO", "NOME_ACEITO"),
    acceptedNameUsageID = c(NA, NA, "2", NA),
    scientificNameAuthorship = c("Mart.", "L.", "Poir.", "(Moric.) R.S.Cowan"),
    order = rep("Fabales", 4),
    stringsAsFactors = FALSE
  )

  genus_index <- .flora_build_genus_index(taxon_df)
  id_lookup <- tapply(seq_len(nrow(taxon_df)), taxon_df$id, unique, simplify = FALSE)

  list(taxon_df = taxon_df, genus_index = genus_index, id_lookup = id_lookup)
}

.mock_flora_match <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .flora_prepare_taxon = function(version, verbose, rm_flora_database) fixture,
    .package = "floraR",
    .env = env
  )
}


test_that("flora_match aligns two name lists that resolve to the same accepted taxon", {
  .mock_flora_match(.flora_match_fixture())

  splist1 <- c("Inga edulis", "Mimosa pudica")
  splist2 <- c("Mimosa pyrenea", "Inga edulis")  # order intentionally swapped

  result <- flora_match(splist1, splist2, progress_bar = FALSE, verbose = FALSE)

  expect_equal(result$Species.List.1, splist1)
  # Mimosa pudica (splist1) should align with Mimosa pyrenea (splist2), a synonym
  expect_equal(result$Species.List.2[result$Species.List.1 == "Mimosa pudica"], "Mimosa pyrenea")
  expect_equal(result$Species.List.2[result$Species.List.1 == "Inga edulis"], "Inga edulis")
})


test_that("flora_match appends splist2-only names when include_all = TRUE", {
  .mock_flora_match(.flora_match_fixture())

  splist1 <- c("Inga edulis")
  splist2 <- c("Inga edulis", "Swartzia simplex var. grandiflora")

  result <- flora_match(splist1, splist2, include_all = TRUE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_true("Swartzia simplex var. grandiflora" %in% result$Species.List.2)
  extra_row <- result[result$Species.List.2 == "Swartzia simplex var. grandiflora", ]
  expect_true(is.na(extra_row$Species.List.1))
})


test_that("flora_match omits splist2-only names when include_all = FALSE", {
  .mock_flora_match(.flora_match_fixture())

  splist1 <- c("Inga edulis")
  splist2 <- c("Inga edulis", "Swartzia simplex var. grandiflora")

  result <- flora_match(splist1, splist2, include_all = FALSE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 1)
})


test_that("flora_match flags duplicated accepted names when identify_dups = TRUE", {
  .mock_flora_match(.flora_match_fixture())

  splist1 <- c("Mimosa pudica", "Mimosa pyrenea")  # both resolve to Mimosa pudica
  splist2 <- c("Inga edulis", "Inga edulis")

  result <- flora_match(splist1, splist2, identify_dups = TRUE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_true("Duplicated.Output.Position" %in% names(result))
  expect_false(all(is.na(result$Duplicated.Output.Position)))
})


test_that("flora_match provides Match.Position.2to1 to re-order splist2", {
  .mock_flora_match(.flora_match_fixture())

  splist1 <- c("Inga edulis", "Mimosa pudica")
  splist2 <- c("Mimosa pudica", "Inga edulis")

  result <- flora_match(splist1, splist2, include_all = FALSE,
                        progress_bar = FALSE, verbose = FALSE)

  expect_equal(splist2[result$Match.Position.2to1], splist1)
})


test_that("flora_match errors when splist1 has no matches at all", {
  .mock_flora_match(.flora_match_fixture())

  expect_error(
    suppressWarnings(
      flora_match("Totallyfake completelynotreal", "Inga edulis",
                 max_distance = 0.01, progress_bar = FALSE, verbose = FALSE)
    ),
    "No match found for splist1"
  )
})
