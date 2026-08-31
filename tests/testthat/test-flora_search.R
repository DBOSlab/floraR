.flora_search_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:5),
    family = c("Fabaceae", "Fabaceae", "Fabaceae", "Malvaceae", "Fabaceae"),
    genus = c("Inga", "Inga", "Mimosa", "Hibiscus", "Mimosa"),
    specificEpithet = c("edulis", "vera", "pudica", "rosa-sinensis", "pudica"),
    infraspecificEpithet = c(NA, NA, NA, NA, NA),
    taxonName = c("Inga edulis", "Inga vera", "Mimosa pudica",
                  "Hibiscus rosa-sinensis", "Mimosa pyrenea"),
    taxonRank = rep("ESPECIE", 5),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO",
                        "NOME_ACEITO", "SINONIMO"),
    acceptedNameUsageID = c(NA, NA, NA, NA, "3"),
    scientificNameAuthorship = c("Mart.", "Willd.", "L.", "L.", "Poir."),
    order = rep("Fabales", 5),
    stringsAsFactors = FALSE
  )

  genus_index <- .flora_build_genus_index(taxon_df)
  id_lookup <- tapply(seq_len(nrow(taxon_df)), taxon_df$id, unique, simplify = FALSE)

  list(taxon_df = taxon_df, genus_index = genus_index, id_lookup = id_lookup)
}

.mock_flora_search <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .flora_prepare_taxon = function(version, verbose, rm_flora_database) fixture,
    .package = "floraR",
    .env = env
  )
}


test_that("flora_search finds exact matches", {
  .mock_flora_search(.flora_search_fixture())

  result <- flora_search("Inga edulis", progress_bar = FALSE, verbose = FALSE)
  expect_equal(result$Accepted.taxon.Name, "Inga edulis")
  expect_equal(result$taxonomicStatus, "NOME_ACEITO")
  expect_equal(result$family, "Fabaceae")
})


test_that("flora_search resolves a synonym to its accepted name", {
  .mock_flora_search(.flora_search_fixture())

  result <- flora_search("Mimosa pyrenea", progress_bar = FALSE, verbose = FALSE)
  expect_equal(result$taxonomicStatus, "SINONIMO")
  expect_equal(result$Accepted.taxon.Name, "Mimosa pudica")
})


test_that("flora_search performs fuzzy matching within max_distance", {
  .mock_flora_search(.flora_search_fixture())

  expect_warning(
    result <- flora_search("Inga edullis", max_distance = 0.2, genus_fuzzy = FALSE,
                           progress_bar = FALSE, verbose = FALSE, show_correct = TRUE),
    NA
  )
  expect_equal(result$Accepted.taxon.Name, "Inga edulis")
  expect_false(result$Correct.Spelling)
})


test_that("flora_search returns an NA row for an unmatched name mixed with a real match", {
  .mock_flora_search(.flora_search_fixture())

  expect_warning(
    result <- flora_search(c("Inga edulis", "Xyzabcus completelyfake"), max_distance = 0.1,
                           progress_bar = FALSE, verbose = FALSE),
    "No match found for 'Xyzabcus completelyfake'"
  )
  expect_equal(result$Accepted.taxon.Name[1], "Inga edulis")
  expect_true(is.na(result$FFB.taxon.ID[2]))
})


test_that("flora_search returns NULL and warns when a single name has no match at all", {
  .mock_flora_search(.flora_search_fixture())

  # Two warnings are raised in sequence: one for the specific unmatched name,
  # and one final summary warning since ALL inputs failed to match.
  expect_warning(
    expect_warning(
      result <- flora_search("Xyzabcus completelyfake", max_distance = 0.1,
                             progress_bar = FALSE, verbose = FALSE),
      "No match found for 'Xyzabcus completelyfake'"
    ),
    "No match found for any input name"
  )
  expect_null(result)
})


test_that("flora_search searches multiple names at once", {
  .mock_flora_search(.flora_search_fixture())

  splist <- c("Inga edulis", "Mimosa pudica")
  result <- flora_search(splist, progress_bar = FALSE, verbose = FALSE)
  expect_equal(nrow(result), 2)
  expect_equal(result$Search, splist)
})


test_that("flora_search warns when an input name lacks an epithet", {
  .mock_flora_search(.flora_search_fixture())

  expect_warning(
    result <- flora_search(c("Inga edulis", "Inga"), progress_bar = FALSE, verbose = FALSE),
    "does not include an epithet"
  )
  expect_true(is.na(result$FFB.taxon.ID[2]))
})
