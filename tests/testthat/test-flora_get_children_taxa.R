.flora_children_fixture <- function() {
  # Fabaceae (family) -> Inga, Mimosa (genera) -> species
  data.frame(
    id = as.character(1:7),
    taxonName = c("Fabaceae", "Inga", "Mimosa", "Inga edulis", "Inga vera",
                  "Mimosa pudica", "Mimosa fakesynonym"),
    taxonRank = c("FAMILIA", "GENERO", "GENERO", "ESPECIE", "ESPECIE",
                  "ESPECIE", "ESPECIE"),
    family = rep("Fabaceae", 7),
    genus = c(NA, "Inga", "Mimosa", "Inga", "Inga", "Mimosa", "Mimosa"),
    class = rep("Magnoliopsida", 7),
    order = rep("Fabales", 7),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO", "NOME_ACEITO",
                        "NOME_ACEITO", "NOME_ACEITO", "SINONIMO"),
    parentNameUsageID = c(NA, "1", "1", "2", "2", "3", "3"),
    stringsAsFactors = FALSE
  )
}

.mock_flora_children <- function(fixture, env = parent.frame()) {
  dwca <- list(dwca_ffb_v393_001_latest = list(data = list(taxon.txt = fixture)))
  testthat::local_mocked_bindings(
    flora_download = function(version, dir, verbose) invisible(NULL),
    flora_parse = function(path, version, verbose) dwca,
    .package = "floraR",
    .env = env
  )
  # flora_get_children_taxa() re-derives the dwca key via
  # list.files("flora_download") against the real working directory, even
  # though flora_download()/flora_parse() are otherwise mocked above.
  withr::local_dir(withr::local_tempdir(.local_envir = env), .local_envir = env)
  dir.create(file.path("flora_download", names(dwca)), recursive = TRUE, showWarnings = FALSE)
}


test_that("flora_get_children_taxa returns direct species children of a genus", {
  .mock_flora_children(.flora_children_fixture())

  result <- flora_get_children_taxa(taxon_name = "Inga", rank = "genus",
                                    child_rank = "species", verbose = FALSE)
  expect_setequal(result$taxonName, c("Inga edulis", "Inga vera"))
})


test_that("flora_get_children_taxa returns genera for a family", {
  .mock_flora_children(.flora_children_fixture())

  result <- flora_get_children_taxa(taxon_name = "Fabaceae", rank = "family",
                                    child_rank = "genus", verbose = FALSE)
  expect_setequal(result$taxonName, c("Inga", "Mimosa"))
})


test_that("flora_get_children_taxa excludes synonyms by default", {
  .mock_flora_children(.flora_children_fixture())

  result <- flora_get_children_taxa(taxon_name = "Mimosa", rank = "genus",
                                    child_rank = "species", verbose = FALSE)
  expect_equal(result$taxonName, "Mimosa pudica")
})


test_that("flora_get_children_taxa includes synonyms when requested", {
  .mock_flora_children(.flora_children_fixture())

  result <- flora_get_children_taxa(taxon_name = "Mimosa", rank = "genus",
                                    child_rank = "species", include_synonyms = TRUE,
                                    verbose = FALSE)
  expect_setequal(result$taxonName, c("Mimosa pudica", "Mimosa fakesynonym"))
})


test_that("flora_get_children_taxa returns all descendant ranks when child_rank is NULL", {
  .mock_flora_children(.flora_children_fixture())

  # Starting from "genus", the recursive descent only needs a single hop
  # (genus -> species), so it returns every species below it.
  result <- flora_get_children_taxa(taxon_name = "Inga", rank = "genus",
                                    child_rank = NULL, verbose = FALSE)
  expect_setequal(result$taxonName, c("Inga edulis", "Inga vera"))
})


test_that("flora_get_children_taxa falls back to direct genera when an intermediate rank (e.g. tribe) has no records", {
  .mock_flora_children(.flora_children_fixture())

  # The fixture has no "tribe"-rank taxa between family and genus, so the
  # recursive descent from "family" with child_rank = NULL stops at the
  # first empty intermediate rank; only the family->genus fallback applies,
  # so species are not reached in this scenario.
  result <- flora_get_children_taxa(taxon_name = "Fabaceae", rank = "family",
                                    child_rank = NULL, verbose = FALSE)
  expect_setequal(result$taxonName, c("Inga", "Mimosa"))
})


test_that("flora_get_children_taxa errors when the parent taxon is not found", {
  .mock_flora_children(.flora_children_fixture())

  expect_error(
    flora_get_children_taxa(taxon_name = "Nonexistentaceae", rank = "family",
                            child_rank = "genus", verbose = FALSE),
    "not found in FFB database"
  )
})


test_that("flora_get_children_taxa errors when child_rank is not lower than the parent rank", {
  .mock_flora_children(.flora_children_fixture())

  expect_error(
    flora_get_children_taxa(taxon_name = "Inga", rank = "genus",
                            child_rank = "family", verbose = FALSE),
    "must be lower than parent rank"
  )
})


test_that("flora_get_children_taxa warns and returns an empty data.frame when nothing matches", {
  .mock_flora_children(.flora_children_fixture())

  expect_warning(
    result <- flora_get_children_taxa(taxon_name = "Inga", rank = "genus",
                                      child_rank = "subspecies", verbose = FALSE),
    "No children found"
  )
  expect_equal(nrow(result), 0)
})


test_that("flora_get_children_taxa requires a non-null taxon_name", {
  expect_error(flora_get_children_taxa(taxon_name = NULL), "must be provided")
})
