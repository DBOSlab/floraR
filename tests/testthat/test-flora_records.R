.flora_records_fixture <- function() {
  taxon_df <- data.frame(
    id = as.character(1:6),
    family = c("Fabaceae", "Fabaceae", "Fabaceae", "Asteraceae", "Asteraceae", "Fabaceae"),
    genus = c("Inga", "Inga", "Mimosa", "Aster", "Aster", "Inga"),
    taxonName = c("Inga edulis", "Inga vera", "Mimosa pudica", "Aster alba",
                  "Aster beta", "Inga synonym"),
    taxonRank = rep("ESPECIE", 6),
    taxonomicStatus = c(rep("NOME_ACEITO", 5), "SINONIMO"),
    stringsAsFactors = FALSE
  )
  # id=1 Inga edulis: Bahia + Minas Gerais, endemic
  # id=2 Inga vera: Bahia, endemic
  # id=3 Mimosa pudica: Sao Paulo, not endemic
  # id=4 Aster alba: Amazonas, not endemic
  # id=5 Aster beta: Para, not endemic
  # id=6 Inga synonym: no distribution row (synonym, excluded from most filters anyway)
  distribution_df <- data.frame(
    id = c("1", "1", "2", "3", "4", "5"),
    locationID = c("BR-BA", "BR-MG", "BR-BA", "BR-SP", "BR-AM", "BR-PA"),
    phytogeographicDomain = c("Caatinga", "Mata Atlantica", "Caatinga", "Cerrado",
                              "Amazonia", "Amazonia"),
    endemism = c("true", "true", "true", "false", "false", "false"),
    stringsAsFactors = FALSE
  )
  speciesprofile_df <- data.frame(
    id = c("1", "2", "3"),
    lifeForm = c("Arbusto", "Arvore", "Erva"),
    habitat = c("Terricola", "Terricola", "Terricola"),
    vegetationType = c("Caatinga (stricto sensu)", "Caatinga (stricto sensu)",
                       "Cerrado (lato sensu)"),
    stringsAsFactors = FALSE
  )
  list(taxon_df = taxon_df, distribution_df = distribution_df, speciesprofile_df = speciesprofile_df)
}

.mock_flora_records <- function(fixture, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .flora_prepare_records = function(version, verbose, rm_flora_database) fixture,
    .package = "floraR",
    .env = env
  )
}


test_that("flora_records with no filters returns the full taxon table", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  result <- flora_records(verbose = FALSE)
  expect_equal(nrow(result), 6)
})


test_that("flora_records filters by taxon (family, genus, species)", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  fam <- flora_records(taxon = "Asteraceae", verbose = FALSE)
  expect_setequal(fam$taxonName, c("Aster alba", "Aster beta"))

  gen <- flora_records(taxon = "Mimosa", verbose = FALSE)
  expect_equal(gen$taxonName, "Mimosa pudica")

  sp <- flora_records(taxon = "Inga edulis", verbose = FALSE)
  expect_equal(sp$taxonName, "Inga edulis")
})


test_that("flora_records filters by taxonRank and taxonomicStatus", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  accepted <- flora_records(taxonomicStatus = "NOME_ACEITO", verbose = FALSE)
  expect_false("Inga synonym" %in% accepted$taxonName)
  expect_equal(nrow(accepted), 5)

  synonyms <- flora_records(taxonomicStatus = "sinonimo", verbose = FALSE)
  expect_equal(synonyms$taxonName, "Inga synonym")

  species_rank <- flora_records(taxonRank = "ESPECIE", verbose = FALSE)
  expect_equal(nrow(species_rank), 6)
})


test_that("flora_records filters by state via the distribution table", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  bahia <- flora_records(state = "Bahia", verbose = FALSE)
  expect_setequal(bahia$taxonName, c("Inga edulis", "Inga vera"))

  # Acronym and diacritics-insensitive input should behave the same way
  bahia_abbrev <- flora_records(state = "BA", verbose = FALSE)
  expect_setequal(bahia_abbrev$taxonName, c("Inga edulis", "Inga vera"))

  sp_state <- flora_records(state = "Sao Paulo", verbose = FALSE)
  expect_equal(sp_state$taxonName, "Mimosa pudica")
})


test_that("flora_records filters by phytogeographicDomain", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  caatinga <- flora_records(phytogeographicDomain = "Caatinga", verbose = FALSE)
  expect_setequal(caatinga$taxonName, c("Inga edulis", "Inga vera"))
})


test_that("flora_records filters by endemism", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  endemics <- flora_records(endemism = TRUE, verbose = FALSE)
  expect_setequal(endemics$taxonName, c("Inga edulis", "Inga vera"))

  non_endemics <- flora_records(endemism = FALSE, verbose = FALSE)
  expect_setequal(non_endemics$taxonName, c("Mimosa pudica", "Aster alba", "Aster beta"))
})


test_that("flora_records filters by lifeForm, habitat, and vegetationType", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  shrubs <- flora_records(lifeForm = "Arbusto", verbose = FALSE)
  expect_equal(shrubs$taxonName, "Inga edulis")

  cerrado_veg <- flora_records(vegetationType = "Cerrado (lato sensu)", verbose = FALSE)
  expect_equal(cerrado_veg$taxonName, "Mimosa pudica")
})


test_that("flora_records combines multiple filters with AND logic", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  result <- flora_records(phytogeographicDomain = "Caatinga",
                          lifeForm = "Arbusto",
                          verbose = FALSE)
  expect_equal(result$taxonName, "Inga edulis")

  none <- flora_records(phytogeographicDomain = "Amazonia",
                        lifeForm = "Arbusto",
                        verbose = FALSE)
  expect_equal(nrow(none), 0)
})


test_that("flora_records saves a CSV file when save = TRUE", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  tmp_dir <- tempfile("flora_records_save_")
  result <- flora_records(taxon = "Fabaceae",
                          save = TRUE,
                          dir = tmp_dir,
                          filename = "fabaceae_records",
                          verbose = FALSE)

  out <- file.path(tmp_dir, "fabaceae_records.csv")
  expect_true(file.exists(out))
  expect_gt(nrow(result), 0)

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_records prints a summary message when verbose = TRUE", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  expect_message(flora_records(taxon = "Fabaceae", verbose = TRUE),
                 "Returned \\d+ taxon record")
})


test_that(".flora_prepare_records extracts taxon/distribution/speciesprofile tables", {
  dwca <- list(
    dwca_ffb_v1 = list(data = list(
      taxon.txt = data.frame(id = "1", taxonName = "Inga edulis", stringsAsFactors = FALSE),
      distribution.txt = data.frame(id = "1", locationID = "BR-BA", stringsAsFactors = FALSE),
      speciesprofile.txt = data.frame(id = "1", lifeForm = "Arbusto", stringsAsFactors = FALSE)
    ))
  )

  testthat::local_mocked_bindings(
    flora_download = function(version, dir, verbose) invisible(NULL),
    flora_parse = function(path, version, verbose) dwca,
    .package = "floraR"
  )

  result <- .flora_prepare_records(version = "latest", verbose = FALSE, rm_flora_database = FALSE)
  expect_equal(result$taxon_df$taxonName, "Inga edulis")
  expect_equal(result$distribution_df$locationID, "BR-BA")
  expect_equal(result$speciesprofile_df$lifeForm, "Arbusto")
})
