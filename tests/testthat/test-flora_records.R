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
    establishmentMeans = c("Nativa", "Nativa", "Nativa", "Ex\u00f3tica", "Nativa", "Ex\u00f3tica"),
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


test_that("flora_records with endemism = TRUE and a state returns the endemics exclusive to it", {
  # Regression: endemism = TRUE with a state used to return every Brazilian
  # endemic that merely occurs in the state (Inga edulis occurs in BA and MG)
  .mock_flora_records(.flora_records_fixture())

  bahia_endemics <- flora_records(state = "Bahia", endemism = TRUE, verbose = FALSE)
  expect_equal(bahia_endemics$taxonName, "Inga vera")
})


test_that("flora_records with endemism = 'Brazil' returns the Brazilian endemics occurring in the areas", {
  .mock_flora_records(.flora_records_fixture())

  in_bahia <- flora_records(state = "Bahia", endemism = "Brazil", verbose = FALSE)
  expect_setequal(in_bahia$taxonName, c("Inga edulis", "Inga vera"))
  # Without an area, "Brazil" is the same as TRUE
  expect_setequal(flora_records(endemism = "Brazil", verbose = FALSE)$taxonName,
                  flora_records(endemism = TRUE, verbose = FALSE)$taxonName)
})


test_that("flora_records with endemism = FALSE and a state returns the taxa not exclusive to it", {
  .mock_flora_records(.flora_records_fixture())

  not_exclusive <- flora_records(state = "Bahia", endemism = FALSE, verbose = FALSE)
  expect_equal(not_exclusive$taxonName, "Inga edulis")
  # TRUE and FALSE partition the taxa occurring in the area
  expect_setequal(c(not_exclusive$taxonName,
                    flora_records(state = "Bahia", endemism = TRUE, verbose = FALSE)$taxonName),
                  flora_records(state = "Bahia", verbose = FALSE)$taxonName)
})


test_that("flora_records treats several states as one area for exclusive endemism", {
  .mock_flora_records(.flora_records_fixture())

  ba_mg <- flora_records(state = c("BA", "Minas Gerais"), endemism = TRUE, verbose = FALSE)
  expect_setequal(ba_mg$taxonName, c("Inga edulis", "Inga vera"))
})


test_that("flora_records applies exclusive endemism to domains and vegetation types", {
  .mock_flora_records(.flora_records_fixture())

  # Inga edulis is also in the Mata Atlantica, so it is not a Caatinga endemic
  caatinga <- flora_records(phytogeographicDomain = "Caatinga", endemism = TRUE, verbose = FALSE)
  expect_equal(caatinga$taxonName, "Inga vera")

  veg <- flora_records(vegetationType = "Caatinga (stricto sensu)", endemism = TRUE, verbose = FALSE)
  expect_setequal(veg$taxonName, c("Inga edulis", "Inga vera"))
})


test_that("flora_records requires exclusivity to every area filter combined", {
  .mock_flora_records(.flora_records_fixture())

  ba_caatinga <- flora_records(state = "Bahia", phytogeographicDomain = "Caatinga",
                               endemism = TRUE, verbose = FALSE)
  expect_equal(ba_caatinga$taxonName, "Inga vera")

  # Inga edulis is restricted to BA + MG, but not to the Caatinga
  ba_mg_caatinga <- flora_records(state = c("BA", "MG"), phytogeographicDomain = "Caatinga",
                                  endemism = TRUE, verbose = FALSE)
  expect_equal(ba_mg_caatinga$taxonName, "Inga vera")
})


test_that("flora_records excludes taxa with no record for an area from its exclusive endemics", {
  fixture <- .flora_records_fixture()
  # A Bahia-only endemic whose state is not recorded cannot be shown to be exclusive
  fixture$distribution_df$locationID[fixture$distribution_df$id == "2"] <- NA
  .mock_flora_records(fixture)

  expect_equal(nrow(flora_records(state = "Bahia", endemism = TRUE, verbose = FALSE)), 0)
})


test_that("flora_records matches area and trait values regardless of accents and case", {
  .mock_flora_records(.flora_records_fixture())

  expect_equal(flora_records(phytogeographicDomain = "Mata Atl\u00e2ntica", verbose = FALSE)$taxonName,
               "Inga edulis")
  expect_equal(flora_records(lifeForm = "\u00c1RVORE", verbose = FALSE)$taxonName, "Inga vera")
})


test_that("flora_records rejects an invalid endemism value", {
  .mock_flora_records(.flora_records_fixture())
  expect_error(flora_records(endemism = "yes", verbose = FALSE),
               "must be TRUE, FALSE, \"Brazil\", or NULL")
})


test_that("flora_records filters by establishmentMeans (origin), in English or Portuguese", {
  .mock_flora_records(.flora_records_fixture())

  introduced <- flora_records(establishmentMeans = "introduced", verbose = FALSE)
  expect_setequal(introduced$taxonName, c("Mimosa pudica", "Aster beta"))
  expect_setequal(flora_records(establishmentMeans = "Ex\u00f3tica", verbose = FALSE)$taxonName,
                  introduced$taxonName)

  native <- flora_records(establishmentMeans = "NATIVA", verbose = FALSE)
  expect_setequal(native$taxonName, c("Inga edulis", "Inga vera", "Aster alba"))
  expect_equal(nrow(flora_records(establishmentMeans = c("native", "introduced"), verbose = FALSE)), 5)
})


test_that("flora_records combines establishmentMeans with the other filters", {
  .mock_flora_records(.flora_records_fixture())
  expect_equal(nrow(flora_records(state = "Bahia", establishmentMeans = "introduced", verbose = FALSE)), 0)
  expect_equal(flora_records(state = "Bahia", establishmentMeans = "native", endemism = TRUE,
                             verbose = FALSE)$taxonName, "Inga vera")
})


test_that("flora_records rejects an unrecognised establishmentMeans", {
  .mock_flora_records(.flora_records_fixture())
  expect_error(flora_records(establishmentMeans = "cultivated", verbose = FALSE), "Unrecognised 'establishmentMeans'")
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


test_that("flora_records adds the distribution and species profile, joined with ' | '", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  result <- flora_records(verbose = FALSE)
  expect_true(all(c("state", "phytogeographicDomain", "endemism", "establishmentMeans",
                    "lifeForm", "habitat", "vegetationType") %in% names(result)))

  edulis <- result[result$taxonName == "Inga edulis", ]
  expect_equal(edulis$state, "BA | MG")
  expect_equal(edulis$phytogeographicDomain, "Caatinga | Mata Atlantica")
  expect_equal(edulis$endemism, "true")          # repeated values appear once
  expect_equal(edulis$establishmentMeans, "Nativa")
  expect_equal(edulis$lifeForm, "Arbusto")
  expect_equal(edulis$vegetationType, "Caatinga (stricto sensu)")

  # A taxon without distribution or profile rows gets NA
  synonym <- result[result$taxonName == "Inga synonym", ]
  expect_true(is.na(synonym$state))
  expect_true(is.na(synonym$lifeForm))
})


test_that("flora_records keeps the taxon's full distribution after an area filter", {
  fixture <- .flora_records_fixture()
  .mock_flora_records(fixture)

  result <- flora_records(state = "Minas Gerais", verbose = FALSE)
  expect_equal(result$taxonName, "Inga edulis")
  expect_equal(result$state, "BA | MG")
})


test_that(".collapse_by_id sorts values ignoring accents and drops empty ones", {
  out <- .collapse_by_id(id = c(1, 1, 1, 2, 2), value = c("Árvore", "Erva", "", NA, " "),
                         ids = c(2, 1, 3))
  expect_equal(out, c(NA, "Árvore | Erva", NA))
})
