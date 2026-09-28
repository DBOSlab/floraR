# ---------------------------------------------------------------------------
# Fixtures: a miniature FFB (taxa + distribution) and GBIF state counts
# ---------------------------------------------------------------------------
.dist_ffb_fixture <- function() {
  taxon_df <- data.frame(
    id = c("10", "11", "12", "13", "14"),
    taxonName = c("Luetzelburgia", "Luetzelburgia auriculata", "Luetzelburgia bahiensis",
                  "Luetzelburgia pallidiflora", "Luetzelburgia auriculata var. minor"),
    genus = "Luetzelburgia",
    acceptedNameUsageID = c(NA, NA, NA, "11", NA),
    stringsAsFactors = FALSE
  )
  distribution_df <- data.frame(
    id = c("11", "11", "12", "14"),
    locationID = c("BR-BA", "BR-PE", "BR-MG", "BR-CE"),
    stringsAsFactors = FALSE
  )
  list(taxon_df = taxon_df, distribution_df = distribution_df, speciesprofile_df = data.frame())
}

.mock_dist_ffb <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .flora_prepare_records = function(version, verbose, rm_flora_database) .dist_ffb_fixture(),
    .package = "floraR",
    .env = env
  )
}

.mock_dist_gbif <- function(state_counts, found = TRUE, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .gbif_plant_key = function(taxon) {
      if (found) list(key = 1L, name = taxon, kingdom = "Plantae") else NULL
    },
    .gbif_brazil_state_counts = function(taxon_key) {
      list(n_records = sum(state_counts$GBIF_records), state_counts = state_counts,
           raw_states = split(state_counts$State, state_counts$State))
    },
    .gbif_brazil_state_records = function(taxon_key, raw_states, limit = 50) {
      data.frame(Municipality = "Recife", Locality = "Dois Irmaos", RecordedBy = "A. Silva",
                 RecordNumber = "12", EventDate = "2001-05-01", Institution = "UFP",
                 CatalogNumber = "999", Identified_as = "Luetzelburgia auriculata",
                 RecordLink = "https://www.gbif.org/occurrence/1", stringsAsFactors = FALSE)
    },
    .package = "floraR",
    .env = env
  )
}

.gbif_counts <- function(states, n) {
  data.frame(State = states, GBIF_records = as.integer(n), stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# State normalisation
# ---------------------------------------------------------------------------
test_that(".normalize_br_state normalises the messy state values found in GBIF", {
  expect_equal(.normalize_br_state(c("Go", "Df", "Est. do Bahia", "Brasília", "Paraiba",
                                     "rs", "Estado de São Paulo", "State of Minas Gerais")),
               c("Goiás", "Distrito Federal", "Bahia", "Distrito Federal",
                 "Paraíba", "Rio Grande do Sul", "São Paulo", "Minas Gerais"))
})


test_that(".normalize_br_state returns NA for values that are not Brazilian states", {
  expect_equal(.normalize_br_state(c("Paraguay", "Buenos Aires", NA, "")),
               rep(NA_character_, 4))
  # "Para" is the state of Pará, not a prefix match of Paraná or Paraíba
  expect_equal(.normalize_br_state("Para"), "Pará")
})


# ---------------------------------------------------------------------------
# FFB's official states
# ---------------------------------------------------------------------------
test_that(".ffb_taxon_states includes infraspecific taxa for a species", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Luetzelburgia auriculata", f$taxon_df, f$distribution_df)
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Ceará"))
})


test_that(".ffb_taxon_states combines all species of a genus", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Luetzelburgia", f$taxon_df, f$distribution_df)
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Ceará", "Minas Gerais"))
})


test_that(".ffb_taxon_states uses the accepted name of an FFB synonym", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Luetzelburgia pallidiflora", f$taxon_df, f$distribution_df)
  expect_equal(res$name, "Luetzelburgia auriculata")
  expect_setequal(res$states, c("Bahia", "Pernambuco", "Ceará"))
})


test_that(".ffb_taxon_states reports a taxon absent from FFB", {
  f <- .dist_ffb_fixture()
  res <- .ffb_taxon_states("Luetzelburgia nova", f$taxon_df, f$distribution_df)
  expect_false(res$found)
  expect_length(res$states, 0)
})


# ---------------------------------------------------------------------------
# flora_distribution_gap()
# ---------------------------------------------------------------------------
test_that("flora_distribution_gap flags a GBIF state not in FFB's distribution", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Paraíba"), c(5, 2)))

  result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  pb <- result[result$State == "Paraíba", ]
  expect_true(pb$New_state_record_candidate)
  expect_false(pb$In_FFB_distribution)
  expect_equal(pb$GBIF_records, 2L)
  expect_false(result$New_state_record_candidate[result$State == "Bahia"])
})


test_that("flora_distribution_gap keeps FFB states without records, not as candidates", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))

  result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  pe <- result[result$State == "Pernambuco", ]
  expect_true(pe$In_FFB_distribution)
  expect_equal(pe$GBIF_records, 0L)
  expect_false(pe$New_state_record_candidate)
})


test_that("flora_distribution_gap compares a genus against all its species' states", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Minas Gerais", "Goiás"), c(3, 1)))

  result <- flora_distribution_gap(taxon = "Luetzelburgia", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  expect_false(result$New_state_record_candidate[result$State == "Minas Gerais"])
  expect_true(result$New_state_record_candidate[result$State == "Goiás"])
})


test_that("flora_distribution_gap restricts the check to the requested states", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Paraíba", "Goiás"), c(5, 2, 1)))

  result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata", state = c("PB", "Goias"),
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  expect_setequal(result$State, c("Paraíba", "Goiás"))
})


test_that("flora_distribution_gap rejects an unrecognised state", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))

  expect_error(flora_distribution_gap(taxon = "Luetzelburgia auriculata", state = "Atlantis",
                                      save = FALSE, html_report = FALSE, verbose = FALSE),
               "Unrecognised Brazilian state")
})


test_that("flora_distribution_gap still reports FFB states when GBIF has no match", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(character(0), integer(0)), found = FALSE)

  result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata", save = FALSE,
                                   html_report = FALSE, verbose = FALSE)

  expect_setequal(result$State, c("Bahia", "Pernambuco", "Ceará"))
  expect_false(any(result$New_state_record_candidate))
})


test_that("flora_distribution_gap skips speciesLink without an API key", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  called <- FALSE
  testthat::local_mocked_bindings(
    .splink_brazil_state_counts = function(taxon, key, ...) {
      called <<- TRUE
      data.frame(State = character(0), speciesLink_records = integer(0))
    },
    .package = "floraR"
  )

  expect_message(
    result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata",
                                     sources = c("gbif", "speciesLink"), specieslink_key = "",
                                     save = FALSE, html_report = FALSE, verbose = TRUE),
    "No speciesLink API key"
  )
  expect_false(called)
  expect_true(all(result$speciesLink_records == 0L))
})


test_that("flora_distribution_gap adds speciesLink evidence when a key is given", {
  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts("Bahia", 5))
  testthat::local_mocked_bindings(
    .splink_brazil_state_counts = function(taxon, key, ...) {
      data.frame(State = "Sergipe", speciesLink_records = 4L, stringsAsFactors = FALSE)
    },
    .package = "floraR"
  )

  result <- flora_distribution_gap(taxon = "Luetzelburgia auriculata",
                                   sources = c("gbif", "speciesLink"), specieslink_key = "k",
                                   save = FALSE, html_report = FALSE, verbose = FALSE)

  se <- result[result$State == "Sergipe", ]
  expect_equal(se$speciesLink_records, 4L)
  expect_equal(se$GBIF_records, 0L)
  expect_true(se$New_state_record_candidate)
})


test_that("flora_distribution_gap explains when FFB lists no state for the taxon", {
  f <- .dist_ffb_fixture()
  f$distribution_df$locationID[f$distribution_df$id == "12"] <- NA
  testthat::local_mocked_bindings(
    .flora_prepare_records = function(version, verbose, rm_flora_database) f,
    .package = "floraR"
  )
  .mock_dist_gbif(.gbif_counts("Sergipe", 3))

  expect_message(
    result <- flora_distribution_gap(taxon = "Luetzelburgia bahiensis", save = FALSE,
                                     html_report = FALSE, verbose = TRUE),
    "FFB lists no Brazilian state"
  )
  expect_true(result$New_state_record_candidate[result$State == "Sergipe"])
})


test_that("flora_distribution_gap requires a single character taxon and valid sources", {
  expect_error(flora_distribution_gap(taxon = NULL), "single character string")
  expect_error(flora_distribution_gap(taxon = c("A", "B")), "single character string")
  expect_error(flora_distribution_gap(taxon = "A", sources = "inaturalist"), "should be one of")
})


test_that("flora_distribution_gap writes a real .xlsx file and HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_dist_ffb()
  .mock_dist_gbif(.gbif_counts(c("Bahia", "Paraíba"), c(5, 2)))
  out_dir <- withr::local_tempdir()

  flora_distribution_gap(taxon = "Luetzelburgia auriculata", save = TRUE, html_report = TRUE,
                         open_report = FALSE, dir = out_dir, filename = "dist_test",
                         verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "dist_test.xlsx")))
  expect_true(file.exists(file.path(out_dir, "dist_test.html")))
})
