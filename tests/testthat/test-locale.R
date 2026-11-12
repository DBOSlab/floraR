# ---------------------------------------------------------------------------
# Locale independence: results must not change when R runs in a non-UTF-8
# locale (e.g. Rscript from a shell with LANG unset, i.e. the C locale).
# This file is kept ASCII: accented values are written with \u escapes.
# ---------------------------------------------------------------------------

# A string as unmarked UTF-8 bytes (Encoding "unknown"), as barRoso, refloraR
# and jabotR return them
.unmarked <- function(x) {
  x <- enc2utf8(x)
  Encoding(x) <- "unknown"
  x
}

.with_c_locale <- function(code) {
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), code)
}


test_that("state names are proper UTF-8, never '<U+00E1>' escape text", {
  st <- .normalize_br_state(c("GO", "Ceara", "Rondonia", "SP"))
  expect_identical(st, c("Goi\u00e1s", "Cear\u00e1", "Rond\u00f4nia", "S\u00e3o Paulo"))
  expect_false(any(grepl("<U+", st, fixed = TRUE)))
  expect_true(all(validUTF8(st)))
  expect_identical(.arg_check_state(c("Goias", "GO")), rep("Goi\u00e1s", 2))
})


test_that(".utf8 marks unmarked UTF-8 strings, in vectors, lists and attributes", {
  x <- c(.unmarked("Goi\u00e1s"), "Bahia", NA)
  expect_identical(Encoding(x)[1], "unknown")
  y <- .utf8(x)
  expect_identical(Encoding(y), c("UTF-8", "unknown", "unknown"))
  expect_identical(y, c("Goi\u00e1s", "Bahia", NA))

  df <- data.frame(state = x, n = 1:3, stringsAsFactors = FALSE)
  attr(df, "extra") <- data.frame(s = .unmarked("Par\u00e1"), stringsAsFactors = FALSE)
  out <- .utf8(list(states = df))
  expect_identical(Encoding(out$states$state[1]), "UTF-8")
  expect_identical(Encoding(attr(out$states, "extra")$s), "UTF-8")
  expect_identical(out$states$n, 1:3)
})


test_that("text is ordered the same way in every locale", {
  st <- c("Rio Grande do Norte", "Rio de Janeiro", "Paran\u00e1", "Para\u00edba", NA)
  expected <- c("Para\u00edba", "Paran\u00e1", "Rio de Janeiro", "Rio Grande do Norte")
  expect_identical(.sort_text(st), expected)
  expect_identical(.with_c_locale(.sort_text(st)), expected)
})


test_that(".use_utf8_locale switches to UTF-8 only until the caller exits", {
  f <- function() {
    .use_utf8_locale()
    isTRUE(l10n_info()[["UTF-8"]])
  }
  res <- .with_c_locale(list(inside = f(), after = isTRUE(l10n_info()[["UTF-8"]]),
                             ctype = Sys.getlocale("LC_CTYPE")))
  skip_if_not(res$inside, "no UTF-8 locale available on this system")
  expect_false(res$after)
  expect_identical(res$ctype, "C")
})


test_that(".use_utf8_locale warns when no UTF-8 locale can be set", {
  local_mocked_bindings(.utf8_locales = "no_such_locale.UTF-8", .package = "floraR")
  f <- function() .use_utf8_locale()
  expect_warning(res <- .with_c_locale(f()), "non-UTF-8 locale")
  expect_false(res)
})


test_that("flora_distribution_gap gives identical results in the C and UTF-8 locales", {
  skip_if_not(.with_c_locale((function() {
    .use_utf8_locale()
    isTRUE(l10n_info()[["UTF-8"]])
  })()), "no UTF-8 locale available on this system")

  # Raw REFLORA/JABOT records with unmarked UTF-8 accented states and
  # municipalities: barRoso::std_place() fails or mangles them in the C locale
  raw <- list(
    reflora = data.frame(
      collectionCode = c("ALCB", "HUEFS", "RB"), catalogNumber = c("1", "2", "3"),
      genus = "Luetzelburgia", specificEpithet = "auriculata",
      scientificName = "Luetzelburgia auriculata",
      country = "Brasil",
      stateProvince = .unmarked(c("Goi\u00e1s", "Cear\u00e1", "Bahia")),
      municipality = .unmarked(c("S\u00e3o F\u00e9lix", "Bel\u00e9m", "Salvador")),
      locality = NA_character_, decimalLatitude = NA, decimalLongitude = NA,
      recordedBy = "D. Cardoso", recordNumber = c("10", "11", "12"), year = "2010",
      typeStatus = NA_character_, stringsAsFactors = FALSE),
    jabot = data.frame(
      collectionCode = "RB", catalogNumber = "99",
      genus = "Luetzelburgia", specificEpithet = "auriculata",
      scientificName = "Luetzelburgia auriculata", country = "Brazil",
      stateProvince = .unmarked("Paran\u00e1"), municipality = "Curitiba",
      locality = NA_character_, decimalLatitude = NA, decimalLongitude = NA,
      recordedBy = "L.P. Queiroz", recordNumber = "5", year = "1999",
      typeStatus = NA_character_, stringsAsFactors = FALSE))
  labels <- c(reflora = "REFLORA", jabot = "JABOT")

  local_mocked_bindings(
    .flora_prepare_records = function(version, verbose, rm_flora_database) {
      list(taxon_df = data.frame(id = "11", taxonName = "Luetzelburgia auriculata",
                                 genus = "Luetzelburgia", acceptedNameUsageID = NA,
                                 stringsAsFactors = FALSE),
           distribution_df = data.frame(id = "11", locationID = c("BR-BA", "BR-PE"),
                                        phytogeographicDomain = "Caatinga",
                                        stringsAsFactors = FALSE),
           speciesprofile_df = data.frame())
    },
    .herbarium_records = function(source, taxon, verbose = TRUE) {
      .std_herbarium_records(raw[[source]], labels[[source]])
    },
    .gbif_plant_key = function(taxon) list(key = 1L, name = taxon, kingdom = "Plantae"),
    .gbif_brazil_state_counts = function(taxon_key) {
      st <- c("Rond\u00f4nia", "Bahia", "Rio de Janeiro", "Rio Grande do Norte")
      list(n_records = 4L,
           state_counts = data.frame(State = st, GBIF_records = 1L, stringsAsFactors = FALSE),
           raw_states = split(st, st))
    },
    .gbif_brazil_state_records = function(taxon_key, raw_states, limit = 50) {
      data.frame(Municipality = .unmarked("S\u00e3o Jos\u00e9"), Locality = NA_character_,
                 RecordedBy = "A. Silva", RecordNumber = "1", EventDate = "2001",
                 Institution = "UFP", CatalogNumber = "9",
                 Identified_as = "Luetzelburgia auriculata",
                 RecordLink = "https://www.gbif.org/occurrence/1", stringsAsFactors = FALSE)
    },
    .package = "floraR"
  )

  run <- function() {
    flora_distribution_gap("Luetzelburgia auriculata", domains = FALSE, save = FALSE,
                           html_report = FALSE, verbose = FALSE)
  }
  utf8 <- run()
  c_run <- .with_c_locale(run())

  expect_identical(c_run, utf8)
  expect_identical(c_run$states$state,
                   c("Bahia", "Cear\u00e1", "Goi\u00e1s", "Paran\u00e1", "Pernambuco",
                     "Rio de Janeiro", "Rio Grande do Norte", "Rond\u00f4nia"))
  expect_identical(sum(c_run$states$new_state_record_candidate), 6L)
  expect_false(any(grepl("<U+", unlist(c_run$records), fixed = TRUE)))
})
