# ---------------------------------------------------------------------------
# POWO fixture: a miniature WCVP (names + distributions) in rWCVPdata's layout
# ---------------------------------------------------------------------------
.wcvp_fixture <- function() {
  names <- data.frame(
    plant_name_id = c(1, 2, 3, 4, 5, 6, 7, 8),
    ipni_id = c("1-1", "2-1", "3-1", "4-1", "5-1", "6-1", "7-1", "8-1"),
    powo_id = c("1-1", "2-1", "3-1", "4-1", "5-1", "6-1", "7-1", "8-1"),
    taxon_rank = "Species",
    taxon_status = c("Accepted", "Accepted", "Synonym", "Accepted", "Accepted",
                     "Accepted", "Accepted", "Misapplied"),
    genus = "Luetzelburgia",
    species = c("andina", "bahiensis", "brasiliensis", "auriculata", "harleyi",
                "nova", "dubia", "errata"),
    taxon_name = paste("Luetzelburgia", c("andina", "bahiensis", "brasiliensis",
                                          "auriculata", "harleyi", "nova", "dubia",
                                          "errata")),
    taxon_authors = c("D.B.O.S.Cardoso & al.", "Yakovlev", "Yakovlev",
                      "(Allemão) Ducke", "D.B.O.S.Cardoso & al.", "Anon.", "Anon.",
                      "auct."),
    accepted_plant_name_id = c(1, 2, 4, 4, 5, 6, 7, 5),
    geographic_area = c("Bolivia", "Brazil (Bahia)", NA, "Brazil", "Brazil (Bahia)",
                        "Brazil (Minas Gerais)", "Peru", NA),
    place_of_publication = c("Syst. Bot.", "Nauchn. Dokl.", "Nauchn. Dokl.",
                             "Notizbl. Bot. Gart. Berlin-Dahlem", "Kew Bull.",
                             NA, NA, NA),
    volume_and_page = c(" 37: 677", " 9: 75", " 9: 75", " 11: 584", " 63: 290",
                        NA, NA, NA),
    first_published = c("(2012)", "(1976)", "(1976)", "(1932)", "(2008)", "(2026)",
                        "(2020)", NA),
    stringsAsFactors = FALSE
  )
  # "nova" (6) has no distribution at all (range-text fallback); "dubia" (7)
  # is only doubtfully recorded in Brazil, which must not count as evidence
  distributions <- data.frame(
    plant_name_id = c(1, 2, 4, 4, 5, 7, 7),
    area_code_l3 = c("BOL", "BZE", "BZE", "BZL", "BZE", "PER", "BZN"),
    area = c("Bolivia", "Brazil Northeast", "Brazil Northeast", "Brazil Southeast",
             "Brazil Northeast", "Peru", "Brazil North"),
    introduced = c(0, 0, 0, 1, 0, 0, 0),
    extinct = 0,
    location_doubtful = c(0, 0, 0, 0, 0, 0, 1),
    stringsAsFactors = FALSE
  )
  list(names = names, distributions = distributions, version = "Test")
}

# ---------------------------------------------------------------------------
# GBIF fixture: names of the genus with Brazilian type specimens, their
# backbone names, and the specimens themselves
# ---------------------------------------------------------------------------
.gbif_counts_fixture <- function() {
  data.frame(GBIF_Key = c("11", "12", "13", "14"),
             N_type_specimens = c(3L, 2L, 1L, 4L),
             stringsAsFactors = FALSE)
}

.gbif_names_fixture <- function() {
  data.frame(
    GBIF_Key = c("11", "12", "13", "14"),
    Taxon_name = c("Luetzelburgia harleyi", "Luetzelburgia andrade-limae",
                   "Luetzelburgia recens", "Luetzelburgia alba var. minor"),
    Authors = c("D.B.O.S.Cardoso & al.", "H.C.Lima", "Silva", "Berg"),
    Rank = c("SPECIES", "SPECIES", "SPECIES", "VARIETY"),
    Taxonomic_status = c("ACCEPTED", "ACCEPTED", "DOUBTFUL", "SYNONYM"),
    Accepted_name = c("Luetzelburgia harleyi", "Luetzelburgia andrade-limae",
                      "Luetzelburgia recens", "Luetzelburgia alba"),
    Accepted_full_name = c("Luetzelburgia harleyi D.B.O.S.Cardoso & al.",
                           "Luetzelburgia andrade-limae H.C.Lima",
                           "Luetzelburgia recens Silva", "Luetzelburgia alba Berg"),
    Genus = "Luetzelburgia",
    GBIF_Published_in = c("Kew Bull. 63: 290 (2008)", NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

.gbif_specimens_fixture <- function(keys) {
  sp <- data.frame(
    GBIF_Key = c("11", "11", "11", "13", "33"),
    Accepted_key = c("11", "11", "11", "13", "13"),
    Species_key = c("11", "11", "11", "13", "13"),
    Specimen_name = c("Luetzelburgia harleyi D.B.O.S.Cardoso & al.",
                      "Luetzelburgia harleyi D.B.O.S.Cardoso & al.",
                      "Luetzelburgia harleyi D.B.O.S.Cardoso & al.",
                      "Luetzelburgia recens Silva", "Vatairea recens Berg"),
    Occurrence_ID = c("101", "102", "103", "131", "132"),
    Type_status = c("Holotype", "Isotype", "Isotype", "Type", "Paratype"),
    Typified_name = NA_character_,
    Institution = c("RB", "K", "NY", "SPF", NA),
    Catalog_number = c("00123", "K0001", "NY9", "555", "777"),
    Collector = c("A. Silva", "A. Silva", "A. Silva", "B. Souza", "B. Souza"),
    Year = c("2006", "2006", "2007", "1990", "1990"),
    State = c("Bahia", "Bahia", "Minas Gerais", "Goiás", NA),
    stringsAsFactors = FALSE
  )
  # Like GBIF, a query for a key also returns its synonyms' specimens
  sp[sp$GBIF_Key %in% keys | sp$Accepted_key %in% keys, ]
}

# ---------------------------------------------------------------------------
# Mocks
# ---------------------------------------------------------------------------
.mock_ffb <- function(taxon_names, other_names = character(0), env = parent.frame()) {
  testthat::local_mocked_bindings(
    .ffb_names = function(taxon, version) {
      list(genus = taxon_names, all = c(taxon_names, other_names))
    },
    # REFLORA and JABOT are queried by default: no archives in tests, unless a
    # test mocks them with herbarium records
    .herbarium_records = function(source, taxon, verbose = TRUE) NULL,
    .package = "floraR",
    .env = env
  )
}

.mock_wcvp <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .wcvp_tables = function() .wcvp_fixture(),
    .package = "floraR",
    .env = env
  )
}

.mock_gbif <- function(found = TRUE, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .gbif_genus_key = function(taxon, rank = "GENUS") {
      if (found) list(key = 99L, name = paste(taxon, "Harms"), kingdom = "Plantae") else NULL
    },
    .gbif_brazil_type_counts = function(genus_key, type_status) .gbif_counts_fixture(),
    .gbif_genus_names = function(genus_key) .gbif_names_fixture(),
    .gbif_type_specimens = function(keys, type_status, ...) .gbif_specimens_fixture(keys),
    .package = "floraR",
    .env = env
  )
}

gap <- function(...) {
  flora_species_gap(taxon = "Luetzelburgia", save = FALSE, html_report = FALSE,
                    verbose = FALSE, ...)
}


# ===========================================================================
# POWO evidence (sources = "powo")
# ===========================================================================
test_that("POWO: flags species POWO places in Brazil that are missing from FFB", {
  .mock_wcvp()
  .mock_ffb(character(0))

  result <- gap(sources = "powo")

  expect_setequal(result$Taxon_name,
                  paste("Luetzelburgia", c("bahiensis", "brasiliensis", "auriculata",
                                           "harleyi", "nova")))
  expect_true(all(result$POWO_Brazil_Evidence))
  expect_true(all(result$Found_in == "POWO"))
  expect_true(all(!result$In_FFB))
  expect_false(any(grepl("^GBIF_", names(result))))
})


test_that("POWO: a synonym whose accepted name is in FFB is not flagged", {
  .mock_wcvp()
  .mock_ffb("Luetzelburgia auriculata")
  expect_false("Luetzelburgia brasiliensis" %in% gap(sources = "powo")$Taxon_name)
})


test_that("POWO: an accepted name FFB files under another genus is recognised", {
  .mock_wcvp()
  .mock_ffb(character(0), other_names = "Luetzelburgia auriculata")
  expect_false("Luetzelburgia brasiliensis" %in% gap(sources = "powo")$Taxon_name)
})


test_that("POWO: orthographic variants are treated as the same name", {
  .mock_wcvp()
  .mock_ffb(c("Luetzelburgia Bahiensis", "Luetzelburgia harley-i"))
  expect_false(any(c("Luetzelburgia bahiensis", "Luetzelburgia harleyi") %in%
                     gap(sources = "powo")$Taxon_name))
})


test_that("POWO: synonyms get their accepted name's distribution and citation", {
  .mock_wcvp()
  .mock_ffb(character(0))

  syn <- gap(sources = "powo")
  syn <- syn[syn$Taxon_name == "Luetzelburgia brasiliensis", ]

  expect_equal(syn$Accepted_name, "Luetzelburgia auriculata")
  expect_equal(syn$POWO_Brazil_Areas, "Brazil Northeast; Brazil Southeast (introduced)")
  expect_equal(syn$POWO_Published_in, "Nauchn. Dokl. 9: 75 (1976)")
  expect_equal(syn$POWO_Year, "1976")
  expect_equal(syn$POWO_URL, "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:3-1")
})


test_that("POWO: WCVP distribution first, POWO range text as fallback", {
  .mock_wcvp()
  .mock_ffb(character(0))

  result <- gap(sources = "powo")
  src <- setNames(result$POWO_Evidence_Source, result$Taxon_name)
  expect_equal(unname(src["Luetzelburgia harleyi"]), "WCVP distribution")
  expect_equal(unname(src["Luetzelburgia nova"]), "POWO range text")
})


test_that("POWO: doubtful Brazilian records and misapplied names are ignored", {
  .mock_wcvp()
  .mock_ffb(character(0))

  result <- gap(sources = "powo", require_brazil_evidence = FALSE)
  dubia <- result[result$Taxon_name == "Luetzelburgia dubia", ]

  expect_false(dubia$POWO_Brazil_Evidence)
  expect_false(dubia$Brazil_evidence)
  expect_match(dubia$POWO_Distribution, "Brazil North (doubtful)", fixed = TRUE)
  expect_false("Luetzelburgia errata" %in% result$Taxon_name)
})


test_that("POWO: require_brazil_evidence = FALSE keeps names without Brazil evidence", {
  .mock_wcvp()
  .mock_ffb(character(0))

  expect_false(any(c("Luetzelburgia andina", "Luetzelburgia dubia") %in%
                     gap(sources = "powo")$Taxon_name))
  expect_equal(nrow(gap(sources = "powo", require_brazil_evidence = FALSE)), 7)
})


test_that("POWO: powo_source = 'checklistbank' uses the web sources, not rWCVPdata", {
  .mock_ffb(character(0))
  testthat::local_mocked_bindings(
    .wcvp_tables = function() stop("rWCVPdata must not be used"),
    .powo_name_search = function(taxon, max_number) {
      data.frame(POWO_ID = c("2-1", "5-1"), IPNI_ID = c("2-1", "5-1"),
                 Taxon_name = paste("Luetzelburgia", c("bahiensis", "harleyi")),
                 Authors = c("Yakovlev", "D.B.O.S.Cardoso & al."),
                 SpecificEpithet = c("bahiensis", "harleyi"), Year = c("1976", "2008"),
                 Taxonomic_status = "Accepted",
                 Accepted_name = paste("Luetzelburgia", c("bahiensis", "harleyi")),
                 Accepted_authors = c("Yakovlev", "D.B.O.S.Cardoso & al."),
                 Accepted_ID = c("urn:lsid:ipni.org:names:2-1", "urn:lsid:ipni.org:names:5-1"),
                 POWO_Range = c("Brazil (Bahia)", "Bolivia"),
                 Published_in_ID = c("r2", "r5"), stringsAsFactors = FALSE)
    },
    .wcvp_distribution = function(accepted_name, ipni_id) {
      if (grepl("bahiensis", accepted_name)) {
        list(found = TRUE, areas = "Brazil Northeast", brazil = "Brazil Northeast")
      } else {
        list(found = FALSE, areas = NA_character_, brazil = NA_character_)
      }
    },
    .powo_publication = function(published_in_id) paste("Citation", published_in_id),
    .package = "floraR"
  )

  result <- gap(sources = "powo", powo_source = "checklistbank")
  expect_equal(result$Taxon_name, "Luetzelburgia bahiensis")
  expect_equal(result$POWO_Published_in, "Citation r2")

  expect_true(all(is.na(gap(sources = "powo", powo_source = "checklistbank",
                            fetch_details = FALSE)$POWO_Published_in)))
})


# ===========================================================================
# GBIF evidence (sources = "gbif")
# ===========================================================================
test_that("GBIF: flags species-level names with Brazilian types missing from FFB", {
  .mock_gbif()
  .mock_ffb(character(0))

  result <- gap(sources = "gbif")

  expect_setequal(result$Taxon_name, paste("Luetzelburgia", c("harleyi", "andrade-limae",
                                                              "recens")))
  expect_true(all(result$Found_in == "GBIF"))
  expect_true(all(result$Brazil_evidence))
  expect_false(any(grepl("^POWO_", names(result))))
})


test_that("GBIF: summarises each name's Brazilian type specimens", {
  .mock_gbif()
  .mock_ffb(character(0))

  result <- gap(sources = "gbif")
  h <- result[result$Taxon_name == "Luetzelburgia harleyi", ]

  expect_equal(h$GBIF_N_type_specimens, 3L)
  # Type statuses standardised with barRoso::std_types()
  expect_equal(h$GBIF_Type_statuses, "holotype; isotype")
  expect_equal(h$GBIF_Type_specimens, "RB 00123 (holotype); K K0001 (isotype); NY NY9 (isotype)")
  expect_equal(h$GBIF_Years, "2006-2007")
  expect_equal(h$GBIF_States, "Bahia; Minas Gerais")
  expect_equal(h$GBIF_URL, "https://www.gbif.org/species/11")
})


test_that("GBIF: types filed under a synonym are assigned to the requested name", {
  .mock_gbif()
  .mock_ffb(character(0))

  r <- gap(sources = "gbif")
  expect_equal(r$GBIF_Specimen_names[r$Taxon_name == "Luetzelburgia recens"],
               "Luetzelburgia recens Silva; Vatairea recens Berg")
})


test_that("GBIF: specimen columns are NA when no specimen could be retrieved", {
  .mock_gbif()
  .mock_ffb(character(0))

  r <- gap(sources = "gbif")
  a <- r[r$Taxon_name == "Luetzelburgia andrade-limae", ]
  expect_true(is.na(a$GBIF_Type_specimens))
  expect_true(is.na(a$GBIF_Years))
})


test_that("GBIF: specimens are fetched only for names missing from FFB", {
  fetched <- NULL
  .mock_gbif()
  testthat::local_mocked_bindings(
    .gbif_type_specimens = function(keys, type_status, ...) {
      fetched <<- keys
      .gbif_specimens_fixture(keys)
    },
    .package = "floraR"
  )
  .mock_ffb("Luetzelburgia harleyi")

  gap(sources = "gbif")
  expect_setequal(fetched, c("12", "13"))
})


test_that("GBIF: a custom type_status is passed on", {
  seen <- NULL
  .mock_gbif()
  testthat::local_mocked_bindings(
    .gbif_brazil_type_counts = function(genus_key, type_status) {
      seen <<- type_status
      .gbif_counts_fixture()
    },
    .package = "floraR"
  )
  .mock_ffb(character(0))

  gap(sources = "gbif", type_status = c("Holotype", "Isotype"))
  expect_equal(seen, c("Holotype", "Isotype"))
})


test_that("GBIF: a genus unknown among plants and algae stops when GBIF is the only source", {
  .mock_gbif(found = FALSE)
  .mock_ffb(character(0))
  expect_error(gap(sources = "gbif"), "not found in GBIF among plants and algae")
})


test_that(".gbif_genus_key searches only the kingdoms holding plants and algae", {
  queried <- character(0)
  testthat::local_mocked_bindings(
    .get_json = function(url) {
      queried <<- c(queried, sub(".*&kingdom=([^&]+).*", "\\1", url))
      NULL
    },
    .package = "floraR"
  )
  expect_null(.gbif_genus_key("Nonexistentia"))
  expect_equal(queried, c("Plantae", "Chromista", "Protozoa"))
})


test_that(".gbif_genus_key ignores a doubtful plant placeholder of a fungal genus", {
  testthat::local_mocked_bindings(
    .get_json = function(url) {
      if (grepl("kingdom=Plantae", url)) {
        list(matchType = "EXACT", rank = "GENUS", kingdom = "Plantae", status = "DOUBTFUL",
             usageKey = 1L, scientificName = "Agaricus")
      } else {
        list(matchType = "EXACT", rank = "GENUS", kingdom = "Fungi", status = "ACCEPTED",
             usageKey = 2L, scientificName = "Agaricus L.")
      }
    },
    .package = "floraR"
  )
  expect_null(.gbif_genus_key("Agaricus"))
})


test_that(".gbif_genus_key keeps a doubtful plant genus with no namesake elsewhere", {
  testthat::local_mocked_bindings(
    .get_json = function(url) {
      if (grepl("kingdom=Plantae", url)) {
        list(matchType = "EXACT", rank = "GENUS", kingdom = "Plantae", status = "DOUBTFUL",
             usageKey = 1L, scientificName = "Dubiella Anon.")
      } else {
        NULL
      }
    },
    .package = "floraR"
  )
  expect_equal(.gbif_genus_key("Dubiella")$key, 1L)
})


# ===========================================================================
# Both sources (default)
# ===========================================================================
test_that("both: a name reported by POWO and GBIF becomes one row, listed first", {
  .mock_wcvp()
  .mock_gbif()
  .mock_ffb(character(0))

  result <- gap()

  expect_equal(sum(result$Taxon_name == "Luetzelburgia harleyi"), 1)
  h <- result[result$Taxon_name == "Luetzelburgia harleyi", ]
  expect_equal(h$Found_in, "POWO; GBIF")
  expect_equal(h$POWO_ID, "5-1")
  expect_equal(h$GBIF_Key, "11")
  expect_equal(result$Taxon_name[1], "Luetzelburgia harleyi")
})


test_that("both: names from a single source keep NA in the other source's columns", {
  .mock_wcvp()
  .mock_gbif()
  .mock_ffb(character(0))

  result <- gap()
  nova <- result[result$Taxon_name == "Luetzelburgia nova", ]
  recens <- result[result$Taxon_name == "Luetzelburgia recens", ]

  expect_equal(nova$Found_in, "POWO")
  expect_true(is.na(nova$GBIF_Key))
  expect_equal(recens$Found_in, "GBIF")
  expect_true(is.na(recens$POWO_ID))
  expect_true(recens$Brazil_evidence)
})


test_that("both: POWO and GBIF spellings of the same name are merged", {
  wc <- .wcvp_fixture()
  wc$names$taxon_name[5] <- "Luetzelburgia harley-i"
  testthat::local_mocked_bindings(.wcvp_tables = function() wc, .package = "floraR")
  .mock_gbif()
  .mock_ffb(character(0))

  result <- gap()
  expect_equal(sum(result$Found_in == "POWO; GBIF"), 1)
})


test_that("both: an unknown GBIF genus is skipped, keeping the POWO results", {
  .mock_wcvp()
  .mock_gbif(found = FALSE)
  .mock_ffb(character(0))

  result <- gap()
  expect_true(nrow(result) > 0)
  expect_true(all(result$Found_in == "POWO"))
})


test_that("returns an empty data.frame when every name is already in FFB", {
  .mock_wcvp()
  .mock_gbif()
  .mock_ffb(c(.wcvp_fixture()$names$taxon_name, .gbif_names_fixture()$Taxon_name))

  result <- gap()
  expect_equal(nrow(result), 0)
  expect_true(all(c("Found_in", "In_FFB") %in% names(result)))
})


test_that("validates its arguments", {
  expect_error(flora_species_gap(taxon = NULL), "character vector")
  expect_error(flora_species_gap(taxon = NA_character_), "character vector")
  expect_error(flora_species_gap(taxon = "A", sources = "wfo"), "should be one of")
  expect_error(flora_species_gap(taxon = "A", powo_source = "api"), "should be one of")
})


test_that("writes a real .xlsx file and HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  .mock_wcvp()
  .mock_gbif()
  .mock_ffb(character(0))
  out_dir <- withr::local_tempdir()

  flora_species_gap(taxon = "Luetzelburgia", save = TRUE, html_report = TRUE,
                    open_report = FALSE, dir = out_dir, verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "flora_species_gap_Luetzelburgia.xlsx")))
  expect_true(file.exists(file.path(out_dir, "flora_species_gap_Luetzelburgia.html")))
})


test_that(".location_mentions_brazil matches Brazil and states but not Paraguay", {
  expect_true(.location_mentions_brazil("Brazil (Bahia)"))
  expect_true(.location_mentions_brazil("Sao Paulo"))
  expect_false(.location_mentions_brazil("Bolivia to Paraguay"))
  expect_false(.location_mentions_brazil(NA_character_))
})


test_that(".name_key collapses hyphen and genitive-ending variants", {
  expect_equal(.name_key("Luetzelburgia andrade-limae"), .name_key("Luetzelburgia andradelimae"))
  expect_equal(.name_key("Luetzelburgia freire-allemanii"), .name_key("Luetzelburgia freire-allemani"))
  expect_equal(.name_key("Myrcia aegiphylloides"), .name_key("Myrcia aegyphylloides"))
  expect_false(.name_key("Myrcia alba") == .name_key("Myrcia albida"))
})


# ---------------------------------------------------------------------------
# REFLORA and JABOT type specimens
# ---------------------------------------------------------------------------
.herbarium_types <- function(label, epithets, status, country = "Brasil",
                             state = "Bahia") {
  raw <- data.frame(collectionCode = "HUEFS", catalogNumber = seq_along(epithets),
                    genus = "Luetzelburgia", specificEpithet = epithets,
                    scientificName = paste("Luetzelburgia", epithets),
                    scientificNameAuthorship = "Cardoso", country = country,
                    stateProvince = state, recordedBy = "D. Cardoso", year = "2010",
                    typeStatus = status, stringsAsFactors = FALSE)
  .std_herbarium_records(raw, label)
}

.mock_herbaria <- function(reflora = NULL, jabot = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .herbarium_records = function(source, taxon, verbose = TRUE) {
      list(reflora = reflora, jabot = jabot)[[source]]
    },
    .package = "floraR",
    .env = env
  )
}


test_that("REFLORA/JABOT: Brazilian types of names missing from FFB are reported", {
  .mock_ffb(c("Luetzelburgia auriculata", "Luetzelburgia andradelimae"))
  .mock_herbaria(
    reflora = .herbarium_types("REFLORA",
                               c("nova", "nova", "auriculata", "andrade-limae", "exotica", "comum"),
                               c("HOLOTYPUS", "ISOTYPUS", "ISOTYPUS", "TYPUS", "HOLOTYPUS", NA),
                               country = c(rep("Brasil", 4), "Bolivia", "Brasil")),
    jabot = .herbarium_types("JABOT", "nova", "Isotype", country = "Brazil"))

  r <- gap(sources = c("reflora", "jabot"))
  # In FFB (also as a spelling variant), collected outside Brazil, or not a type: left out
  expect_equal(r$Taxon_name, "Luetzelburgia nova")
  expect_equal(r$Found_in, "REFLORA; JABOT")
  expect_true(r$Brazil_evidence)
  expect_equal(r$REFLORA_N_type_specimens, 2L)
  expect_equal(r$REFLORA_Type_statuses, "holotype; isotype")
  expect_equal(r$JABOT_Type_statuses, "isotype")
  expect_equal(r$REFLORA_States, "Bahia")
})


test_that("names reported by more sources are listed first", {
  .mock_gbif()
  .mock_ffb(character(0))
  .mock_herbaria(reflora = .herbarium_types("REFLORA", c("harleyi", "nova"),
                                            c("Holotype", "Holotype")))

  r <- gap(sources = c("gbif", "reflora"))
  expect_equal(r$Taxon_name[1], "Luetzelburgia harleyi")
  expect_equal(r$Found_in[1], "GBIF; REFLORA")
  expect_equal(r$Found_in[r$Taxon_name == "Luetzelburgia nova"], "REFLORA")
  expect_true(all(c("GBIF_Key", "REFLORA_Type_specimens") %in% names(r)))
  expect_false(any(grepl("^JABOT_", names(r))))
})


# ===========================================================================
# Several genera in one call
# ===========================================================================
test_that("several genera: FFB and herbaria are read once, results stacked per genus", {
  .mock_wcvp()
  .mock_gbif()
  ffb_calls <- 0
  herb_calls <- list()
  testthat::local_mocked_bindings(
    .ffb_names = function(taxon, version) {
      ffb_calls <<- ffb_calls + 1
      list(genus = c("Luetzelburgia auriculata", "Vatairea macrocarpa"),
           all = c("Luetzelburgia auriculata", "Vatairea macrocarpa"))
    },
    .herbarium_records = function(source, taxon, verbose = TRUE) {
      herb_calls[[source]] <<- taxon
      NULL
    },
    .species_gap_taxon = function(taxon, ffb, ...) {
      ffb_genus <- ffb$genus[sub("\\s.*", "", ffb$genus) == taxon]
      data.frame(Taxon_name = paste(taxon, "nova"), In_FFB = FALSE,
                 N_ffb = length(ffb_genus), stringsAsFactors = FALSE)
    },
    .package = "floraR"
  )
  res <- flora_species_gap(taxon = c("Luetzelburgia", "Vatairea", "Luetzelburgia"),
                           save = FALSE, html_report = FALSE, verbose = FALSE)
  expect_equal(ffb_calls, 1)
  expect_equal(herb_calls, list(reflora = c("Luetzelburgia", "Vatairea"),
                                jabot = c("Luetzelburgia", "Vatairea")))
  expect_equal(res$Taxon_name, c("Luetzelburgia nova", "Vatairea nova"))
  expect_equal(res$N_ffb, c(1L, 1L))
})


test_that("several genera: each genus gets its own spreadsheet", {
  .mock_wcvp()
  .mock_gbif()
  .mock_ffb(character(0))
  out_dir <- withr::local_tempdir()
  res <- flora_species_gap(taxon = c("Luetzelburgia", "Vatairea"), sources = "powo",
                           save = TRUE, html_report = FALSE, dir = out_dir, verbose = FALSE)
  expect_true(file.exists(file.path(out_dir, "flora_species_gap_Luetzelburgia.xlsx")))
})
