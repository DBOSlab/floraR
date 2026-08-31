test_that(".arg_check_dir validates type and strips trailing slash", {
  expect_equal(.arg_check_dir("some_dir/"), "some_dir")
  expect_equal(.arg_check_dir("some_dir"), "some_dir")
  expect_error(.arg_check_dir(123), "should be a character")
})


test_that(".arg_check_path validates existence and required files", {
  tmp <- tempfile("flora_path_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_error(.arg_check_path(123, character(0), list()), "should be a character")
  expect_error(.arg_check_path(file.path(tmp, "nope"), character(0), list()),
              "no folder")

  # No dwca folder at all
  expect_error(.arg_check_path(tmp, "not_a_dwca_folder", list(character(0))),
              "no FFB-downloaded dwca folder")

  # dwca folder present but empty
  expect_error(.arg_check_path(tmp, "dwca_ffb_v1", list(character(0))),
              "fully empty")

  # dwca folder present with files, but missing taxon.txt/distribution.txt
  expect_error(.arg_check_path(tmp, "dwca_ffb_v1", list(c("eml.xml"))),
              "taxon.txt.*distribution.txt")

  # Valid dwca folder
  expect_silent(.arg_check_path(tmp, "dwca_ffb_v1", list(c("taxon.txt", "distribution.txt"))))
})


test_that(".arg_check_recordYear validates single year and ranges", {
  expect_silent(.arg_check_recordYear("2001"))
  expect_silent(.arg_check_recordYear(c("1990", "2024")))
  expect_error(.arg_check_recordYear(c("1990", "2000", "2010")), "single year or a range")
  expect_error(.arg_check_recordYear("20a1"), "4-digit numbers")
  expect_error(.arg_check_recordYear(c("2024", "1990")), "first year must be less")
})


test_that(".arg_check_state returns full state names by default", {
  result <- .arg_check_state(c("Bahia", "SP", "Sao Paulo", "XX"))
  expect_equal(result, c("Bahia", "São Paulo", "São Paulo", "XX"))
})


test_that(".arg_check_state can return acronyms when requested", {
  result <- .arg_check_state(c("Bahia", "Sao Paulo", "PE"), return_abbrev = TRUE)
  expect_equal(result, c("BA", "SP", "PE"))
})


test_that(".arg_check_state is diacritics-insensitive (but not case-insensitive)", {
  expect_equal(.arg_check_state("Ceara", return_abbrev = TRUE), "CE")
  expect_equal(.arg_check_state("Rondonia", return_abbrev = TRUE), "RO")
  # lowercase input does not match (no case-folding is applied)
  expect_equal(.arg_check_state("ceara", return_abbrev = TRUE), "ceara")
})


test_that(".check_taxon_match validates presence of taxon in the FFB database", {
  df <- data.frame(family = "Fabaceae", genus = "Inga", taxonName = "Inga edulis",
                   stringsAsFactors = FALSE)
  expect_silent(.check_taxon_match(df, taxon = "Fabaceae", verbose = FALSE))
  expect_error(.check_taxon_match(df, taxon = "Invalidaceae", verbose = FALSE),
              "Flora e Funga do Brasil database")
  expect_message(.check_taxon_match(df, taxon = c("Fabaceae", "Unknownus"), verbose = TRUE),
                 "not found in any column")
})


test_that(".check_state_match validates presence of state in the data", {
  df <- data.frame(stateProvince = c("Bahia", "Minas Gerais"), stringsAsFactors = FALSE)
  expect_silent(.check_state_match(df, state = "Bahia", verbose = FALSE))
  expect_error(.check_state_match(df, state = "ZZ", verbose = FALSE),
              "Flora e Funga do Brasil database")
  expect_message(.check_state_match(df, state = c("Bahia", "ZZ"), verbose = TRUE),
                 "not found")
})


test_that(".flora_names_check validates character type, non-emptiness and encoding", {
  expect_silent(.flora_names_check(c("Inga edulis", "Mimosa pudica")))
  expect_error(.flora_names_check(123), "must be a character vector")
  expect_error(.flora_names_check(character(0)), "must not be empty")
})
