test_that("reflora_records basic usage returns a data.frame", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
})


test_that("reflora_records handles empty taxon search", {
  expect_error(
    reflora_records(
    herbarium = "RB",
    taxon = "Fakeplantus invalidus",
    save = FALSE,
    verbose = FALSE)
  )
})


test_that("reflora_records applies state and year filters", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    state = c("Bahia", "Minas Gerais"),
    recordYear = c("2000", "2024"),
    save = FALSE,
    verbose = FALSE
  )
  expect_true(all(result$stateProvince %in% c("Bahia", "Minas Gerais")))
  expect_true(all(as.numeric(result$year) >= 2000 & as.numeric(result$year) <= 2024))
})


test_that("reflora_records reorders columns properly", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    reorder = c("year", "herbarium"),
    save = FALSE,
    verbose = FALSE
  )
  expect_true("year" %in% names(result))
  expect_true("institutionCode" %in% names(result))
})


test_that("reflora_records saves file when save = TRUE", {
  temp_dir <- tempdir()
  test_file <- "test_output"
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    dir = temp_dir,
    filename = test_file,
    save = TRUE,
    verbose = FALSE
  )
  output_path <- file.path(temp_dir, paste0(test_file, ".csv"))
  expect_true(file.exists(output_path))

  unlink(output_path)
})


test_that(".check_taxon_match handles valid and invalid taxa", {
  df <- data.frame(
    family = c("Fabaceae", "Rosaceae"),
    genus = c("Luetzelburgia", "Rosa"),
    taxonName = c("Luetzelburgia auriculata", "Rosa canina"),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_taxon_match(df, c("Fabaceae", "Luetzelburgia"), verbose = FALSE)
  )
  expect_error(
    .check_taxon_match(df, c("Fakeplantus"), verbose = FALSE),
    "must contain at least one name"
  )
  expect_message(
    .check_taxon_match(df, c("Fabaceae", "Unknownus"), verbose = TRUE),
    "not found"
  )
})


test_that(".check_year_match handles valid and invalid years", {
  df <- data.frame(
    year = c(1999, 2005, 2020),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_year_match(df, c("2005", "2020"), verbose = FALSE)
  )
  expect_error(
    .check_year_match(df, c("1800", "1801"), verbose = FALSE),
    "must contain at least one year"
  )
  expect_message(
    .check_year_match(df, c("2005", "3000"), verbose = TRUE),
    "not found"
  )
})


test_that(".check_state_match handles valid and invalid states", {
  df <- data.frame(
    stateProvince = c("Bahia", "Minas Gerais", "São Paulo"),
    stringsAsFactors = FALSE
  )
  expect_silent(
    .check_state_match(df, c("Bahia", "Minas Gerais"), verbose = FALSE)
  )
  expect_error(
    .check_state_match(df, c("ZZ", "XX"), verbose = FALSE),
    "must contain at least one name"
  )
  expect_message(
    .check_state_match(df, c("Minas Gerais", "ZZ"), verbose = TRUE),
    "not found"
  )
})


test_that("reflora_records removes indeterminate specimens with indets = FALSE", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    indets = FALSE,
    save = FALSE,
    verbose = FALSE
  )
  expect_false(any(result$taxonRank %in% c("family", "genus", "FAMILY", "GENERO", "FAMILIA", "SUB_FAMILIA",
                                           "TRIBO", "DIVISAO", "ORDEM", "CLASSE")))
})


test_that("reflora_records triggers auto download when path is NULL", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    path = NULL,
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records creates new dir if not present", {
  tmp_dir <- file.path(tempdir(), "new_reflora_records_dir")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
  expect_silent(
    reflora_records(
      herbarium = "RB",
      taxon = "Fabaceae",
      dir = tmp_dir,
      save = TRUE,
      verbose = FALSE
    )
  )
  expect_true(dir.exists(tmp_dir))
  unlink(tmp_dir, recursive = TRUE)
})


test_that("reflora_records returns empty data.frame if no match after filters", {
  expect_error(
    reflora_records(
      herbarium = "RB",
      taxon = "Fabaceae",
      state = "ZZ",  # invalid state
      save = FALSE,
      verbose = FALSE)
  )
})


test_that("reflora_records uses updates = FALSE with preexisting path", {
  test_path <- tempdir()
  reflora_download(herbarium = "ALCB",
                   dir = test_path,
                   verbose = FALSE)

  result <- reflora_records(
    herbarium = "ALCB",
    taxon = "Fabaceae",
    path = test_path,
    updates = FALSE,
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records handles partial reorder vector", {
  result <- reflora_records(
    herbarium = "ALCB",
    taxon = "Fabaceae",
    reorder = c("taxa", "year"),
    save = FALSE,
    verbose = FALSE
  )
  expect_true(all(c("family", "year") %in% colnames(result)))
})


test_that("reflora_records with default values still returns results", {
  result <- reflora_records()
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records stops on invalid year", {
  expect_error(
    reflora_records(
      taxon = "Fabaceae",
      herbarium = "RB",
      recordYear = c("1880", "1870"), # invalid range
      save = FALSE,
      verbose = FALSE
    )
  )
})


test_that("reflora_records updates data when path is given and updates = TRUE", {
  tmp_path <- tempdir()
  reflora_download(herbarium = "RB", dir = tmp_path, verbose = FALSE)
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    path = tmp_path,
    updates = TRUE,
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records works with herbarium = NULL", {
  result <- reflora_records(
    herbarium = NULL,
    taxon = "Fabaceae",
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records saves CSV and log.txt with save = TRUE", {
  tmp_dir <- tempdir()
  test_file <- "log_test"

  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    save = TRUE,
    dir = tmp_dir,
    filename = test_file,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(tmp_dir, paste0(test_file, ".csv"))))
  expect_true(file.exists(file.path(tmp_dir, "log.txt")))

  unlink(file.path(tmp_dir, paste0(test_file, ".csv")))
  unlink(file.path(tmp_dir, "log.txt"))
})


test_that("reflora_records prints message when verbose = TRUE", {
  expect_message(
    reflora_records(
      herbarium = "RB",
      taxon = "Fabaceae",
      save = FALSE,
      verbose = TRUE
    ),
    "Checking whether the input herbarium code exists"
  )
})


test_that("reflora_records handles NULL filename (if supported)", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    filename = NULL,
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records handles invalid reorder column gracefully", {
  expect_error(
    reflora_records(
      herbarium = "RB",
      taxon = "Fabaceae",
      reorder = c("INVALID_COLUMN"),
      save = FALSE,
      verbose = FALSE
    )
  )
})


test_that("reflora_records works with genus-level taxon only", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Inga",
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records defaults to repatriated = TRUE", {
  result <- reflora_records(
    herbarium = "RB",
    taxon = "Fabaceae",
    repatriated = TRUE,
    save = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
})


test_that("reflora_records with repatriated = FALSE excludes repatriated herbaria", {
  result <- reflora_records(
    herbarium = NULL,
    taxon = "Fabaceae",
    repatriated = FALSE,
    save = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.frame")

  # Ensure repatriated herbaria like K and E are not present
  repatriated_codes <- c("K", "E")
  found <- unique(result$collectionCode)
  expect_true(all(!repatriated_codes %in% found))
})


