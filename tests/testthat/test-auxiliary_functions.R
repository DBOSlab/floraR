test_that(".save_csv() writes a file correctly", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  tmp_dir <- tempdir()
  filename <- "test_file"

  .save_csv(df, verbose = FALSE, filename = filename, dir = tmp_dir)

  output_path <- file.path(tmp_dir, paste0(filename, ".csv"))
  expect_true(file.exists(output_path))

  written_df <- read.csv(output_path)
  expect_equal(ncol(written_df), 2)
  expect_equal(nrow(written_df), 3)

  unlink(output_path)
})


test_that(".replace_cols() replaces target column with group", {
  result <- .replace_cols(c("herbarium", "collector"), replace = c("collectionCode"), columns_to_replace = "herbarium")
  expect_equal(result[1], "collectionCode")

  result2 <- .replace_cols(c("a", "taxa", "b"), replace = c("family", "genus"), columns_to_replace = "taxa")
  expect_equal(result2, c("a", "family", "genus", "b"))
})


test_that(".check_taxon_match() validates correctly", {
  df <- data.frame(family = "Fabaceae", genus = "Inga", taxonName = "Inga edulis")
  expect_silent(.check_taxon_match(df, taxon = "Fabaceae", verbose = FALSE))
  expect_error(.check_taxon_match(df, taxon = "Invalidus", verbose = FALSE))
})


test_that(".check_state_match() validates state presence", {
  df <- data.frame(stateProvince = c("BA", "MG"))
  expect_silent(.check_state_match(df, state = "BA", verbose = FALSE))
  expect_error(.check_state_match(df, state = "XX", verbose = FALSE))
})


test_that(".check_year_match() validates year presence", {
  df <- data.frame(year = c(2000, 2010))
  expect_silent(.check_year_match(df, recordYear = 2000, verbose = FALSE))
  expect_error(.check_year_match(df, recordYear = 1990, verbose = FALSE))
})


test_that(".filter_occur_df() handles taxon + year filtering", {
  df <- data.frame(
    family = c("Fabaceae", "Malvaceae"),
    genus = c("Inga", "Hibiscus"),
    taxonName = c("Inga edulis", "Hibiscus rosa-sinensis"),
    stateProvince = c("Bahia", "Minas Gerais"),
    year = c(2005, 2020)
  )

  result <- .filter_occur_df(df,
                             taxon = "Fabaceae",
                             state = "Bahia",
                             recordYear = c(2005, 2010),
                             verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$family, "Fabaceae")
})


test_that(".save_csv() writes a file correctly", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  tmp_dir <- tempdir()
  filename <- "test_file"

  .save_csv(df, verbose = FALSE, filename = filename, dir = tmp_dir)

  output_path <- file.path(tmp_dir, paste0(filename, ".csv"))
  expect_true(file.exists(output_path))

  written_df <- read.csv(output_path)
  expect_equal(ncol(written_df), 2)
  expect_equal(nrow(written_df), 3)

  unlink(output_path)
})


test_that(".save_log() creates a log with summary statistics", {
  df <- data.frame(
    collectionCode = c("RB", "RB", "K"),
    family = c("Fabaceae", "Fabaceae", "Asteraceae"),
    genus = c("Inga", "Inga", "Baccharis"),
    country = c("Brazil", "Brazil", "Brazil"),
    stateProvince = c("BA", "BA", "SP")
  )

  tmp_dir <- tempdir()
  filename <- "test_log_file"

  log_path <- file.path(tmp_dir, "log.txt")
  if (file.exists(log_path)) unlink(log_path)

  .save_log(df = df, herbarium = c("RB", "K"), filename = filename, dir = tmp_dir)
  expect_true(file.exists(log_path))

  log_contents <- readLines(log_path)
  expect_true(any(grepl("Total records: 3", log_contents)))
  expect_true(any(grepl("Records per herbarium:", log_contents)))
  expect_true(any(grepl("Records per family:", log_contents)))
  expect_true(any(grepl("Records per genus:", log_contents)))
  expect_true(any(grepl("Records per country:", log_contents)))
  expect_true(any(grepl("Records per stateProvince:", log_contents)))

  unlink(log_path)
})


test_that(".replace_cols() replaces target column with group", {
  result <- .replace_cols(c("herbarium", "collector"), replace = c("collectionCode"), columns_to_replace = "herbarium")
  expect_equal(result[1], "collectionCode")

  result2 <- .replace_cols(c("a", "taxa", "b"), replace = c("family", "genus"), columns_to_replace = "taxa")
  expect_equal(result2, c("a", "family", "genus", "b"))
})


test_that(".check_taxon_match() validates correctly", {
  df <- data.frame(family = "Fabaceae", genus = "Inga", taxonName = "Inga edulis")
  expect_silent(.check_taxon_match(df, taxon = "Fabaceae", verbose = FALSE))
  expect_error(.check_taxon_match(df, taxon = "Invalidus", verbose = FALSE))
})


test_that(".check_state_match() validates state presence", {
  df <- data.frame(stateProvince = c("BA", "MG"))
  expect_silent(.check_state_match(df, state = "BA", verbose = FALSE))
  expect_error(.check_state_match(df, state = "XX", verbose = FALSE))
})


test_that(".check_year_match() validates year presence", {
  df <- data.frame(year = c(2000, 2010))
  expect_silent(.check_year_match(df, recordYear = 2000, verbose = FALSE))
  expect_error(.check_year_match(df, recordYear = 1990, verbose = FALSE))
})


test_that(".filter_occur_df() handles taxon + year filtering", {
  df <- data.frame(
    family = c("Fabaceae", "Malvaceae"),
    genus = c("Inga", "Hibiscus"),
    taxonName = c("Inga edulis", "Hibiscus rosa-sinensis"),
    stateProvince = c("Bahia", "Minas Gerais"),
    year = c(2005, 2020)
  )

  result <- .filter_occur_df(df,
                             taxon = "Fabaceae",
                             state = "Bahia",
                             recordYear = c(2005, 2010),
                             verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$family, "Fabaceae")
})


