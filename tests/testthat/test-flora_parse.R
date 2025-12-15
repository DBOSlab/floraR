test_that("reflora_parse excludes repatriated herbaria when repatriated = FALSE", {
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  # Simulate download of both repatriated and non-repatriated herbaria
  reflora_download(herbarium = c("RB", "K"),
                   dir = test_path,
                   verbose = FALSE)

  # Run with repatriated = FALSE (K should be excluded)
  parsed <- reflora_parse(path = test_path,
                          herbarium = NULL,
                          repatriated = FALSE,
                          verbose = FALSE)

  collections <- names(parsed)

  # K should not be in the parsed dwca list
  expect_false(any(grepl("dwca_k_reflora", collections)))
  expect_true(any(grepl("dwca_rb_", collections)))

  unlink(test_path, recursive = TRUE)
})


test_that("reflora_parse includes repatriated herbaria when repatriated = TRUE", {
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  reflora_download(herbarium = c("RB", "K"),
                   dir = test_path,
                   verbose = FALSE)

  parsed <- reflora_parse(path = test_path,
                          herbarium = NULL,
                          repatriated = TRUE,
                          verbose = FALSE)

  collections <- names(parsed)

  expect_true(any(grepl("dwca_k_reflora", collections)))
  expect_true(any(grepl("dwca_rb_", collections)))

  unlink(test_path, recursive = TRUE)
})


test_that("reflora_parse emits message when skipping repatriated collections", {
  test_path <- file.path(tempdir(), "reflora_repatriated_test")
  if (dir.exists(test_path)) unlink(test_path, recursive = TRUE)

  reflora_download(herbarium = c("RB", "K"), dir = test_path, verbose = FALSE)

  expect_message(
    reflora_parse(path = test_path,
                  herbarium = NULL,
                  repatriated = FALSE,
                  verbose = TRUE),
    "Skipping repatriated collections:"
  )
  unlink(test_path, recursive = TRUE)
})
