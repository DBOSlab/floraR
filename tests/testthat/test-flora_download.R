.flora_version_fixture <- function() {
  data.frame(
    Version = c("393.002", "393.001"),
    Latest = c(TRUE, FALSE),
    Published_on = c("2024-01-15 09:30:00", "2023-05-10 10:00:00"),
    Records = c("121,500", "120,000"),
    URL = c(
      "https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil&v=393.002",
      "https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil&v=393.001"
    ),
    stringsAsFactors = FALSE
  )
}

.mock_flora_version <- function(fixture = .flora_version_fixture(), env = parent.frame()) {
  testthat::local_mocked_bindings(
    flora_version = function() fixture,
    .package = "floraR",
    .env = env
  )
}

.mock_download_unzip <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, method) {
      writeLines("mock zip content", destfile)
      invisible(0L)
    },
    unzip = function(zipfile, exdir) {
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      writeLines("taxon", file.path(exdir, "taxon.txt"))
      writeLines("distribution", file.path(exdir, "distribution.txt"))
      invisible(character(0))
    },
    .package = "utils",
    .env = env
  )
}


test_that("flora_download creates the target directory if missing", {
  .mock_flora_version()
  .mock_download_unzip()

  tmp_dir <- tempfile("flora_download_dir_")
  expect_false(dir.exists(tmp_dir))

  flora_download(version = "latest", verbose = FALSE, dir = tmp_dir)
  expect_true(dir.exists(tmp_dir))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download downloads the latest version when nothing is cached", {
  .mock_flora_version()
  .mock_download_unzip()

  tmp_dir <- tempfile("flora_download_latest_")
  dir.create(tmp_dir)

  flora_download(version = "latest", verbose = FALSE, dir = tmp_dir)

  downloaded <- list.files(tmp_dir)
  expect_true(any(grepl("393_002.*latest", downloaded)))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download reuses a previously downloaded latest version without re-downloading", {
  .mock_flora_version()

  tmp_dir <- tempfile("flora_download_cached_")
  dir.create(tmp_dir)
  cached_folder <- file.path(tmp_dir, "dwca_ffb_v393_002_latest")
  dir.create(cached_folder)

  download_called <- FALSE
  testthat::local_mocked_bindings(
    download.file = function(...) { download_called <<- TRUE },
    .package = "utils"
  )

  expect_message(
    flora_download(version = "latest", verbose = TRUE, dir = tmp_dir),
    "previously downloaded"
  )
  expect_false(download_called)

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download downloads a specific requested version not yet cached", {
  .mock_flora_version()
  .mock_download_unzip()

  tmp_dir <- tempfile("flora_download_specific_")
  dir.create(tmp_dir)

  flora_download(version = "393.001", verbose = FALSE, dir = tmp_dir)

  downloaded <- list.files(tmp_dir)
  expect_true(any(grepl("393_001", downloaded)))

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download skips a specific version that is already cached", {
  .mock_flora_version()

  tmp_dir <- tempfile("flora_download_specific_cached_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "dwca_ffb_v393_001"))

  download_called <- FALSE
  testthat::local_mocked_bindings(
    download.file = function(...) { download_called <<- TRUE },
    .package = "utils"
  )

  flora_download(version = "393.001", verbose = FALSE, dir = tmp_dir)
  expect_false(download_called)

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download stops when there is no internet and no cached data", {
  testthat::local_mocked_bindings(
    flora_version = function() stop("simulated offline"),
    .package = "floraR"
  )

  tmp_dir <- tempfile("flora_download_offline_")
  dir.create(tmp_dir)

  expect_error(
    suppressMessages(flora_download(version = "latest", verbose = FALSE, dir = tmp_dir)),
    "No internet connection"
  )

  unlink(tmp_dir, recursive = TRUE)
})


test_that("flora_download uses cached data with a message when offline but data exists", {
  testthat::local_mocked_bindings(
    flora_version = function() stop("simulated offline"),
    .package = "floraR"
  )

  tmp_dir <- tempfile("flora_download_offline_cached_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "dwca_ffb_v393_001"))

  expect_message(
    flora_download(version = "393.001", verbose = TRUE, dir = tmp_dir),
    "no internet conection"
  )

  unlink(tmp_dir, recursive = TRUE)
})
