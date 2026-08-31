.flora_descriptions_page_text <- function() {
  paste(c(
    "Descrição com campos controlados",
    "Habit: Tree.",
    "Height: 5-10m.",
    "Descrição livre",
    "This is a free description of the species.",
    "Comentários",
    "Some remarks."
  ), collapse = "\n")
}

.mock_success_webshot <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    webshot = function(url, file, delay, useragent) {
      writeLines("mock pdf placeholder", file)
      invisible(file)
    },
    .package = "webshot",
    .env = env
  )
  testthat::local_mocked_bindings(
    pdf_text = function(pdf) list(.flora_descriptions_page_text()),
    .package = "pdftools",
    .env = env
  )
}

.mock_failing_webshot <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    webshot = function(url, file, delay, useragent) invisible(NULL),  # never creates the file
    .package = "webshot",
    .env = env
  )
}


test_that("flora_get_descriptions extracts controlled and free description text", {
  .mock_success_webshot()

  taxa <- data.frame(
    genus = "Inga", specificEpithet = "edulis",
    references = "https://floradobrasil.jbrj.gov.br/consulta/ficha.html?id=1",
    stringsAsFactors = FALSE
  )

  result <- flora_get_descriptions(taxa, delay = 0, verbose = FALSE)

  expect_named(result, c("descriptions_controlled", "descriptions_free"))
  expect_equal(result$descriptions_controlled[["Inga edulis"]], "Habit: Tree. Height: 5-10m.")
  expect_equal(result$descriptions_free[["Inga edulis"]],
              "This is a free description of the species.")
})


test_that("flora_get_descriptions records failures when the page cannot be captured", {
  .mock_failing_webshot()

  taxa <- data.frame(
    genus = "Inga", specificEpithet = "edulis",
    references = "https://floradobrasil.jbrj.gov.br/consulta/ficha.html?id=1",
    stringsAsFactors = FALSE
  )

  result <- flora_get_descriptions(taxa, delay = 0, verbose = FALSE)

  # Failures are reported as list(descriptions, failed_names) when any occur
  expect_length(result$descriptions_controlled, 2)
  expect_equal(result$descriptions_controlled[[2]], "Inga edulis")
  expect_length(result$descriptions_free, 2)
  expect_equal(result$descriptions_free[[2]], "Inga edulis")
})


test_that("flora_get_descriptions processes multiple taxa and names results by scientific name", {
  .mock_success_webshot()

  taxa <- data.frame(
    genus = c("Inga", "Mimosa"),
    specificEpithet = c("edulis", "pudica"),
    references = c(
      "https://floradobrasil.jbrj.gov.br/consulta/ficha.html?id=1",
      "https://floradobrasil.jbrj.gov.br/consulta/ficha.html?id=2"
    ),
    stringsAsFactors = FALSE
  )

  result <- flora_get_descriptions(taxa, delay = 0, verbose = FALSE)

  expect_setequal(names(result$descriptions_controlled), c("Inga edulis", "Mimosa pudica"))
})


test_that("flora_get_descriptions prints a progress message per taxon when verbose = TRUE", {
  .mock_success_webshot()

  taxa <- data.frame(
    genus = "Inga", specificEpithet = "edulis",
    references = "https://floradobrasil.jbrj.gov.br/consulta/ficha.html?id=1",
    stringsAsFactors = FALSE
  )

  expect_message(flora_get_descriptions(taxa, delay = 0, verbose = TRUE), "Extracted 1/1")
})
