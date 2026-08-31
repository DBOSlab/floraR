.flora_version_mock_html <- function() {
  # Minimal HTML fragments satisfying each regex used by flora_version():
  #   Version:      >(\d{3}\.\d{3})<
  #   Published_on: (\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})
  #   Records:      '([0-9,]+)'
  #   URL:          href=\"([^\"]+)
  block <- function(version, date, records) {
    sprintf(
      paste0(
        '<li><a href="https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil&v=%s">',
        "<span>%s</span></a> was published on %s with '%s' records.</li>"
      ),
      version, version, date, records
    )
  }

  c(
    "<html><body><ul>",
    block("393.001", "2023-05-10 10:00:00", "120,000"),
    "/* only show released versions marker */",
    block("393.002", "2024-01-15 09:30:00", "121,500"),
    "</ul></body></html>"
  )
}

.mock_flora_version <- function(html_lines, env = parent.frame()) {
  testthat::local_mocked_bindings(
    readLines = function(...) html_lines,
    .package = "base",
    .env = env
  )
}


test_that("flora_version parses version, date, records, and URL from IPT metadata", {
  .mock_flora_version(.flora_version_mock_html())

  result <- flora_version()

  expect_s3_class(result, "data.frame")
  expect_setequal(result$Version, c("393.001", "393.002"))
  expect_setequal(result$Records, c("120,000", "121,500"))
  expect_true(all(grepl("^https://ipt.jbrj.gov.br", result$URL)))
})


test_that("flora_version sorts results by Published_on descending (most recent first)", {
  .mock_flora_version(.flora_version_mock_html())

  result <- flora_version()

  expect_equal(result$Version[1], "393.002")  # published 2024, more recent
  expect_true(result$Published_on[1] > result$Published_on[2])
})


test_that("flora_version excludes 'Version_num' helper column and bootstrap URLs from the result", {
  .mock_flora_version(.flora_version_mock_html())

  result <- flora_version()

  expect_false("Version_num" %in% names(result))
  expect_false(any(grepl("bootstrap", result$URL)))
})


test_that("flora_version drops fully empty blocks with no extractable metadata", {
  html <- c(
    "<html><body><ul>",
    "<li>Some unrelated text with no version metadata at all.</li>",
    .flora_version_mock_html()[-1],  # keep the real blocks, drop the wrapper <html><body>
    "</ul></body></html>"
  )
  .mock_flora_version(html)

  result <- flora_version()
  expect_equal(nrow(result), 2)
})
