.flora_parse_setup_path <- function() {
  tmp <- tempfile("flora_parse_path_")
  dwca_dir <- file.path(tmp, "dwca_ffb_v393_001_latest")
  dir.create(dwca_dir, recursive = TRUE)
  # .arg_check_path() only requires these files to exist on disk; their
  # content is irrelevant since finch::dwca_read() itself is mocked below.
  writeLines("", file.path(dwca_dir, "taxon.txt"))
  writeLines("", file.path(dwca_dir, "distribution.txt"))
  writeLines("", file.path(dwca_dir, "eml.xml"))
  tmp
}

.flora_parse_mock_dwca <- function() {
  taxon.txt <- data.frame(
    id = c("1", "2", "3"),
    kingdom = c("Plantae", "Plantae", "Fungi"),
    family = c("Fabaceae", "NA", "Fungaceae"),
    genus = c("Inga", "NA", "Fungusgen"),
    specificEpithet = c("edulis", "vera", "fakus"),
    infraspecificEpithet = c(NA, NA, NA),
    taxonRank = c("ESPECIE", "ESPECIE", "ESPECIE"),
    stringsAsFactors = FALSE
  )
  distribution.txt <- data.frame(
    id = c("1", "2", "3"),
    occurrenceRemarks = c(
      '{"endemism":"true","phytogeographicDomain":["Amazonia","Cerrado"]}',
      '{"endemism":"false","phytogeographicDomain":["Caatinga"]}',
      '{"endemism":"false","phytogeographicDomain":["Mata Atlantica"]}'
    ),
    locationID = c("BR-AM", "BR-BA", "BR-RJ"),
    stringsAsFactors = FALSE
  )
  speciesprofile.txt <- data.frame(
    id = c("1", "2", "3"),
    lifeForm = c(
      '{"lifeForm":["Arbusto","Arvore"],"habitat":["Terricola"],"vegetationType":["Cerrado (lato sensu)"]}',
      '{"lifeForm":["Erva"],"habitat":["Epifita"],"vegetationType":["Caatinga (stricto sensu)"]}',
      '{"lifeForm":["Erva"],"habitat":["Terricola"],"vegetationType":["Mata Atlantica"]}'
    ),
    stringsAsFactors = FALSE
  )
  resourcerelationship.txt <- data.frame(id = c("1", "2", "3"), stringsAsFactors = FALSE)
  vernacularname.txt <- data.frame(id = c("1", "2", "3"), stringsAsFactors = FALSE)
  typesandspecimen.txt <- data.frame(id = c("1", "2", "3"), stringsAsFactors = FALSE)
  reference.txt <- data.frame(id = c("1", "2", "3"), stringsAsFactors = FALSE)

  list(
    files = list(xml_files = "dwca_ffb_v393_001_latest/eml.xml"),
    data = list(
      taxon.txt = taxon.txt,
      distribution.txt = distribution.txt,
      speciesprofile.txt = speciesprofile.txt,
      resourcerelationship.txt = resourcerelationship.txt,
      vernacularname.txt = vernacularname.txt,
      typesandspecimen.txt = typesandspecimen.txt,
      reference.txt = reference.txt
    )
  )
}

.mock_dwca_read <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    dwca_read = function(...) .flora_parse_mock_dwca(),
    .package = "finch",
    .env = env
  )
}


test_that("flora_parse keeps only Plantae records and drops Fungi", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)

  taxon <- dwca[[1]][["data"]][["taxon.txt"]]
  expect_equal(nrow(taxon), 2)
  expect_true(all(taxon$kingdom == "Plantae"))

  unlink(path, recursive = TRUE)
})


test_that("flora_parse builds a standardized taxonName column", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)
  taxon <- dwca[[1]][["data"]][["taxon.txt"]]

  expect_true("taxonName" %in% names(taxon))
  expect_equal(taxon$taxonName[taxon$id == "1"], "Inga edulis")
})


test_that("flora_parse converts the literal string 'NA' to a real NA in family/genus", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)
  taxon <- dwca[[1]][["data"]][["taxon.txt"]]

  row2 <- taxon[taxon$id == "2", ]
  expect_true(is.na(row2$family))
  expect_true(is.na(row2$genus))
})


test_that("flora_parse extracts endemism and unnests phytogeographicDomain", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)
  distribution <- dwca[[1]][["data"]][["distribution.txt"]]

  # id=1 has two domains (Amazonia, Cerrado) -> unnested into 2 rows
  id1_rows <- distribution[distribution$id == "1", ]
  expect_equal(nrow(id1_rows), 2)
  expect_setequal(id1_rows$phytogeographicDomain, c("Amazonia", "Cerrado"))
  expect_true(all(id1_rows$endemism == "true"))
})


test_that("flora_parse extracts lifeForm, habitat, and vegetationType from speciesprofile", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)
  speciesprofile <- dwca[[1]][["data"]][["speciesprofile.txt"]]

  expect_true(all(c("lifeForm", "habitat", "vegetationType", "lifeForm_json") %in% names(speciesprofile)))

  id1_rows <- speciesprofile[speciesprofile$id == "1", ]
  expect_equal(nrow(id1_rows), 2)  # two lifeForm states: Arbusto, Arvore
  expect_setequal(id1_rows$lifeForm, c("Arbusto", "Arvore"))
  expect_true(all(id1_rows$habitat == "Terricola"))
})


test_that("flora_parse names the returned list by the dwca folder", {
  .mock_dwca_read()
  path <- .flora_parse_setup_path()

  dwca <- flora_parse(path = path, version = "latest", verbose = FALSE)
  expect_equal(names(dwca), "dwca_ffb_v393_001_latest")

  unlink(path, recursive = TRUE)
})
