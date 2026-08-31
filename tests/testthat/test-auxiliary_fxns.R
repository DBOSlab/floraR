test_that(".filter_occur_df filters by family, genus, and species", {
  df <- data.frame(
    family = c("Fabaceae", "Fabaceae", "Malvaceae"),
    genus = c("Inga", "Mimosa", "Hibiscus"),
    taxonName = c("Inga edulis", "Mimosa pudica", "Hibiscus rosa-sinensis"),
    stringsAsFactors = FALSE
  )

  fam_result <- .filter_occur_df(df, taxon = "Fabaceae", state = NULL, verbose = FALSE)
  expect_setequal(fam_result$taxonName, c("Inga edulis", "Mimosa pudica"))

  gen_result <- .filter_occur_df(df, taxon = "Mimosa", state = NULL, verbose = FALSE)
  expect_equal(gen_result$taxonName, "Mimosa pudica")

  sp_result <- .filter_occur_df(df, taxon = "Inga edulis", state = NULL, verbose = FALSE)
  expect_equal(sp_result$taxonName, "Inga edulis")
})


test_that(".filter_occur_df filters by state when stateProvince is present", {
  df <- data.frame(
    family = c("Fabaceae", "Fabaceae"),
    genus = c("Inga", "Mimosa"),
    taxonName = c("Inga edulis", "Mimosa pudica"),
    stateProvince = c("Bahia", "Minas Gerais"),
    stringsAsFactors = FALSE
  )
  result <- .filter_occur_df(df, taxon = NULL, state = "Bahia", verbose = FALSE)
  expect_equal(result$taxonName, "Inga edulis")
})


test_that(".save_csv writes a CSV file to disk", {
  tmp_dir <- tempfile("flora_save_csv_")
  df <- data.frame(x = 1:3, y = letters[1:3])

  .save_csv(df, verbose = FALSE, filename = "test_file", dir = tmp_dir)

  out <- file.path(tmp_dir, "test_file.csv")
  expect_true(file.exists(out))
  written <- read.csv(out)
  expect_equal(nrow(written), 3)
  expect_equal(ncol(written), 2)

  unlink(tmp_dir, recursive = TRUE)
})


test_that(".save_log writes a summary log file", {
  tmp_dir <- tempfile("flora_save_log_")
  dir.create(tmp_dir)
  df <- data.frame(
    family = c("Fabaceae", "Fabaceae"),
    genus = c("Inga", "Mimosa"),
    country = c("Brazil", "Brazil"),
    stateProvince = c("Bahia", "Bahia"),
    stringsAsFactors = FALSE
  )

  .save_log(df, filename = "test_file", dir = tmp_dir)

  log_path <- file.path(tmp_dir, "log.txt")
  expect_true(file.exists(log_path))
  log_contents <- readLines(log_path)
  expect_true(any(grepl("Total records: 2", log_contents)))
  expect_true(any(grepl("Records per family:", log_contents)))

  unlink(tmp_dir, recursive = TRUE)
})


test_that(".flora_get_taxon extracts the taxon.txt table from a dwca list", {
  dwca <- list(
    dwca_ffb_v1 = list(data = list(taxon.txt = data.frame(id = "1", taxonName = "Inga edulis")))
  )
  result <- .flora_get_taxon(dwca)
  expect_equal(result$taxonName, "Inga edulis")

  expect_error(.flora_get_taxon(list()), "non-empty named list")
  expect_error(.flora_get_taxon(list(x = list(data = list()))), "No 'taxon.txt' table found")
})


test_that(".flora_get_col safely accesses a column or returns NA vector", {
  df <- data.frame(a = 1:3)
  expect_equal(.flora_get_col(df, "a"), 1:3)
  expect_equal(.flora_get_col(df, "missing"), rep(NA_character_, 3))
})


test_that(".flora_names_standardize trims and collapses whitespace", {
  expect_equal(.flora_names_standardize("  Inga   edulis  "), "Inga edulis")
  expect_equal(.flora_names_standardize(c("A  B", " C ")), c("A B", "C"))
})


test_that(".flora_splist_classify parses genus, epithet, infra rank, and author", {
  result <- .flora_splist_classify(c(
    "Inga edulis",
    "Solanum lycopersicum subsp. esculentum Mill.",
    "Mimosa pudica L."
  ))

  expect_equal(result$genus, c("Inga", "Solanum", "Mimosa"))
  expect_equal(result$epithet, c("edulis", "lycopersicum", "pudica"))
  expect_equal(result$infra_rank[2], "subsp.")
  expect_equal(result$infra_epithet[2], "esculentum")
  expect_equal(result$author[2], "Mill.")
  expect_equal(result$author[3], "L.")
  expect_true(is.na(result$infra_rank[1]))
})


test_that(".flora_splist_classify handles NA and empty input gracefully", {
  result <- .flora_splist_classify(c(NA_character_, ""))
  expect_true(all(is.na(result$genus)))
  expect_true(all(is.na(result$epithet)))
})


test_that(".flora_build_genus_index builds a genus-to-row-index lookup", {
  taxon_df <- data.frame(genus = c("Inga", "Inga", "Mimosa"), stringsAsFactors = FALSE)
  idx <- .flora_build_genus_index(taxon_df)
  expect_equal(sort(idx[["Inga"]]), c(1L, 2L))
  expect_equal(idx[["Mimosa"]], 3L)
})


test_that(".flora_get_threshold computes integer or fractional Levenshtein thresholds", {
  expect_equal(.flora_get_threshold(2, 10), 2L)
  expect_equal(.flora_get_threshold(0.2, 10), 2L)
  expect_equal(.flora_get_threshold(0.01, 10), 1L)  # minimum of 1
})


test_that(".flora_resolve_accepted resolves synonyms to their accepted name", {
  taxon_df <- data.frame(
    id = c("1", "2"),
    taxonomicStatus = c("SINONIMO", "NOME_ACEITO"),
    acceptedNameUsageID = c("2", NA_character_),
    taxonName = c("Inga synonym", "Inga edulis"),
    stringsAsFactors = FALSE
  )
  id_lookup <- list("2" = 2L)

  syn_result <- .flora_resolve_accepted(1L, taxon_df, id_lookup)
  expect_equal(syn_result$id, "2")
  expect_equal(syn_result$name, "Inga edulis")

  acc_result <- .flora_resolve_accepted(2L, taxon_df, id_lookup)
  expect_equal(acc_result$id, "2")
  expect_equal(acc_result$name, "Inga edulis")
})


test_that(".flora_resolve_accepted warns and returns NA when the accepted ID is missing", {
  taxon_df <- data.frame(
    id = "1",
    taxonomicStatus = "SINONIMO",
    acceptedNameUsageID = "999",
    taxonName = "Inga synonym",
    stringsAsFactors = FALSE
  )
  expect_warning(result <- .flora_resolve_accepted(1L, taxon_df, list()),
                 "not found in id_lookup")
  expect_true(is.na(result$id))
})


test_that(".flora_na_row builds a single NA-filled row, optionally with Correct.Spelling", {
  row <- .flora_na_row("Unknown species")
  expect_equal(nrow(row), 1)
  expect_equal(row$Search, "Unknown species")
  expect_true(is.na(row$FFB.taxon.ID))
  expect_false("Correct.Spelling" %in% names(row))

  row2 <- .flora_na_row("Unknown species", include_correct = TRUE)
  expect_true("Correct.Spelling" %in% names(row2))
  expect_true(is.na(row2$Correct.Spelling))
})


test_that(".flora_build_rows builds result rows resolving accepted names", {
  taxon_df <- data.frame(
    id = c("1", "2"),
    taxonRank = c("ESPECIE", "ESPECIE"),
    taxonomicStatus = c("NOME_ACEITO", "NOME_ACEITO"),
    taxonName = c("Inga edulis", "Inga vera"),
    scientificNameAuthorship = c("Mart.", "L."),
    family = c("Fabaceae", "Fabaceae"),
    order = c("Fabales", "Fabales"),
    stringsAsFactors = FALSE
  )
  id_lookup <- list()

  result <- .flora_build_rows("Inga edulis", rows = 1L, dists = 0L, taxon_df, id_lookup)
  expect_equal(result$Search, "Inga edulis")
  expect_equal(result$FFB.taxon.ID, "1")
  expect_equal(result$family, "Fabaceae")
  expect_equal(result$Accepted.taxon.ID, "1")
  expect_equal(result$Accepted.taxon.Name, "Inga edulis")
})


test_that(".flora_find_dups flags duplicated Accepted.taxon.Name entries", {
  result <- data.frame(
    Accepted.taxon.Name = c("Inga edulis", "Mimosa pudica", "Inga edulis", NA_character_)
  )
  dups <- .flora_find_dups(result)
  expect_equal(dups[1], 3L)
  expect_true(is.na(dups[2]))
  expect_equal(dups[3], 1L)
  expect_true(is.na(dups[4]))
})


test_that(".buildMatrix parses semi-structured description strings into a character matrix", {
  # The FIRST description defines the expected column structure; here it has
  # two "Habit" states (Tree; Shrub) and one "Leaf" state.
  descriptions <- list(
    "sp1" = "Habit: Tree; Shrub. Leaf: Simple.",
    "sp2" = "Habit: Herb; Climber. Leaf: Compound."
  )
  mat <- .buildMatrix(descriptions)
  expect_equal(rownames(mat), c("sp1", "sp2"))
  expect_equal(colnames(mat), c("Habit", "Habit", "Leaf"))
  # Row-level splitting uses ". " (period+space) rather than "." alone, so the
  # final field keeps its trailing period when the string simply ends there.
  expect_equal(unname(mat["sp1", ]), c("Tree", "Shrub", "Simple."))
  expect_equal(unname(mat["sp2", ]), c("Herb", "Climber", "Compound."))
})


test_that(".buildMatrix leaves a row as NA when its structure doesn't match the first entry", {
  descriptions <- list(
    "sp1" = "Habit: Tree; Shrub. Leaf: Simple.",
    "sp2" = "Habit: Herb. Leaf: Compound."  # only one Habit state, mismatched length
  )
  mat <- .buildMatrix(descriptions)
  expect_true(all(is.na(mat["sp2", ])))
})
