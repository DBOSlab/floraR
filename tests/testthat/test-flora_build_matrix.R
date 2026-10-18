test_that("flora_build_matrix builds a matrix directly from a plain description list", {
  descriptions <- list(
    "sp1" = "Habit: Tree; Shrub. Leaf: Simple.",
    "sp2" = "Habit: Herb; Climber. Leaf: Compound.",
    "sp3" = "Habit: Tree. Leaf: Compound."
  )
  mat <- flora_build_matrix(descriptions)
  expect_setequal(rownames(mat), c("sp1", "sp2", "sp3"))
  expect_equal(colnames(mat), c("Habit", "Habit", "Leaf"))
})


test_that("flora_build_matrix removes NULL entries before building the matrix", {
  descriptions <- list(
    "sp1" = "Habit: Tree; Shrub. Leaf: Simple.",
    "sp2" = NULL,
    "sp3" = "Habit: Herb; Climber. Leaf: Compound."
  )
  mat <- flora_build_matrix(descriptions)
  expect_setequal(rownames(mat), c("sp1", "sp3"))
})


test_that("flora_build_matrix handles the length-2 (descriptions + failures) input shape", {
  # This is the shape returned by flora_get_descriptions() when some taxa
  # failed to extract: list(descriptions_list, failed_names_vector)
  descriptions_list <- list(
    "sp1" = "Habit: Tree; Shrub. Leaf: Simple.",
    "sp2" = NULL,
    "sp3" = "Habit: Herb; Climber. Leaf: Compound."
  )
  wrapped <- list(descriptions_list, c("sp2"))

  mat <- flora_build_matrix(wrapped)
  expect_setequal(rownames(mat), c("sp1", "sp3"))
  expect_equal(colnames(mat), c("Habit", "Habit", "Leaf"))
})


test_that("flora_build_matrix treats exactly two named descriptions as two taxa", {
  # Regression: a plain list of two descriptions was mistaken for the
  # list(descriptions, failures) wrapper
  descriptions <- list("sp1" = "Habit: Tree. Leaf: Simple.",
                       "sp2" = "Habit: Herb. Leaf: Compound.")
  mat <- flora_build_matrix(descriptions)
  expect_equal(rownames(mat), c("sp1", "sp2"))
  expect_equal(unname(mat["sp2", ]), c("Herb", "Compound"))
})


# ---------------------------------------------------------------------------
# Free descriptions
# ---------------------------------------------------------------------------
.free_descriptions <- function() {
  list(
    "Luetzelburgia bahiensis" = paste(
      "Shrub 1.5 m, profusely branched; branchlets puberulent.",
      "Leaves 5.5–20 cm long, (1)3–5(7)-foliolate; petiole 1.0–7.0 cm long.",
      "Panicles pendent, the main axis 4.0–11 cm long.",
      "Flowers (18–)20–28 mm long; calyx c. 6 mm.",
      "Samaras 5.5–10 cm long.", "Seeds 19–25 mm long."),
    "Luetzelburgia amazonica" = paste(
      "Large tree to 35 m; branchlets glabrous.",
      "Leaves 18–39 cm long, 7–11-foliolate.",
      "Panicles erect, main axis 8.5–14.5 cm long.",
      "Flowers 13–16 mm long; standard with a purple spot (M. Nee 34997).",
      "Samaras 7.3–11 cm long.", "Seed c. 19 mm long."),
    "Luetzelburgia praecox" = paste(
      "Shrub to small tree of 1–8 m.",
      "Leaves 17–18 cm long.", "Panicles erect.",
      "Flowers 13–18 mm long.", "Samaras 7.0–8.0 cm long.", "Seeds not seen.")
  )
}

test_that("flora_build_matrix builds a taxon-by-organ matrix from free descriptions", {
  mat <- flora_build_matrix(.free_descriptions())

  expect_equal(colnames(mat), c("Habit", "Leaf", "Panicle", "Flower", "Samara", "Seed"))
  expect_equal(rownames(mat), names(.free_descriptions()))
  expect_false(any(is.na(mat)))
  expect_equal(mat["Luetzelburgia praecox", "Habit"], "Shrub to small tree of 1–8 m")
  expect_equal(mat["Luetzelburgia bahiensis", "Leaf"],
               "Leaves 5.5–20 cm long, (1)3–5(7)-foliolate; petiole 1.0–7.0 cm long")
})


test_that("flora_build_matrix merges singular and plural organ names into one column", {
  mat <- flora_build_matrix(.free_descriptions())
  expect_equal(mat["Luetzelburgia amazonica", "Seed"], "Seed c. 19 mm long")
  expect_equal(mat["Luetzelburgia praecox", "Seed"], "Seeds not seen")
})


test_that("flora_build_matrix keeps decimals, 'c.' and citations inside their organ", {
  mat <- flora_build_matrix(.free_descriptions())
  # "M. Nee 34997" must not start a new 'Nee' organ, and "c. 6 mm" must not
  # end the Flower sentence
  expect_false("Nee" %in% colnames(mat))
  expect_match(mat["Luetzelburgia amazonica", "Flower"], "(M. Nee 34997)", fixed = TRUE)
  expect_match(mat["Luetzelburgia bahiensis", "Flower"], "calyx c. 6 mm", fixed = TRUE)
})


test_that("flora_build_matrix aligns organs by name and leaves undescribed organs NA", {
  descriptions <- list(
    "sp1" = "Tree to 10 m. Leaves simple. Flowers white.",
    "sp2" = "Herb. Flowers red. Leaves compound.",
    "sp3" = "Shrub. Leaves simple."
  )
  mat <- flora_build_matrix(descriptions, type = "free")
  expect_equal(mat["sp2", "Leaf"], "Leaves compound")
  expect_equal(mat["sp2", "Flower"], "Flowers red")
  expect_true(is.na(mat["sp3", "Flower"]))
})


test_that("flora_build_matrix handles non-breaking spaces from FFB pages", {
  descriptions <- list(
    "sp1" = "Tree. Leaves simple. Seeds small.",
    "sp2" = "Shrub. Leaves compound. Seeds large."
  )
  mat <- flora_build_matrix(descriptions)
  expect_equal(colnames(mat), c("Habit", "Leaf", "Seed"))
  expect_equal(mat["sp1", "Leaf"], "Leaves simple")
})


test_that("flora_build_matrix detects controlled versus free descriptions", {
  expect_equal(.description_type(list(a = "Habit: Tree. Leaf: Simple; entire.")), "controlled")
  expect_equal(.description_type(.free_descriptions()), "free")
})


test_that("flora_build_matrix stops clearly when there is no description", {
  expect_error(flora_build_matrix(list(sp1 = NULL)), "no extracted description")
  expect_error(flora_build_matrix(list(a = "x"), type = "other"), "should be one of")
})
