test_that("flora_build_matrix builds a matrix directly from a plain description list", {
  # Note: a plain list of exactly length 2 is ambiguous with the wrapped
  # list(descriptions, failures) shape (see the length-2 test below), so this
  # uses 3 taxa to unambiguously exercise the "plain list" branch.
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
