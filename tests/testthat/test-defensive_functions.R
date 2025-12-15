test_that(".arg_check_dir handles valid and invalid input", {
  expect_equal(.arg_check_dir("my/path"), "my/path")
  expect_equal(.arg_check_dir("my/path/"), "my/path")
  expect_error(.arg_check_dir(123), "The argument dir should be a character")
})


test_that(".arg_check_path detects missing or invalid folders", {
  temp <- tempdir()
  empty_dir <- file.path(temp, "empty_folder")
  dir.create(empty_dir, showWarnings = FALSE)

  # 1. Invalid path (not a dir)
  expect_error(.arg_check_path(42, character(), list()), "should be a character")

  # 2. Dir doesn't exist
  expect_error(.arg_check_path("not_exist", character(), list()), "There is no folder")

  # 3. No dwca folders
  expect_error(.arg_check_path(empty_dir, "folder", list()), "no REFLORA-downloaded dwca")

  # 4. dwca folder but empty
  fake_dwca <- file.path(temp, "dwca_example")
  dir.create(fake_dwca, showWarnings = FALSE)
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list())), "fully empty")

  # 5. Missing key files
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list("random.txt"))),
               "files are missing")
})


test_that(".arg_check_recordYear detects bad input", {
  expect_silent(.arg_check_recordYear(c("2000", "2024")))
  expect_error(.arg_check_recordYear(c("2024", "2000")), "first year must be less")
  expect_error(.arg_check_recordYear(c("199", "2020")), "4-digit")
  expect_error(.arg_check_recordYear(c("2000", "2020", "2024")), "should be either a single year or a range")
})


test_that(".arg_check_state converts and validates state names", {
  expect_equal(.arg_check_state("BA"), "Bahia")
  expect_equal(.arg_check_state("Bahia"), "Bahia")
  expect_equal(.arg_check_state("São Paulo"), "São Paulo")
  expect_equal(.arg_check_state("Sao Paulo"), "São Paulo") # test with diacritic removed
})


test_that(".arg_check_state handles acronyms, full names, and unknowns", {
  input <- c("BA", "Minas Gerais", "Santa Cruz de la Sierra")
  result <- .arg_check_state(input)
  expect_equal(result, c("Bahia", "Minas Gerais", "Santa Cruz de la Sierra"))
})


test_that(".arg_check_state handles diacritics", {
  input <- c("Sao Paulo", "Ceara", "Espirito Santo")
  expected <- c("São Paulo", "Ceará", "Espírito Santo")
  result <- .arg_check_state(input)
  expect_equal(result, expected)
})


test_that(".arg_check_state returns unknown inputs unchanged", {
  input <- c("Unknown State", "ZZ")
  result <- .arg_check_state(input)
  expect_equal(result, c("Unknown State", "ZZ"))
})


test_that(".arg_check_state handles case-insensitive acronyms", {
  input <- c("rj", "mg", "Sp")
  result <- .arg_check_state(toupper(input))
  expect_equal(result, c("Rio de Janeiro", "Minas Gerais", "São Paulo"))
})

