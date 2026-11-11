test_that("the match array of a female career file is parsed", {
  rows <- parse_match_array(read_fixture("FemalePlayerCareer.js"))
  expect_length(rows, 3)
  expect_equal(rows[[1]][[1]], "19951030")
  expect_equal(rows[[1]][[12]], "Anne Miller")
})

test_that("the match array of a male career file is parsed", {
  rows <- parse_match_array(read_fixture("MalePlayerCareer.js"))
  expect_length(rows, 3)
  expect_equal(rows[[3]][[2]], "Seville CH")
})

test_that("text without a match array gives an error", {
  expect_error(parse_match_array("<html>Not found</html>"), "No match array")
})

test_that("rows become a data frame with the selected fields", {
  raw <- rows_to_frame(parse_match_array(read_fixture("FemalePlayerCareer.js")))
  expect_s3_class(raw, "data.frame")
  expect_equal(names(raw), names(match_columns))
  expect_equal(raw$opponent[2], "Alexia Dechaume Balleret")
  expect_equal(raw$opp_birth_or_age[1], "19770119")
})

test_that("short rows and JSON nulls become NA instead of failing", {
  raw <- rows_to_frame(list(list("20200101", "Mock Open", NULL)))
  expect_true(is.na(raw$surface))
  expect_true(is.na(raw$opponent))
})

test_that("female and male files are told apart", {
  female <- rows_to_frame(parse_match_array(read_fixture("FemalePlayerCareer.js")))
  male <- rows_to_frame(parse_match_array(read_fixture("MalePlayerCareer.js")))
  expect_true(is_female_file(female))
  expect_false(is_female_file(male))
})
