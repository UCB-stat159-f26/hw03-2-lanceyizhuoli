# read_remote() is replaced by fake_read_remote() (see helper-fixtures.R),
# so none of these tests contact tennisabstract.com.

test_that("dates of birth are found in page text", {
  expect_equal(extract_dob("var dob = 19810926;"), as.Date("1981-09-26"))
  expect_equal(extract_dob("var dob = '19860603';"), as.Date("1986-06-03"))
  expect_true(is.na(extract_dob("no birth date here")))
})

test_that("URLs point to the right Tennis Abstract files", {
  expect_equal(career_js_url("RafaelNadal"),
               "https://www.tennisabstract.com/jsmatches/RafaelNadalCareer.js")
  expect_match(player_page_url("SerenaWilliams", female = TRUE), "wplayer\\.cgi\\?p=SerenaWilliams$")
  expect_match(player_page_url("RafaelNadal", female = FALSE), "/player\\.cgi\\?p=RafaelNadal$")
})

test_that("a female player's matches are returned for any spelling", {
  local_mocked_bindings(read_remote = fake_read_remote)
  serena <- get_career_matches("serena williams")
  expect_equal(nrow(serena), 3)
  expect_equal(serena$player[1], "Serena Williams")
  expect_equal(serena$player_age[1], 14.092, tolerance = 1e-3)
  expect_equal(get_career_matches("SerenaWilliams"), serena)
})

test_that("a male player's matches are returned", {
  local_mocked_bindings(read_remote = fake_read_remote)
  rafa <- get_career_matches("Rafael Nadal")
  expect_equal(rafa$opp_age, c(22.5, 25.1, 19.8))
  expect_equal(rafa$player[1], "Rafael Nadal")
})

test_that("an unknown player gives a meaningful error", {
  local_mocked_bindings(read_remote = fake_read_remote)
  expect_error(get_career_matches("GastonSanchez"), "No Tennis Abstract career data found for \"GastonSanchez\"")
  expect_error(get_career_matches("Gaston Sanchez"), "Check the spelling")
})

test_that("a missing date of birth gives a meaningful error", {
  local_mocked_bindings(read_remote = function(url) {
    if (grepl("Career.js", url)) read_fixture("MalePlayerCareer.js") else NULL
  })
  expect_error(get_career_matches("Rafael Nadal"), "Could not find a date of birth")
})
