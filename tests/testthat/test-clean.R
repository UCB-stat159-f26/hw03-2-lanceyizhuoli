female_raw <- function() rows_to_frame(parse_match_array(read_fixture("FemalePlayerCareer.js")))
male_raw <- function() rows_to_frame(parse_match_array(read_fixture("MalePlayerCareer.js")))

test_that("ages are measured in years of 365.25 days", {
  expect_equal(age_in_years(as.Date("2000-01-01"), as.Date("2000-01-01") + 365.25 * 2), 2)
})

test_that("opponent age comes from a birth date or directly from an age", {
  date <- as.Date(c("1995-10-30", "2001-09-10"))
  ages <- opponent_age(date, c("19770119", "22.5"))
  expect_equal(ages[1], as.numeric(date[1] - as.Date("1977-01-19")) / 365.25)
  expect_equal(ages[2], 22.5)
  expect_true(is.na(opponent_age(date[1], "")))
})

test_that("female file: ages derived from dates of birth", {
  serena <- clean_matches(female_raw(), as.Date("1981-09-26"), "Serena Williams")
  expect_equal(nrow(serena), 3)
  expect_equal(serena$date[1], as.Date("1995-10-30"))
  expect_equal(serena$player_age[1], 14.092, tolerance = 1e-3)
  expect_equal(serena$opp_age[1], 18.776, tolerance = 1e-3)
  expect_equal(serena$age_diff, serena$player_age - serena$opp_age)
})

test_that("male file: opponent ages read directly", {
  rafa <- clean_matches(male_raw(), as.Date("1986-06-03"), "Rafael Nadal")
  expect_equal(rafa$opp_age, c(22.5, 25.1, 19.8))
  expect_equal(rafa$player_age[1], as.numeric(as.Date("2001-09-10") - as.Date("1986-06-03")) / 365.25)
})

test_that("result is a won/lost factor and columns are in a fixed order", {
  rafa <- clean_matches(male_raw(), as.Date("1986-06-03"), "Rafael Nadal")
  expect_equal(as.character(rafa$result), c("lost", "lost", "won"))
  expect_equal(levels(rafa$result), c("won", "lost"))
  expect_equal(names(rafa), c("player", "date", "tournament", "surface", "level", "round",
                              "opponent", "player_age", "opp_age", "age_diff", "result"))
  expect_true(all(rafa$player == "Rafael Nadal"))
})

test_that("matches without a W/L result or an opponent age are dropped", {
  raw <- female_raw()
  raw$result[1] <- ""
  raw$opp_birth_or_age[2] <- ""
  serena <- clean_matches(raw, as.Date("1981-09-26"))
  expect_equal(nrow(serena), 1)
  expect_equal(serena$tournament, "Manhattan Beach")
})
