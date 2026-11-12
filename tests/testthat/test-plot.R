mock_serena <- function() {
  raw <- rows_to_frame(parse_match_array(read_fixture("FemalePlayerCareer.js")))
  clean_matches(raw, as.Date("1981-09-26"), "Serena Williams")
}

test_that("the plot maps age, age gap and result", {
  serena <- mock_serena()
  p <- plot_career_matches(serena)
  expect_s3_class(p, "ggplot")
  points <- ggplot2::layer_data(p, 2)  
  expect_equal(points$x, serena$player_age)
  expect_equal(points$y, serena$age_diff)
  expect_equal(unique(points$colour), "#8e2a8b")   
  expect_match(p$labels$title, "Serena Williams")
})

test_that("bad input gives a meaningful error", {
  expect_error(plot_career_matches("serena"), "get_career_matches")
  expect_error(plot_career_matches(data.frame(x = 1)), "get_career_matches")
  expect_error(plot_career_matches(mock_serena()[0, ]), "no matches")
})

test_that("y-axis labels mark the same-age line", {
  expect_equal(format_gap_labels(c(-10, 0, 10, NA)), c("-10", "Same age", "+10", NA))
})
