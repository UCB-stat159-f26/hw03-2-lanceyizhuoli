test_that("different spellings of a name give the same Tennis Abstract id", {
  expect_equal(format_player_name("serena williams"), "SerenaWilliams")
  expect_equal(format_player_name("Serena Williams"), "SerenaWilliams")
  expect_equal(format_player_name("SerenaWilliams"), "SerenaWilliams")
  expect_equal(format_player_name("  rafael   nadal "), "RafaelNadal")
})

test_that("punctuation inside names is dropped", {
  expect_equal(format_player_name("Felix Auger-Aliassime"), "FelixAugerAliassime")
  expect_equal(format_player_name("juan martin del potro"), "JuanMartinDelPotro")
})

test_that("invalid inputs give a clear error", {
  expect_error(format_player_name(42), "single string")
  expect_error(format_player_name(c("a", "b")), "single string")
  expect_error(format_player_name(NA_character_), "single string")
  expect_error(format_player_name("   "), "contain letters")
})

test_that("ids are turned back into readable labels", {
  expect_equal(player_label("SerenaWilliams"), "Serena Williams")
  expect_equal(player_label("JuanMartinDelPotro"), "Juan Martin Del Potro")
})
