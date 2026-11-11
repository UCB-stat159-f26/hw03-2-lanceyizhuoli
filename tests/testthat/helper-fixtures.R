# Read a mock-up career file from tests/testthat/fixtures/ as one string
read_fixture <- function(file_name) {
  paste(readLines(test_path("fixtures", file_name), warn = FALSE), collapse = "\n")
}

# Stand-in for read_remote(): serves the fixtures instead of tennisabstract.com.
# Any player other than the two mock players gets NULL, like a 404.
fake_read_remote <- function(url) {
  if (grepl("SerenaWilliamsCareer.js", url, fixed = TRUE)) return(read_fixture("FemalePlayerCareer.js"))
  if (grepl("RafaelNadalCareer.js", url, fixed = TRUE)) return(read_fixture("MalePlayerCareer.js"))
  if (grepl("p=SerenaWilliams", url, fixed = TRUE)) return("var fullname = 'Serena Williams';\nvar dob = 19810926;")
  if (grepl("p=RafaelNadal", url, fixed = TRUE)) return("var fullname = 'Rafael Nadal';\nvar dob = 19860603;")
  NULL
}
