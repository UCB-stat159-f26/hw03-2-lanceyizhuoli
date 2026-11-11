# Position (1-based) of each field inside one match array.
# Female files: position 17 = opponent's date of birth ("19770119").
# Male files:   position 17 = opponent's age at the match.
match_columns <- c(date = 1, tournament = 2, surface = 3, level = 4, result = 5,
                   round = 9, opponent = 12, opp_birth_or_age = 17)

clean_columns <- c("player", "date", "tournament", "surface", "level", "round",
                   "opponent", "player_age", "opp_age", "age_diff", "result")

parse_match_array <- function(js_text) {
  pattern <- "(?s)\\[\\s*\\[.*?\\]\\s*\\]"
  blocks <- regmatches(js_text, gregexpr(pattern, js_text, perl = TRUE))[[1]]
  if (length(blocks) == 0) stop("No match array found in the career file.", call. = FALSE)
  rows <- lapply(blocks, jsonlite::fromJSON, simplifyVector = FALSE)
  unique(unlist(rows, recursive = FALSE))
}

field_value <- function(row, i) {
  if (length(row) < i || is.null(row[[i]])) return(NA_character_)
  as.character(row[[i]])
}

rows_to_frame <- function(rows) {
  cols <- lapply(match_columns, function(i) vapply(rows, field_value, character(1), i = i))
  as.data.frame(cols, stringsAsFactors = FALSE)
}

is_birth_date <- function(x) {
  grepl("^[0-9]{8}$", x)
}

is_female_file <- function(raw_matches) {
  any(is_birth_date(raw_matches$opp_birth_or_age))
}

age_in_years <- function(birth_date, on_date) {
  as.numeric(on_date - birth_date) / 365.25
}

opponent_age <- function(match_date, opp_birth_or_age) {
  is_dob <- is_birth_date(opp_birth_or_age)
  birth <- as.Date(ifelse(is_dob, opp_birth_or_age, NA), format = "%Y%m%d")
  age <- suppressWarnings(as.numeric(ifelse(is_dob, NA, opp_birth_or_age)))
  ifelse(is_dob, age_in_years(birth, match_date), age)
}

add_age_columns <- function(matches, player_dob) {
  matches$player_age <- age_in_years(player_dob, matches$date)
  matches$opp_age <- opponent_age(matches$date, matches$opp_birth_or_age)
  matches$age_diff <- matches$player_age - matches$opp_age
  matches
}

clean_matches <- function(raw_matches, player_dob, player = NA_character_) {
  matches <- raw_matches[raw_matches$result %in% c("W", "L"), ]
  matches$date <- as.Date(matches$date, format = "%Y%m%d")
  matches <- add_age_columns(matches, player_dob)
  matches$result <- factor(ifelse(matches$result == "W", "won", "lost"), levels = c("won", "lost"))
  matches$player <- rep(player, nrow(matches))
  matches <- matches[!is.na(matches$date) & !is.na(matches$opp_age), ]
  rownames(matches) <- NULL
  matches[, clean_columns]
}
