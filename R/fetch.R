# Private helpers that build Tennis Abstract URLs and download files.

# "serena williams" / "Serena Williams" / "SerenaWilliams" -> "SerenaWilliams"
format_player_name <- function(player_name) {
  if (!is.character(player_name) || length(player_name) != 1 || is.na(player_name)) {
    stop("`player_name` must be a single string, e.g. \"Serena Williams\".", call. = FALSE)
  }
  words <- gsub("[^A-Za-z]", "", strsplit(trimws(player_name), "\\s+")[[1]])
  words <- words[nzchar(words)]
  if (length(words) == 0) stop("`player_name` must contain letters.", call. = FALSE)
  paste0(toupper(substr(words, 1, 1)), substring(words, 2), collapse = "")
}

player_label <- function(player_id) {
  gsub("([a-z])([A-Z])", "\\1 \\2", player_id)}