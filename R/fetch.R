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

career_js_url <- function(player_id) {
  sprintf("https://www.tennisabstract.com/jsmatches/%sCareer.js", player_id)
}

player_page_url <- function(player_id, female) {
  page <- if (female) "wplayer.cgi" else "player.cgi"
  sprintf("https://www.tennisabstract.com/cgi-bin/%s?p=%s", page, player_id)
}

read_remote <- function(url) {
  tryCatch(
    paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
}

fetch_career_js <- function(player_id, player_name) {
  url <- career_js_url(player_id)
  js_text <- read_remote(url)
  if (is.null(js_text) || !grepl("\\[\\s*\\[", js_text)) {
    stop(sprintf(paste0("No Tennis Abstract career data found for \"%s\" (tried %s). ",
                        "Check the spelling, e.g. \"Serena Williams\" or \"RafaelNadal\"."),
                 player_name, url), call. = FALSE)
  }
  js_text
}
