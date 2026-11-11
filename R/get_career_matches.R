#' Get the career matches of a tennis player
#'
#' Downloads the player's career file from Tennis Abstract
#' (`https://www.tennisabstract.com/jsmatches/<Name>Career.js`) and returns
#' one row per completed match, with the player's age, the opponent's age and
#' the match result.
#'
#' Female players' files list each opponent's date of birth, so the opponent's
#' age is computed from it. Male players' files already list the opponent's
#' age. The player's own date of birth is read from Tennis Abstract.
#'
#' @param player_name Name of the player, as one string. Case and spacing do
#'   not matter: `"serena williams"`, `"Serena Williams"` and
#'   `"SerenaWilliams"` all refer to the same player.
#'
#' @return A data frame with columns `player`, `date`, `tournament`,
#'   `surface`, `level`, `round`, `opponent`, `player_age`, `opp_age`,
#'   `age_diff` (`player_age - opp_age`, in years; positive means the player
#'   was older) and `result` (factor: `"won"`, `"lost"`).
#'   Matches without a `W`/`L` result or without an opponent age are dropped.
#'
#' @export
#' @examples
#' \dontrun{
#' serena <- get_career_matches("serena williams")
#' rafa <- get_career_matches("Rafael Nadal")
#' get_career_matches("GastonSanchez")   # error: player not found
#' }
get_career_matches <- function(player_name) {
  player_id <- format_player_name(player_name)
  js_text <- fetch_career_js(player_id, player_name)
  raw_matches <- rows_to_frame(parse_match_array(js_text))
  player_dob <- fetch_player_dob(player_id, js_text, is_female_file(raw_matches))
  clean_matches(raw_matches, player_dob, player_label(player_id))
}
