#' Plot a player's matches by age and age gap with the opponent
#'
#' Scatter plot in the style of the New York Times graphic of Serena
#' Williams's matches (without annotations): one point per match, the
#' player's age on the x-axis, the age difference with the opponent
#' (`player_age - opp_age`) on the y-axis, and color for won/lost.
#'
#' @param matches Data frame returned by [get_career_matches()].
#'
#' @return A `ggplot` object (printing it draws the plot).
#'
#' @export
#' @examples
#' \dontrun{
#' serena <- get_career_matches("serena williams")
#' plot_career_matches(serena)
#' }
plot_career_matches <- function(matches) {
  check_matches(matches)
  player <- matches$player[1]
  ggplot2::ggplot(matches, ggplot2::aes(x = .data$player_age, y = .data$age_diff,
                                        color = .data$result)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray40") +
    ggplot2::geom_point(size = 1, alpha = 0.8) +
    ggplot2::scale_color_manual(values = c(won = "#29b6e8", lost = "#8e2a8b"),
                                labels = c(won = "Match won", lost = "Match lost"),
                                drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(10, 50, 2), position = "top") +
    ggplot2::scale_y_continuous(breaks = seq(-40, 40, 10), labels = format_gap_labels) +
    ggplot2::labs(title = sprintf("%s's Professional Matches, by Age", player),
                  x = sprintf("%s's age", player), y = NULL, color = NULL,
                  caption = "Source: Tennis Abstract") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
}

check_matches <- function(matches) {
  needed <- c("player", "player_age", "age_diff", "result")
  if (!is.data.frame(matches) || !all(needed %in% names(matches))) {
    stop("`matches` must be a data frame returned by get_career_matches().", call. = FALSE)
  }
  if (nrow(matches) == 0) stop("`matches` has no matches to plot.", call. = FALSE)
  invisible(matches)
}

format_gap_labels <- function(breaks) {
  labels <- ifelse(breaks > 0, paste0("+", breaks), as.character(breaks))
  labels[which(breaks == 0)] <- "Same age"
  labels
}
