#' Display the progress if iterative processes as percentage value
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#'
#' @keywords internal
#'
display_progress_pct <- function(n, nmax, t0, word){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")
  prog <- paste0(round(100*n/nmax), "%")
  cat("\r", word, prog,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Print message for completed process
#'
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now
#'
#' @keywords internal
#'
finish_progress <- function(nmax, t0, word1, word2) {
  cat("\r", paste0(rep(" ", 75), collapse = ""))
  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("\r",word1, nmax, word2%&%plural(nmax), "in", ., "\n")
}

#' Add plural 's' to the written message if multiple operations done
#'
#' @param n Interger number of operations
#'
#' @keywords internal
#'
plural <- function(n) {
  ifelse(n <= 1, "", "s")
}

#' Concatenate without separator
#'
#' \%&\% pastes two strings by "".
#'
#' @keywords internal
#'
'%&%' <- function(a, b) paste0(a, b)


#' Convert number to character and add leading zeros based on existing IDs
#'
#' @param i ID i which is converted
#' @param ids Numeric vector of existing IDs
#'
#' @returns The value of i as character with leading zeros
#'
#' @keywords internal
#'
add_lead_zeros <- function(i, ids) {
  n_digit <- nchar(as.character(max(ids)))
  num_fmt <- paste0('%0', n_digit, 'd')
  sprintf(num_fmt, i)
}
