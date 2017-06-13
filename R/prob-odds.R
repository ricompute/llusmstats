#' Convert probability to odds.
#'
#' \code{p_to_o()} converts decimal probability to odds.
#'
#' If \code{print = TRUE}, an explanatory sentence will be printed in the
#' form "If the probability of an event occuring is <probability>, the odds
#' are <decimal odds> or <ratio:odds>."
#'
#' @param probability A single numeric decimal probability (i.e., a number
#'     between 0 and 1).
#' @param print A logical indicating whether to print a sentence description of
#'     the calculated odds for the given probaiblity.
#' @return The output will be decimal odds. If \code{print = TRUE}, the decimal
#'     odds will be returned invisibly.
#' @seealso \code{\link{o_to_p}} for converting odds to probability.
#' @examples
#' p_to_o(0.5)
#' p_to_o(0.1, print = TRUE)
#' calculated_odds <- p_to_o(0.9, print = TRUE)
#' calculated_odds
#' @export
p_to_o <- function(probability, print = FALSE) {
    odds <- (probability / (1 - probability))
    odds_char <- as.character(MASS::as.fractions(odds))
    odds_char <- unlist(stringr::str_split(odds_char, "/"))
    if (length(odds_char) == 1) {
        odds <- as.integer(odds)
        o1 <- odds_char
        o2 <- "1"
        if (print) {
            cat(sprintf(paste("If the probability of an event occuring is %f,",
                              "the odds are %d or %s:%s."),
                        probability,
                        odds,
                        o1, o2))
            invisible(odds)
        } else {
            return(odds)
        }
    } else {
        o1 <- odds_char[1]
        o2 <- odds_char[2]
        if (print) {
            cat(sprintf(paste("If the probability of an event occuring is %f,",
                              "the odds are %f or %s:%s."),
                        probability,
                        odds,
                        o1, o2))
            invisible(odds)
        } else {
            return(odds)
        }
    }
}

#' Convert odds to probability.
#'
#' \code{o_to_p()} converts decimal or ratio odds to probability.
#'
#' If \code{print = TRUE}, an explanatory sentence will be printed in the
#' form "If the odds of an event occuring are <odds>, the probability
#' is <decimal probability>."
#'
#' @param odds A single odds, either in the format of character ratio odds
#'     (e.g., \code{"3:1"}) or numeric decimal odds (e.g., \code{0.5} or
#'     \code{9}).
#' @param print A logical indicating whether to print a sentence description of
#'     the calculated odds for the given probaiblity.
#' @return The output will be a decimal probability. If \code{print = TRUE}, the
#'     decimal probability will be returned invisibly.
#' @seealso \code{\link{p_to_o}} for converting probability to odds.
#' @examples
#' o_to_p("9:1")
#' o_to_p("3:1", print = TRUE)
#' calculated_prob <- o_to_p(0.5, print = TRUE)
#' calculated_prob
#' @export
o_to_p <- function(odds, print = FALSE) {
    if (class(odds) == "character" & stringr::str_detect(odds, ":")) {
        odds_char <- unlist(stringr::str_split(odds, ":"))
        o1 <- as.integer(odds_char[1])
        o2 <- as.integer(odds_char[2])
        odds_float <- o1/o2
    } else if (class(odds) %in% c("numeric", "integer")) {
        odds_float <- odds
    } else {
        stop(paste("Invalid odds format.\n",
                   "Valid options are character ratio odds (e.g., \"3:1\")\n",
                   "or numeric decimal odds (e.g., 0.5 or 9)."))
    }
    probability <- (odds_float / (1 + odds_float))
    if (print) {
        cat(sprintf(paste("If the odds of an event occuring are %s,",
                          "the probability is %f."),
                    odds,
                    probability))
        invisible(probability)
    } else {
        return(probability)
    }
}