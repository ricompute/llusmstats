% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prob-odds.R
\name{o_to_p}
\alias{o_to_p}
\title{Convert odds to probability.}
\usage{
o_to_p(odds, print = FALSE)
}
\arguments{
\item{odds}{A single odds, either in the format of character ratio odds
(e.g., \code{"3:1"}) or numeric decimal odds (e.g., \code{0.5} or
\code{9}).}

\item{print}{A logical indicating whether to print a sentence description of
the calculated odds for the given probaiblity.}
}
\value{
The output will be a decimal probability. If \code{print = TRUE}, the
  decimal probability will be returned invisibly.
}
\description{
\code{o_to_p()} converts decimal or ratio odds to probability.
}
\details{
If \code{print = TRUE}, an explanatory sentence will be printed in the
form "If the odds of an event occuring are <odds>, the probability
is <decimal probability>."
}
\examples{
o_to_p("9:1")
o_to_p("3:1", print = TRUE)
calculated_prob <- o_to_p(0.5, print = TRUE)
calculated_prob
}
\seealso{
\code{\link{p_to_o}} for converting probability to odds.
}
