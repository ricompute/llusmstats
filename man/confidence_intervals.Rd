% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confints.R
\name{confidence_intervals}
\alias{confidence_intervals}
\alias{pairwise_lsd_confint}
\alias{pairwise_bonf_confint}
\title{Produce pairwise confidence intervals after one-way ANOVA.}
\usage{
pairwise_lsd_confint(aov, level = 0.95)

pairwise_bonf_confint(aov, level = 0.95)
}
\arguments{
\item{aov}{An \code{aov} object containing a one-way ANOVA.}

\item{level}{A decimal confidence level for the confidence intervals.}
}
\value{
The output will be a \code{data.frame} with the following columns:
    \itemize{
        \item \strong{Groups}: A character string showing which group means are being compared.
        \item \strong{Difference}: The difference between the group means.
        \item \strong{Lower}: The lower bound of the confidence interval.
        \item \strong{Upper}: The upper bound of the confidence interval.
        \item \strong{Sig}: A logical indicating whether the difference between group means is significant at the given confidence level. If \code{TRUE}, the confidence interval does not contain 0.
    }
}
\description{
\code{pairwise_lsd_confint()} and \code{pairwise_bonf_confint()} produce
Fisher's Least Significant Difference and Bonferroni confidence intervals,
respectively, for differences between group means after one-way ANOVA.
}
\details{
These functions are currently designed to handle only one-way ANOVA objects.
}
\examples{
iris_aov <- aov(Sepal.Length ~ Species, iris)
pairwise_lsd_confint(iris_aov)
if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(pairwise_bonf_confint(iris_aov))
} else {
    pairwise_bonf_confint(iris_aov)
}


}
