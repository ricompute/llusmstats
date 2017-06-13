#' Produce pairwise confidence intervals after one-way ANOVA.
#'
#' \code{pairwise_lsd_confint()} and \code{pairwise_bonf_confint()} produce
#' Fisher's Least Significant Difference and Bonferroni confidence intervals,
#' respectively, for differences between group means after one-way ANOVA.
#'
#' These functions are currently designed to handle only one-way ANOVA objects.
#'
#' @param aov An \code{aov} object containing a one-way ANOVA.
#' @param level A decimal confidence level for the confidence intervals.
#'
#' @return The output will be a \code{data.frame} with the following columns:
#'     \itemize{
#'         \item \strong{Groups}: A character string showing which group means are being compared.
#'         \item \strong{Difference}: The difference between the group means.
#'         \item \strong{Lower}: The lower bound of the confidence interval.
#'         \item \strong{Upper}: The upper bound of the confidence interval.
#'         \item \strong{Sig}: A logical indicating whether the difference between group means is significant at the given confidence level. If \code{TRUE}, the confidence interval does not contain 0.
#'     }
#'
#' @examples
#' iris_aov <- aov(Sepal.Length ~ Species, iris)
#' pairwise_lsd_confint(iris_aov)
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'     knitr::kable(pairwise_bonf_confint(iris_aov))
#' } else {
#'     pairwise_bonf_confint(iris_aov)
#' }
#'
#'
#' @name confidence_intervals

#' @rdname confidence_intervals
#' @export
pairwise_lsd_confint <- function(aov, level = 0.95) {
    group_by <- names(aov$contrasts)
    df <- aov$model
    data <- colnames(df)[which(colnames(df) != group_by)]
    group_names <- unique(df[, group_by])

    t_crit <- qt((1 - (1 - level)/2),
                 summary(aov)[[1]]["Residuals", "Df"])
    MSE <- summary(aov)[[1]]["Residuals", "Mean Sq"]

    res <- data.frame()

    for (i in seq_along(group_names)[-length(group_names)]) {
        for (ii in seq(i + 1, length(group_names))) {
            n_i <- length(df[df[ , group_by] == group_names[i], data])
            n_ii <- length(df[df[ , group_by] == group_names[ii], data])
            mean_i <- mean(df[df[ , group_by] == group_names[i], data])
            mean_ii <- mean(df[df[ , group_by] == group_names[ii], data])
            difference <- mean_i - mean_ii
            lower <- difference - (t_crit * sqrt(MSE * ((1/n_i) + (1/n_ii))))
            upper <- difference + (t_crit * sqrt(MSE * ((1/n_i) + (1/n_ii))))

            res <- rbind(res,
                         data.frame(Groups = paste0(group_names[i],
                                                    " - ",
                                                    group_names[ii]),
                                    Difference = difference,
                                    Lower = lower,
                                    Upper = upper,
                                    Sig = ifelse(sign(lower) == sign(upper),
                                                 TRUE,
                                                 FALSE)))
        }
    }

    res
}

#' @rdname confidence_intervals
#' @export
pairwise_bonf_confint <- function(aov, level = 0.95) {
    group_by <- names(aov$contrasts)
    df <- aov$model
    data <- colnames(df)[which(colnames(df) != group_by)]
    group_names <- unique(df[, group_by])

    level <- 1 - ((1 - level) / length(group_names))  # Bonferroni correction
    t_crit <- qt((1 - (1 - level)/2),
                 summary(aov)[[1]]["Residuals", "Df"])
    MSE <- summary(aov)[[1]]["Residuals", "Mean Sq"]

    res <- data.frame()

    for (i in seq_along(group_names)[-length(group_names)]) {
        for (ii in seq(i + 1, length(group_names))) {
            n_i <- length(df[df[ , group_by] == group_names[i], data])
            n_ii <- length(df[df[ , group_by] == group_names[ii], data])
            mean_i <- mean(df[df[ , group_by] == group_names[i], data])
            mean_ii <- mean(df[df[ , group_by] == group_names[ii], data])
            difference <- mean_i - mean_ii
            lower <- difference - (t_crit * sqrt(MSE * ((1/n_i) + (1/n_ii))))
            upper <- difference + (t_crit * sqrt(MSE * ((1/n_i) + (1/n_ii))))

            res <- rbind(res,
                         data.frame(Groups = paste0(group_names[i],
                                                    " - ",
                                                    group_names[ii]),
                                    Difference = difference,
                                    Lower = lower,
                                    Upper = upper,
                                    Sig = ifelse(sign(lower) == sign(upper),
                                                 TRUE,
                                                 FALSE)))
        }
    }

    res
}