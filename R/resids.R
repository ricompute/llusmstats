#' Plot residuals vs. fitted values.
#'
#' Plot model residuals vs. fitted values with ggplot2 to assess a model fit.
#'
#' @param model An \code{lm} or \code{aov} model object.
#' @param resid_type A string indicating which type of residuals to use. The
#'   default \code{"std"} uses standardized residuals, \code{"unstd"} uses
#'   unstandardized residuals, and \code{"both"} produces both plots stacked
#'   with \code{"unstd"} on top and \code{"std"} on the bottom.
#' @param outlier_2sd_lines A logical indicating whether to draw horizontal red
#'   lines on the standardized residual plot at a y-value of +/- 2.
#' @param title A string indicating what to title the plot.
#' @param subtitle A logical indicating whether to include the default subtitle.
#' @param xlab A string indicating what to label the x-axis.
#' @param ylab A string indicating what to label the y-axis.
#' @param ... Additional arguments passed to
#'   \code{ggplot2::\link[ggplot2]{theme}()}.
#' @return If a single plot type (\code{"std"} or \code{"unstd"}) is specified,
#'   a ggplot2 plot object is returned. If \code{"both"} plot types are
#'   requested, a call to \code{gridExtra::\link[gridExtra]{grid.arrange}} draws
#'   on the current device and a gtable is returned.
#' @seealso \code{\link{resid_hist_qq}} for residual histograms and Q-Q plots.
#' @examples
#' mtcars_lm <- lm(mpg ~ wt, mtcars)
#' resid_fitted_plot(mtcars_lm)
#' resid_fitted_plot(mtcars_lm, type = "unstd")
#' resid_fitted_plot(mtcars_lm, type = "both")
#'
#' @export
resid_fitted_plot <- function(model, resid_type = "std", outlier_2sd_lines = TRUE,
                               title = "Residuals vs. Fitted Values",
                               subtitle = TRUE,
                               xlab = "Fitted Values",
                               ylab = "Residuals",
                               ...) {
    # Put residuals into a data frame to plot
    resid_df <- data.frame(resids = model$residuals,
                           stdresids = rstandard(model),
                           fitted = model$fitted.values)

    # Plot unstandardized residuals against fitted values
    resid_plot <- ggplot2::ggplot(resid_df,
                                  ggplot2::aes(x = fitted, y = resids)) +
        ggalt::geom_lollipop() + ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(title = title,
                      subtitle = ifelse(subtitle,
                                        "Unstandardized residuals and fitted values",
                                        ""),
                      x = xlab,
                      y = ylab) +
        ggplot2::theme(...)
    # Plot standardized residuals against fitted values
    std_resid_plot <- ggplot2::ggplot(resid_df,
                                      ggplot2::aes(x = fitted, y = stdresids)) +
        ggalt::geom_lollipop() + ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(title = title,
                      subtitle = ifelse(subtitle,
                                        "Standardized residuals and fitted values",
                                        ""),
                      x = xlab,
                      y = ylab) +
        ggplot2::theme(...)
    if (outlier_2sd_lines) {
        std_resid_plot <- std_resid_plot +
            ggplot2::geom_hline(yintercept = 2, color = "red") +
            ggplot2::geom_hline(yintercept = -2, color = "red")
    }

    if (resid_type == "std") {
        return(std_resid_plot)
    } else if (resid_type == "unstd") {
        return(resid_plot)
    } else if (resid_type == "both") {
        # Arrange and display the two residual plots on top of each other
        return(gridExtra::grid.arrange(resid_plot, std_resid_plot, nrow = 2))
    } else {
        stop("Invalid resid_type. ",
             "Valid options are \"std\" (default), \"unstd\", or \"both\".")
    }
}


#' Plot residual histograms and Q-Q plots.
#'
#' Plot histograms and Q-Q plots of model residuals to assess residual normality.
#'
#' @param model An \code{lm} or \code{aov} model object.
#' @param plot_type A string indicating which type of plot to produce. The
#'   default \code{"both"} produces both histograms and Q-Q plots side by side,
#'   \code{"hist"} produces only histograms, and \code{"qq"} produces only Q-Q
#'   plots.
#' @param resid_type A string indicating which type of residuals to use. The
#'   default \code{"std"} uses standardized residuals, \code{"unstd"} uses
#'   unstandardized residuals, and \code{"both"} produces both plots stacked
#'   with \code{"unstd"} on top and \code{"std"} on the bottom.
#' @param bins How many bins to use for the histograms.
#' @param qqline A logical indicating whether to include a qqline on the Q-Q
#'   plots.
#' @param htitle A string indicating what to title the histograms.
#' @param qtitle A string indicating what to title the Q-Q plots.
#' @param subittle A logical indicating whether to include the default
#'   subtitles.
#' @param hxlab A string indicating what to title the histogram x-axes.
#' @param qxlab A string indicating what to title the Q-Q plot x-axes.
#' @param hylab A string indicating what to title the histogram y-axes.
#' @param qylab A string indicating what to title the Q-Q plot y-axes.
#' @param ... Additional arguments passed to
#'   \code{ggplot2::\link[ggplot2]{theme}()}.
#' @return If single \code{plot_} and \code{resid_} \code{type}s are specified
#'   (i.e., a single histogram or Q-Q plot is requested), a ggplot2 plot object
#'   is returned. If more than one plot is requested, a call to
#'   \code{gridExtra::\link[gridExtra]{grid.arrange}} draws on the current
#'   device and a gtable is returned.
#' @seealso \code{\link{resid_fitted_plot}} for residual vs. fitted values
#'   plots.
#' @examples
#' mtcars_lm <- lm(mpg ~ wt, mtcars)
#' resid_hist_qq(mtcars_lm)
#' resid_hist_qq(mtcars_lm, plot_type = "qq", resid_type = "unstd")
#' resid_hist_qq(mtcars_lm, resid_type = "both")
#' @export
resid_hist_qq <- function(model, plot_type = "both", resid_type = "std",
                          bins = 9, qqline = TRUE,
                          htitle = "Histogram",
                          qtitle = "Normal Q-Q Plot",
                          subtitle = TRUE,
                          hxlab = "Residuals",
                          qxlab = "Theoretical Quantiles",
                          hylab = "Count",
                          qylab = "Sample Quantiles",
                          ...) {
    # Check for valid plot_type and resid_type
    if (!(plot_type %in% c("both", "hist", "qq"))) {
        stop("Invalid plot_type. ",
             "Valid options are \"both\" (default), \"hist\", or \"qq\".")
    }
    if (!(resid_type %in% c("std", "unstd", "both"))) {
        stop("Invalid resid_type. ",
             "Valid options are \"std\" (default), \"unstd\", or \"both\".")
    }
    # Put residuals into a data frame to plot
    resid_df <- data.frame(resids = model$residuals,
                           stdresids = rstandard(model),
                           fitted = model$fitted.values)

    # Plot histogram of unstandardized residuals
    resid_hist <- ggplot2::ggplot(resid_df, ggplot2::aes(x = resids)) +
        ggplot2::geom_histogram(bins = bins) +
        ggplot2::labs(title = htitle,
                      subtitle = ifelse(subtitle,
                                        "Unstandardized Residuals",
                                        ""),
                      x = hxlab,
                      y = hylab) +
        ggplot2::theme(...)
    # Plot histogram of standardized residuals
    std_resid_hist <- ggplot2::ggplot(resid_df, ggplot2::aes(x = stdresids)) +
        ggplot2::geom_histogram(bins = bins) +
        ggplot2::labs(title = htitle,
                      subtitle = ifelse(subtitle,
                                        "Standardized Residuals",
                                        ""),
                      x = hxlab,
                      y = hylab) +
        ggplot2::theme(...)

    # Q-Q plot with unstandardized residuals
    resid_qq <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resids)) +
        ggplot2::stat_qq() +
        ggplot2::labs(title = qtitle,
                      subtitle = ifelse(subtitle,
                                        "Unstandardized Residuals",
                                        ""),
                      x = qxlab,
                      y = qylab) +
        ggplot2::theme(...)
    if (qqline) {
        resid_qq <- resid_qq +
            ggplot2::geom_abline(intercept = mean(resid_df$resids),
                                 slope = sd(resid_df$resids))
    }
    # Q-Q plot with standardized residuals
    std_resid_qq <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = stdresids)) +
        ggplot2::stat_qq() +
        ggplot2::labs(title = qtitle,
                      subtitle = ifelse(subtitle,
                                        "Standardized Residuals",
                                        ""),
                      x = qxlab,
                      y = qylab) +
        ggplot2::theme(...)
    if (qqline) {
        std_resid_qq <- std_resid_qq +
            ggplot2::geom_abline()
    }

    if (plot_type == "both" & resid_type == "std") {
        return(gridExtra::grid.arrange(std_resid_hist, std_resid_qq, nrow = 1))
    } else if (plot_type == "both" & resid_type == "unstd") {
        return(gridExtra::grid.arrange(resid_hist, resid_qq, nrow = 1))
    } else if (plot_type == "both" & resid_type == "both") {
        return(gridExtra::grid.arrange(resid_hist, resid_qq,
                                       std_resid_hist, std_resid_qq,
                                       ncol = 2, nrow = 2))
    } else if (plot_type == "hist" & resid_type == "std") {
        return(std_resid_hist)
    } else if (plot_type == "hist" & resid_type == "unstd") {
        return(resid_hist)
    } else if (plot_type == "hist" & resid_type == "both") {
        return(gridExtra::grid.arrange(resid_hist, std_resid_hist, nrow = 2))
    } else if (plot_type == "qq" & resid_type == "std") {
        return(std_resid_qq)
    } else if (plot_type == "qq" & resid_type == "unstd") {
        return(resid_qq)
    } else if (plot_type == "qq" & resid_type == "both") {
        return(gridExtra::grid.arrange(resid_qq, std_resid_qq, nrow = 2))
    }
}