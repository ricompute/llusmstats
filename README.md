# llusmstats

This completely unofficial package includes miscellaneous helper functions for
statistics classes in the Loma Linda University School of Medicine Department of
Basic Sciences.

## Installation

To install the package:

```r
# If devtools is not installed, 
# install.package("devtools")
devtools::install_github("ricompute/llusmstats")
```

## Contents

See each function's help page (e.g., `?resid_hist_qq()`) for usage and examples.

### Residual Plots

`resid_fitted_plot()` plots model residuals vs. fitted values.

`resid_hist_qq()` plots model residual histograms and Q-Q plots.

### One-way ANOVA Confidence Intervals

`pairwise_lsd_confint()` produces pairwise Fisher's Least Significant Difference confidence intervals from a one-way ANOVA object.

`pairwise_bonf_confint()` produces pairwise Bonferroni confidence intervals from a one-way ANOVA object.

### Probability and Odds

`o_to_p()` converts odds to probability.

`p_to_o()` converts probability to odds.
