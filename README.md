# goshawk R package

Longitudinal biomarker/lab visualizations functions. These can be used stand alone but are also called by the
[teal.goshawk](https://github.com/insightsengineering/teal.goshawk) package which provides `teal` modules to be used
inside `teal` applications.

## Functions
- `g_boxplot`
- `g_correlationplot`
- `g_density_distribution_plot`
- `g_lineplot`
- `g_scatterplot`
- `g_spaghettiplot`
- `t_summarytable`

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
install.packages("devtools")
devtools::install_github("insightsengineering/goshawk@*release")
```

Currently, it is necessary to manually install all of the packages dependencies found on Github (for example `scda` to run the examples) before using this package.
