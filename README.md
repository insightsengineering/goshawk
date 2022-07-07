# goshawk R package

Longitudinal biomarker/lab visualizations functions. These can be used stand alone but are also called by the
[teal.goshawk](https://insightsengineering.github.io/teal.goshawk) package which provides `teal` modules to be used
inside `teal` applications.

## Functions
<!-- markdownlint-disable MD007 MD030 -->
-   `g_boxplot`
-   `g_correlationplot`
-   `g_density_distribution_plot`
-   `g_lineplot`
-   `g_scatterplot`
-   `g_spaghettiplot`
-   `t_summarytable`
<!-- markdownlint-enable MD007 MD030 -->

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/goshawk@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda) package.
