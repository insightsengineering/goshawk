# goshawk R package

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/goshawk/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/goshawk/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/goshawk/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/goshawk/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/goshawk/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/goshawk/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/goshawk?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/goshawk?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/goshawk)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/goshawk)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/goshawk)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/goshawk)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/goshawk)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/goshawk)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/goshawk/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/goshawk/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/goshawk?color=red\&label=open%20issues)](https://github.com/insightsengineering/goshawk/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

Longitudinal biomarker/lab visualizations functions. These can be used stand alone but are also called by the
[teal.goshawk](https://insightsengineering.github.io/teal.goshawk/) package which provides `teal` modules to be used
inside `teal` applications.

## Functions
<!-- markdownlint-disable MD007 MD030 -->
-   `g_boxplot`
-   `g_correlationplot`
-   `g_density_distribution_plot`
-   `g_lineplot`
-   `g_spaghettiplot`
-   `t_summarytable`
<!-- markdownlint-enable MD007 MD030 -->

## Installation

For releases from August 2022 it is recommended that you [create and use a GitHub PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/goshawk@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/goshawk.svg)](https://starchart.cc/insightsengineering/goshawk)

### Stargazers

[![Stargazers repo roster for @insightsengineering/goshawk](https://reporoster.com/stars/insightsengineering/goshawk)](https://github.com/insightsengineering/goshawk/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/goshawk](https://reporoster.com/forks/insightsengineering/goshawk)](https://github.com/insightsengineering/goshawk/network/members)
