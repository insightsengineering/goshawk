# Add horizontal and/or vertical lines and their legend labels to a plot

This function is currently designed to be used with
[`g_boxplot`](https://insightsengineering.github.io/goshawk/reference/g_boxplot.md),
[`g_correlationplot`](https://insightsengineering.github.io/goshawk/reference/g_correlationplot.md),
[`g_spaghettiplot`](https://insightsengineering.github.io/goshawk/reference/g_spaghettiplot.md),
and ('g_density_distribution_plot'), but may also work in general.

## Usage

``` r
geom_axes_lines(
  data,
  hline_arb = numeric(0),
  hline_arb_color = "red",
  hline_arb_label = "Horizontal line",
  hline_vars = character(0),
  hline_vars_colors = "green",
  hline_vars_labels = hline_vars,
  vline_arb = numeric(0),
  vline_arb_color = "red",
  vline_arb_label = "Vertical line",
  vline_vars = character(0),
  vline_vars_colors = "green",
  vline_vars_labels = vline_vars
)
```

## Arguments

- data:

  ('data.frame') data where `hline_vars` and `vline_var` columns are
  taken from.

- hline_arb:

  ('numeric vector') value identifying intercept for arbitrary
  horizontal lines.

- hline_arb_color:

  ('character vector') optional, color for the arbitrary horizontal
  lines.

- hline_arb_label:

  ('character vector') optional, label for the legend to the arbitrary
  horizontal lines.

- hline_vars:

  ('character vector'), names of variables `(ANR*)` or values `(*LOQ)`
  identifying intercept values. Needs `data` to be specified.

- hline_vars_colors:

  ('character vector') colors for the horizontal lines defined by
  variables.

- hline_vars_labels:

  ('character vector') labels for the legend to the horizontal lines
  defined by variables.

- vline_arb:

  ('numeric vector') value identifying intercept for arbitrary vertical
  lines.

- vline_arb_color:

  ('character vector') optional, color for the arbitrary vertical lines.

- vline_arb_label:

  ('character vector') optional, label for the legend to the arbitrary
  vertical lines.

- vline_vars:

  ('character vector'), names of variables `(ANR*)` or values `(*LOQ)`
  identifying intercept values. Needs `data` to be specified.

- vline_vars_colors:

  ('character vector') colors for the vertical lines defined by
  variables.

- vline_vars_labels:

  ('character vector') labels for the legend to the vertical lines
  defined by variables.

## Value

`ggplot` object

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  goshawk:::geom_axes_lines(
    hline_arb = c(20, 25, 30),
    hline_arb_color = "red",
    hline_arb_label = "Hori Line"
  )
```
