# Validate line arguments given in the parameters against the data.

helper function to be called by
[`geom_axes_lines()`](https://insightsengineering.github.io/goshawk/reference/geom_axes_lines.md)

## Usage

``` r
validate_line_args(data, vars = character(0), color = "green", label = vars)
```

## Arguments

- data:

  (`data.frame`)\
  should contain `vars` which will be used to create the plot.

- vars:

  (`character`)\
  names of variables to take the values from. Only first value from the
  variable will be applied.

- color:

  (`character`)\
  colors for the lines defined by variables.

- label:

  (`character`)\
  labels for the legend to the lines defined by variables.

## Value

(`data.frame`) containing the `values`,`colors` and `labels` fields
defining attributes for horizontal or vertical lines.
