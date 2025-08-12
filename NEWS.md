# goshawk 0.1.19.9001

* `g_scatterplot` was hard deprecated (#267).

# goshawk 0.1.19

### Fixes

* Enhance slope functions that is used in `g_correlationplot` when `reg_line = TRUE` (#255).
* Remove analysis variable name from plot title in `g_spaghettiplot` (#249).
* User can pass custom x-axis label to `g_lineplot` and `g_spaghettiplot` (#250).
* Update links and installation instructions in README.

# goshawk 0.1.18

### Fixes

* Change the automated label on `g_spaghettiplot`.

# goshawk 0.1.17

### Miscellaneous

* Update `g_lineplot` and `g_spaghettiplot` to include a `dot_size` parameter for adjusting the size of plot points.

# goshawk 0.1.16

### Miscellaneous
* Add `loq_flag_var` argument to plots.
* Remove package prefix from examples.
* Replace soft deprecated `ggplot2` functions with their current equivalents, e.g. `aes_string()` with `aes()`.

### Fixes

* Update `h_identify_loq_values` to filter for records with `LOQFL == "Y"` before calculating `LOQ`.
* Fix an issue with vertical range lines displayed only when there are any other lines included.
* Fix partial attribute matching.


# goshawk 0.1.15

### Miscellaneous

* Deprecated `g_scatterplot`. Use `g_correlationplot` instead.
* Removed `scda` package dependency from examples.

# goshawk 0.1.14

### Fixes

* Fixed `h_caption_loqs_label` to handle multiple parameters when data frame is returned from `h_identify_loq_values`.

### Enhancements

* Implemented `nestcolor` with slight refactoring to `g_lineplot`, `g_density_distribution_plot` and added `nestcolor` in examples without custom color manuals.
* `g_spaghettiplot` has a new parameter `facet_scales` that allows for releasing scales in plot facets.

### Miscellaneous

* Updated installation instructions.

# goshawk 0.1.13

### Fixes

* Fixed incorrect color of arbitrary lines legend elements in `geom_arb_hline` and `geom_arb_vline` functions.

### Miscellaneous

* Added a template to the `pkgdown` site.

# goshawk 0.1.12

### Fixes

* Fixed plots legends to keep specific items order.

#### Plot lines

* Improve straight lines to have a standard `ggplot2` legend format. Each line type (horizontal/vertical, arbitrary/range) has a separate legend entry.
* Added labels to the axes equaling the value of a horizontal or vertical line that is added to the plot.
* Standardized the argument to control the range of the axes to `xlim` and `ylim`, complying with `ggplot2`.

# goshawk 0.1.11

### Enhancements

* Renamed `visit_var` as `facet_var` in `t_summarytable` and allowed it to be `NULL` when there is no facetting.
* Extended the functionality of the parameters `hline_arb` (and `vline_arb` for `g_correlationplot`) to a vector of arbitrary length, allowing users to add any number of arbitrary horizontal or vertical lines to `g_spaghettiplot`, `g_boxplot`, `g_density_distribution_plot` and `g_correlationplot`.

### Miscellaneous

* Added R version requirement `R >= 3.6`.
* Removed dependency on the `test.nest` package.
* Removed dependency on the `utils.nest` package and replaced its functions with equivalents from the `checkmate` package.

# goshawk 0.1.10

### New Features

* Lab normal range and `LOQs` horizontal line feature in `g_spaghettiplot`, `g_boxplot` and `g_correlationplot`.

### Breaking Changes

* Allow arbitrary horizontal line arguments in `g_spaghettiplot`, `g_boxplot`, `g_density_distribution_plot` and `g_correlationplot` and vertical line arguments in `g_correlationplot`. This functionality has changed the arguments required to use the functions:
  - `hline` replaced by `hline_arb`, `hline_arb_color` and `hline_arb_label` in the above functions.
  - `vline` replaced by `vline_arb_var`, `vline_arb_color` and `vline_arb_label` in `g_correlationplot`.

### Miscellaneous

* Updated `LICENCE` and `README` with new package references.
* Updated examples and documentation using `scda` synthetic data instead of `random.cdisc.data`.
* Added `error_on_lint: TRUE` to `.lintr`.

# goshawk 0.1.9

* Updated `g_density_distribution_plot` legend to show lines not squares.
* Fixed a bug in `g_correlationplot` when facetting by treatment variable would nullify facetting by visit variable.

# goshawk 0.1.8

* Removed warnings in `g_boxplot` and `t_summarytable` when one group has no data.
* `g_boxplot` and `g_correlationplot` have been modified to always have empty points, regardless of whether `loq_legend` parameter is set to `TRUE` or `FALSE`.

# goshawk 0.1.7

* Added optional table in `g_lineplot`, controlled by argument `display_center_tbl`, that displays means or medians of each group of each time point corresponding to the plot.

# goshawk 0.1.6

### Enhancements
* `g_boxplot`
    - Argument changes: `facet` --> `facet_var`
* `g_density_distribution_plot`
   - Rug plot option added.
* `g_lineplot`:
    - Argument changes: `font_size` --> `plot_font_size`
    - Line and symbol type can now be configured. especially useful if line splitting is used.
    - Added minimum records threshold.
    - Table font size can now be controlled.
    - Decreased width of error bars.
    - Horizontal and columned legend orientation.

### General
* Added additional validation and description for the `loq_legend` argument.
* Set legend label as variable label attribute of `trt_group` across all modules.

# goshawk 0.1.5

* updated `LLOQ`/`ULOQ` footnote for correlation plot

# goshawk 0.1.4

* Added labeling function and data driven `LLOQ` and `ULOQ` related footnote.

# goshawk 0.1.3.9000

* Issues with no news:

# goshawk 0.1.3

* Added Study Identifier and Analysis Variable as columns to `t_summarytable`.
* Some cosmetic changes and address check issues.

# goshawk 0.1.2

* Added argument to control the following:
  - Box: Toggle `LoQ` legend on/off.
  - Correlation: Toggle `LoQ` legend on/off, toggle visit facetting on/off.
  - Density: Toggle combined treatment line on/off.

# goshawk 0.1.1

* First release.
