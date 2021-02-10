# goshawk 0.1.6.9000

* Issues with no news:

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
    - Added optional table, controlled by argument `display_center_tbl`, that displays means or medians to plot

### General
* Added additional validation and description for the `loq_legend` argument.
* Set legend label as variable label attribute of `trt_group` across all modules.

# goshawk 0.1.5

* updated LLOQ/ULOQ footnote for correlation plot

# goshawk 0.1.4

* Added labeling function and data driven LLOQ and ULOQ related footnote.

# goshawk 0.1.3.9000

* Issues with no news:

# goshawk 0.1.3

* Added Study Identifier and Analysis Variable as columns to `t_summarytable`.
* Some cosmetic changes and address check issues.

# goshawk 0.1.2

* Added argument to control the following:
  - Box: Toggle LoQ legend on/off.
  - Correlation: Toggle LoQ legend on/off, toggle visit facetting on/off.
  - Density: Toggle combined treatment line on/off.

# goshawk 0.1.1

* First release.