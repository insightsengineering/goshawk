# goshawk 0.1.6

### Enhancements
* Added optional rug plot to `g_density_distribution_plot`.
* Custom line type and custom symbol are now supported in `g_lineplot` in case of line splitting.
* Added minimum records threshold to `g_lineplot`.
* Changing the font size of values in the `g_lineplot` table is now enabled. Also changed the name of the `font_size` 
argument to `plot_font_size`.

### Miscellaneous
* Added additional validation and description for the `loq_legend` argument.
* Decreased width of error bars in `g_lineplot`.
* Set legend label as variable label attribute of `trt_group` across all modules.
* Changed `facet` to `facet_var` in `g_boxplot`.
* Set legend orientation horizontal in `g_lineplot`.

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