#' Add horizontal and/or vertical lines and their legend labels to a plot
#'
#' This function is currently designed to be used with \code{\link{g_boxplot}}, \code{\link{g_correlationplot}},
#' \code{\link{g_spaghettiplot}}, and ('g_density_distribution_plot'), but may also work in general.
#'
#' @param data ('data.frame') data where `hline_vars` and `vline_var` columns are taken from.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param hline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   Needs `data` to be specified.
#' @param hline_vars_colors ('character vector') colors for the horizontal lines defined by variables.
#' @param hline_vars_labels ('character vector') labels for the legend to the horizontal lines defined by variables.
#' @param vline_arb ('numeric vector') value identifying intercept for arbitrary vertical lines.
#' @param vline_arb_color ('character vector') optional, color for the arbitrary vertical lines.
#' @param vline_arb_label ('character vector') optional, label for the legend to the arbitrary vertical lines.
#' @param vline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   Needs `data` to be specified.
#' @param vline_vars_colors ('character vector') colors for the vertical lines defined by variables.
#' @param vline_vars_labels ('character vector') labels for the legend to the vertical lines defined by variables.
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   goshawk:::geom_axes_lines(
#'     hline_arb = c(20, 25, 30),
#'     hline_arb_color = "red",
#'     hline_arb_label = "Hori Line"
#'   )
#' @return \code{ggplot} object
#' @keywords internal
#'
geom_axes_lines <- function(data,
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
                            vline_vars_labels = vline_vars) {
  arb_hlines <- if (length(hline_arb) > 0) {
    geom_arb_hline(
      yintercept = hline_arb,
      color = hline_arb_color,
      label = hline_arb_label,
      legend_title = "Horizontal arbitrary line(s)",
      linetype = 2
    )
  }

  range_hlines <- if (length(hline_vars > 0)) {
    geom_range_hline(
      data = data,
      vars = hline_vars,
      color = hline_vars_colors,
      label = hline_vars_labels,
      linetype = 2
    )
  }

  arb_vlines <- if (length(vline_arb) > 0) {
    geom_arb_vline(
      xintercept = vline_arb,
      color = vline_arb_color,
      label = vline_arb_label,
      legend_title = "Vertical arbitrary line(s)",
      linetype = 2
    )
  }

  range_vlines <- if (length(vline_vars) > 0) {
    geom_range_vline(
      data = data,
      vars = vline_vars,
      color = vline_vars_colors,
      label = vline_vars_labels,
      linetype = 2
    )
  }

  Filter(
    Negate(is.null),
    list(
      arb_hlines,
      range_hlines,
      arb_vlines,
      range_vlines
    )
  )
}

#' Validate line arguments given in the parameters against the data.
#'
#' helper function to be called by [geom_axes_lines()]
#'
#' @param data (`data.frame`)\cr
#'  should contain `vars` which will be used to create the plot.
#' @param vars (`character`)\cr
#'  names of variables to take the values from. Only first value from the variable will be
#'  applied.
#' @param color (`character`)\cr
#'  colors for the lines defined by variables.
#' @param label (`character`)\cr
#'  labels for the legend to the lines defined by variables.
#'
#' @return (`data.frame`) containing the `values`,`colors` and `labels` fields defining attributes
#'  for horizontal or vertical lines.
#' @keywords internal
#'
validate_line_args <- function(data,
                               vars = character(0),
                               color = "green",
                               label = vars) {
  if (length(vars) > 0) {
    stopifnot(all(vars %in% names(data)))
    checkmate::assert_data_frame(data[vars], types = "numeric")
    checkmate::assert(
      check_color(color, len = 1),
      check_color(color, len = length(vars))
    )
    checkmate::assert_character(label, len = length(vars))


    label <- vapply(
      vars,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (is.null(attributes(data[[x]])$label)) {
          x
        } else {
          attributes(data[[x]])$label
        }
      }
    )

    vars <- sapply(
      vars,
      USE.NAMES = FALSE,
      function(name) {
        x <- data[[name]]
        vals <- unique(x)
        if (!checkmate::test_number(vals)) {
          warning(sprintf("First value is taken from variable '%s' to draw the straight line", name))
        }
        vals[1]
      }
    )
  } else {
    vars <- numeric(0)
    color <- character(0)
    label <- character(0)
  }

  data.frame(
    values = vars,
    colors = color,
    labels = label
  )
}

#' Straight lines for `ggplot2`
#'
#' Arbitrary lines for `ggplot2`
#' @name geom_straight_lines
#' @param vars (`character`)\cr
#'  names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#' @param xintercept (`numeric`)\cr
#'  Position of the vertical line(s) on the x-axis
#' @param yintercept (`numeric`)\cr
#'  Position of the horizontal line(s) on the y-axis
#' @param label (`character`)\cr
#'  Label to be rendered in the legend.
#'  Should be a single string or vector of length equal to length of `xintercept`.
#' @param color (`character`)\cr
#'  Valid color convertible to RGB scale by [grDevices::col2rgb()].
#'  Should be a single string or vector of length equal to length of `xintercept`.
#'
#' @inherit ggplot2::geom_hline return
#'
#' @keywords internal
#'
NULL

#' @rdname geom_straight_lines
#' @examples
#' # horizontal arbitrary lines
#' data <- data.frame(x = seq_len(10), y = seq_len(10), color = rep(c("a", "b"), each = 5))
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = color)) +
#'   ggplot2::geom_point() +
#'   goshawk:::geom_arb_hline(
#'     yintercept = c(2, 5), color = "blue", label = c("h1", "h2"), linetype = 2
#'   )
geom_arb_hline <- function(yintercept,
                           label = "Horizontal line",
                           color = "red",
                           legend_title = "Horizontal line(s)",
                           ...) {
  checkmate::assert_numeric(yintercept, min.len = 1)
  checkmate::assert(
    check_color(color, len = 1),
    check_color(color, len = length(yintercept))
  )
  checkmate::assert(
    checkmate::check_string(label),
    checkmate::check_character(label, len = length(yintercept))
  )
  data <- data.frame(yintercept, color, label, color_var = paste(color, label))

  list(
    ggnewscale::new_scale_color(),
    ggplot2::geom_hline(
      data = data,
      mapping = ggplot2::aes(
        yintercept = yintercept,
        color = .data[["color_var"]], # need legend entry for each color-label combination
      ),
      ...
    ),
    ggplot2::scale_color_manual(
      name = legend_title,
      values = stats::setNames(data$color, data$color_var),
      labels = data$label,
      limits = data$color_var,
      guide = ggplot2::guide_legend(order = 11) # high order to be put after main plot items
    )
  )
}

#' @rdname geom_straight_lines
#' @examples
#' # vertical arbitrary lines
#' data <- data.frame(x = seq_len(10), y = seq_len(10), color = rep(c("a", "b"), each = 5))
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = color)) +
#'   ggplot2::geom_point() +
#'   goshawk:::geom_arb_vline(
#'     xintercept = c(2, 5), color = "blue", label = c("h1", "h2"), linetype = 2
#'   )
geom_arb_vline <- function(xintercept,
                           label = "Vertical line",
                           color = "red",
                           legend_title = "Vertical line(s)",
                           ...) {
  checkmate::assert_numeric(xintercept, min.len = 1)
  checkmate::assert(
    check_color(color, len = 1),
    check_color(color, len = length(xintercept))
  )
  checkmate::assert(
    checkmate::check_character(label, len = 1),
    checkmate::check_character(label, len = length(xintercept))
  )

  data <- data.frame(xintercept, color, label, color_var = paste(color, label))

  list(
    ggnewscale::new_scale_color(),
    ggplot2::geom_vline(
      data = data,
      mapping = ggplot2::aes(
        xintercept = xintercept,
        color = .data[["color_var"]], # need legend entry for each color-label combination
      ),
      ...
    ),
    ggplot2::scale_color_manual(
      name = legend_title,
      values = stats::setNames(data$color, data$color_var),
      labels = data$label,
      limits = data$color_var,
      guide = ggplot2::guide_legend(order = 12) # high order to be put after main plot items
    )
  )
}


#' @rdname geom_straight_lines
#' @examples
#' # horizontal range
#'
#' data <- data.frame(
#'   x = seq_len(10),
#'   y = seq_len(10),
#'   color = rep(c("a", "b"), each = 5),
#'   lower = rep(c(2, 3), each = 5),
#'   upper = rep(c(7, 8), each = 5)
#' )
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = color)) +
#'   ggplot2::geom_point() +
#'   goshawk:::geom_range_hline(
#'     vars = c("lower", "upper"),
#'     data = data.frame(lower = 2, upper = 7),
#'     color = "blue",
#'     linetype = 2
#'   )
geom_range_hline <- function(vars,
                             data,
                             color = "green",
                             label = vars,
                             legend_title = "Horizontal range line(s)",
                             ...) {
  line_data <- validate_line_args(
    data = data,
    vars = vars,
    label = label,
    color = color
  )
  geom_arb_hline(
    yintercept = line_data$values,
    label = line_data$labels,
    color = line_data$colors,
    legend_title = legend_title,
    ...
  )
}

#' @rdname geom_straight_lines
#' @examples
#' # vertical range
#' data <- data.frame(
#'   x = seq_len(10),
#'   y = seq_len(10),
#'   color = rep(c("a", "b"), each = 5),
#'   lower = rep(c(2, 3), each = 5),
#'   upper = rep(c(7, 8), each = 5)
#' )
#' ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = color)) +
#'   ggplot2::geom_point() +
#'   goshawk:::geom_range_vline(
#'     vars = c("lower", "upper"),
#'     data = data.frame(lower = 2, upper = 7),
#'     color = "blue",
#'     linetype = 2
#'   )
geom_range_vline <- function(vars,
                             data,
                             color = "green",
                             label = vars,
                             legend_title = "Vertical range line(s)",
                             ...) {
  line_data <- validate_line_args(
    data = data,
    vars = vars,
    label = label,
    color = color
  )
  geom_arb_vline(
    xintercept = line_data$values,
    label = line_data$labels,
    color = line_data$colors,
    legend_title = legend_title,
    ...
  )
}
