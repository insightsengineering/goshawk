#' Helper for identifying any LLOQ and ULOQ values in LBSTRESC. Outcome drives
#' horizontal line functionality display and legend labeling along with display
#' of values in footnote.
#'
#' @details Biomarker Sciences would like to have LLOQ and ULOQ values available for
#' reference in the visualizations. This also aids in setting the data constraint
#' ranges when goshawk functions are run from teal.goshawk UI.
#'
#' @param loqs_data (`data frame`)\cr loqs_data data set containing assay data with potential LOQ values
#'
#' @import dplyr
#'
#' @examples
#' library(scda)
#'
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' goshawk:::h_identify_loq_values(loqs_data = ADLB)
h_identify_loq_values <- function(loqs_data) {
  ifelse(
    !grep("PARAM", names(loqs_data)),
    stop("Assay dataset must include variable PARAM to use the caption_loqs_label function."),
    1
  )
  ifelse(
    !grep("LBSTRESC", names(loqs_data)),
    stop("Assay dataset must include variable LBSTRESC to use the caption_loqs_label function."),
    1
  )

  # get LLOQ value
  lloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl("<", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(LLOQC = .data$LBSTRESC, LLOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.data$LBSTRESC)

  # get ULOQ value
  uloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(ULOQC = .data$LBSTRESC, ULOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.data$LBSTRESC)

  # return LOQ data
  loq_values <- merge(lloq, uloq, by = "PARAM", all = TRUE)
  if (nrow(loq_values) == 0) {
    loq_values <- data.frame(
      PARAM = names(table(droplevels(as.factor(loqs_data$PARAM)))),
      LLOQC = NA,
      LLOQN = NA,
      ULOQC = NA,
      ULOQN = NA
    )
  }

  attr(loq_values[["PARAM"]], "label") <- "Parameter"
  attr(loq_values[["LLOQC"]], "label") <- "Lower Limit of Quantitation (C)"
  attr(loq_values[["LLOQN"]], "label") <- "Lower Limit of Quantitation"
  attr(loq_values[["ULOQC"]], "label") <- "Upper Limit of Quantitation (C)"
  attr(loq_values[["ULOQN"]], "label") <- "Upper Limit of Quantitation"

  return(loq_values)
}

#' Add footnote to identify LLOQ and ULOQ values identified from data
#'
#' @param loqs_data (`data frame`)\cr loqs_data data set containing assay data with potential LOQ values
#'
#' @import dplyr
#'
#' @examples
#' library(scda)
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' caption_label <- goshawk:::h_caption_loqs_label(loqs_data = ADLB)
h_caption_loqs_label <- function(loqs_data) {
  loq_values <- h_identify_loq_values(loqs_data)

  if (is.na(loq_values$LLOQC)) {
    lloq_value <- "NA"
  } else {
    lloq_value <- loq_values$LLOQC
  }

  if (is.na(loq_values$ULOQC)) {
    uloq_value <- "NA"
  } else {
    uloq_value <- loq_values$ULOQC
  }

  # create caption
  caption_loqs_label <- paste0(
    "Limits of quantification read from study data for ",
    loqs_data$PARAM,
    ": LLOQ is ",
    lloq_value,
    ", ULOQ is ",
    uloq_value
  )

  return(caption_loqs_label)
}

#' validate line arguments given in the parameters against the data.
#'
#' helper function to be called by add_straight_lines
#'
#' @param data ('data.frame') with variables which will be used to create the plot.
#' @param line_arb ('numeric vector') value identifying intercept for arbitrary lines.
#' @param line_arb_color ('character vector') optional, color for the arbitrary lines.
#' @param line_arb_label ('character vector') optional, label for the legend to the arbitrary lines.
#' @param line_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data must also contain the columns with these variable names
#' @param line_vars_colors ('character vector') colors for the lines defined by variables.
#' @param line_vars_labels ('character vector') labels for the legend to the lines defined by variables.
#'
#' @return ('list') containing the `values`,`colors` and `labels` fields defining attributes
#' for horizontal or vertical lines.
validate_line_args <- function(data,
                               line_arb = numeric(0),
                               line_arb_color = "red",
                               line_arb_label = "Arbitrary line",
                               line_vars = character(0),
                               line_vars_colors = "green",
                               line_vars_labels = line_vars) {
  if (length(line_arb) > 0) {
    checkmate::assert_numeric(line_arb)
    stopifnot(length(line_arb_color) == 1 || length(line_arb_color) == length(line_arb))
    stopifnot(length(line_arb_label) == 1 || length(line_arb_label) == length(line_arb))

    if (length(line_arb_color) == 1) {
      line_arb_color <- rep(line_arb_color, length(line_arb))
    }

    if (length(line_arb_label) == 1) {
      line_arb_label <- rep(line_arb_label, length(line_arb))
    }
  } else {
    line_arb <- numeric(0)
    line_arb_color <- character(0)
    line_arb_label <- character(0)
  }

  if (length(line_vars) > 0) {
    stopifnot(all(line_vars %in% names(data)))
    checkmate::check_list(data[line_vars], types = "numeric")
    stopifnot(length(line_vars_labels) == length(line_vars))
    if (length(line_vars_colors) == 1) {
      line_vars_colors <- rep(line_vars_colors, length(line_vars))
    } else {
      stopifnot(length(line_vars_colors) == length(line_vars))
    }

    line_vars_labels <- vapply(
      line_vars,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) if (is.null(attributes(data[[x]])$label)) x else attributes(data[[x]])$label
    )

    line_vars <- sapply(
      line_vars,
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
    line_vars <- numeric(0)
    line_vars_colors <- character(0)
    line_vars_labels <- character(0)
  }

  list(
    values = c(line_arb, line_vars),
    colors = c(line_arb_color, line_vars_colors),
    labels = c(line_arb_label, line_vars_labels)
  )
}

#' Add horizontal and/or vertical lines and their legend labels to a plot
#'
#' This function is currently designed to be used with ('g_boxplot'), ('g_correlationplot'), ('g_spaghettiplot'),
#' and ('g_density_distribution_plot'), but may also work in general.
#'
#' @param plot ('ggplot2 object') which the horizontal and/or vertical lines should be added to
#' @param agg_label ('character') label for the line denoting the Mean or Median.
#' @param color_comb ('character') denoting the color of the Mean or Median line.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param hline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the ggplot2 object must also contain the columns with these variable names
#' @param hline_vars_colors ('character vector') colors for the horizontal lines defined by variables.
#' @param hline_vars_labels ('character vector') labels for the legend to the horizontal lines defined by variables.
#' @param vline_arb ('numeric vector') value identifying intercept for arbitrary vertical lines.
#' @param vline_arb_color ('character vector') optional, color for the arbitrary vertical lines.
#' @param vline_arb_label ('character vector') optional, label for the legend to the arbitrary vertical lines.
#' @param vline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the ggplot2 object must also contain the columns with these variable names
#' @param vline_vars_colors ('character vector') colors for the vertical lines defined by variables.
#' @param vline_vars_labels ('character vector') labels for the legend to the vertical lines defined by variables.
#'
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#' p %>% goshawk:::add_axes_lines(
#'   hline_arb = c(20, 25, 30),
#'   hline_arb_color = "red",
#'   hline_arb_label = "Hori Line"
#' )
#' @return \code{ggplot} object
#'
add_axes_lines <- function(plot,
                           agg_label = NULL,
                           color_comb = NULL,
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
  plot_data <- ggplot_build(plot)$plot$data
  draw_key_cust <- function(data, params, size) {
    if (data$orientation == "horizontal") {
      draw_key_path(data, params, size)
    } else {
      draw_key_vpath(data, params, size)
    }
  }

  # handling horizontal lines-------------------------------------------------------
  validated_res <- validate_line_args(
    data = plot_data,
    line_arb = hline_arb,
    line_arb_color = hline_arb_color,
    line_arb_label = hline_arb_label,
    line_vars = hline_vars,
    line_vars_colors = hline_vars_colors,
    line_vars_labels = hline_vars_labels
  )

  for (i in seq_along(validated_res$values)) {
    plot <- plot +
      geom_hline(
        aes_(yintercept = validated_res$values[i], linetype = as.factor(paste0("dashed_", i))),
        size = 0.5,
        color = validated_res$colors[i],
        key_glyph = draw_key_cust
      )
  }

  # handling vertical lines--------------------------------------------------------
  validated_res_vert <- validate_line_args(
    data = plot_data,
    line_arb = vline_arb,
    line_arb_color = vline_arb_color,
    line_arb_label = vline_arb_label,
    line_vars = vline_vars,
    line_vars_colors = vline_vars_colors,
    line_vars_labels = vline_vars_labels
  )


  for (i in seq_along(validated_res_vert$values)) {
    plot <- plot +
      geom_vline(
        aes_(xintercept = validated_res_vert$values[i], linetype = as.factor(paste0("vert_dashed_", i))),
        size = 0.5,
        color = validated_res_vert$colors[i],
        key_glyph = draw_key_cust
      )
  }

  # plotting ---------------------------------------------------------------------------
  if (length(validated_res$values) > 0 || length(validated_res_vert$values) > 0) {
    plot +
      scale_linetype_manual(
        name = paste0(
          `if`(length(validated_res$values) > 0, "Horizontal ", NULL),
          `if`(length(validated_res$values) > 0 && length(validated_res_vert$values) > 0, "and ", NULL),
          `if`(length(validated_res_vert$values) > 0, "Vertical ", NULL),
          "Line(s)"
        ),
        label = c(
          validated_res$labels,
          agg_label,
          validated_res_vert$labels
        ),
        values = c(
          rep(2, length(validated_res$values)),
          if (is.null(agg_label)) agg_label else 1,
          rep(2, length(validated_res_vert$values))
        )
      ) +
      guides(linetype = guide_legend(
        override.aes =
          list(
            color = c(
              validated_res$colors,
              if (is.null(agg_label)) agg_label else color_comb,
              validated_res_vert$colors
            ),
            orientation = c(
              rep("horizontal", length(c(validated_res$values, agg_label))),
              rep("vertical", length(validated_res_vert$values))
            )
          )
      )) +
      theme(legend.key.size = unit(0.5, "in"))
  } else {
    plot
  }
}
