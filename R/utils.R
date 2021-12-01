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
#'
h_identify_loq_values <- function(loqs_data) {
  ifelse(
    !grep("PARAM", names(loqs_data)),
    stop("Assay dataset must include variable PARAM to use the caption_loqs_label function."),
    1)
  ifelse(
    !grep("LBSTRESC", names(loqs_data)),
    stop("Assay dataset must include variable LBSTRESC to use the caption_loqs_label function."),
    1)

  # get LLOQ value
  lloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl("<", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(LLOQC = .data$LBSTRESC, LLOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
    select(-.data$LBSTRESC)

  # get ULOQ value
  uloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(ULOQC = .data$LBSTRESC, ULOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
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
#'
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
    uloq_value)

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
#' @return ('list')
validate_line_args <- function(data,
                               line_arb = NULL,
                               line_arb_color = "red",
                               line_arb_label = NULL,
                               line_vars = NULL,
                               line_vars_colors = NULL,
                               line_vars_labels = NULL
                               ) {

  new_line_col <- if (!is.null(line_arb)) {
    stopifnot(is_numeric_vector(line_arb))
    if (is.null(line_arb_color)) {
      line_arb_color <- rep("red", length(line_arb))
    } else {
      if (is_character_single(line_arb_color)) {
        line_arb_color <- rep(line_arb_color, length(line_arb))
      } else {
        stopifnot(is_character_vector(line_arb_color, min_length = length(line_arb), max_length = length(line_arb)))
      }
    }
    if (is.null(line_arb_label)) {
      line_arb_label <- rep("Arbitrary Vertical Line", length(line_arb))
    } else {
      if (is_character_single(line_arb_label)) {
        line_arb_label <- rep(line_arb_label, length(line_arb))
      } else {
        stopifnot(is_character_vector(line_arb_label, min_length = length(line_arb), max_length = length(line_arb)))
      }
    }

    new_line_col <- paste0("Arbitrary_Vertical_Line_", seq_len(length(line_arb)))
    for (index in seq_len(length(line_arb))) {
      i <- 1
      while (new_line_col[index] %in% names(data)) {
        new_line_col[index] <- paste0(new_line_col[index], "_", i)
        i <- i + 1
      }
    }
    new_line_col
  }

  line_vars_labels <- if (!is.null(line_vars)) {
    stopifnot(is_character_vector(line_vars, min_length = 1, max_length = length(data)))
    stopifnot(all(line_vars %in% names(data)))
    stopifnot(
      all(vapply(
        line_vars,
        FUN = function(x) is.numeric(data[[x]]) == 1,
        FUN.VALUE = logical(1)
      ))
    )

    if (!is.null(line_vars_labels)) {
      stopifnot(is_character_vector(
        line_vars_labels, min_length = length(line_vars),
        max_length = (length(line_vars)))
      )
    } else {
      line_vars_labels <- vapply(
        line_vars,
        FUN = function(x) if_null(attributes(data[[x]])$label, ""),
        FUN.VALUE = character(1)
      )
      line_vars_labels <- vapply(
        seq_along(line_vars_labels),
        FUN = function(x) `if`(line_vars_labels[x] == "", line_vars[x], line_vars_labels[x]),
        FUN.VALUE = character(1)
      )
    }
    if (!is.null(line_vars_colors)) {
      stopifnot(is_character_vector(
        line_vars_colors,
        min_length = length(line_vars),
        max_length = (length(line_vars)))
      )
    }
    line_vars_labels
  }

  return(
    list(
      new_line_col = new_line_col,
      line_vars_labels = line_vars_labels,
      line_arb_color = line_arb_color,
      line_arb_label = line_arb_label
    )
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
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p %>% goshawk:::add_straight_lines(
#'   hline_arb = c(20, 25, 30),
#'   hline_arb_color = "red",
#'   hline_arb_label = "Hori Line"
#' )
#'
#' @return \code{ggplot} object
#'
add_straight_lines <- function(plot,
                               agg_label = NULL,
                               color_comb = NULL,
                               hline_arb = NULL,
                               hline_arb_color = "red",
                               hline_arb_label = NULL,
                               hline_vars = NULL,
                               hline_vars_colors = NULL,
                               hline_vars_labels = NULL,
                               vline_arb = NULL,
                               vline_arb_color = "green",
                               vline_arb_label = NULL,
                               vline_vars = NULL,
                               vline_vars_colors = NULL,
                               vline_vars_labels = NULL
                               ) {

  plot_data <- ggplot_build(plot)$plot$data

  validated_res <- validate_line_args(
    data = plot_data,
    line_arb = hline_arb, line_arb_color = hline_arb_color, line_arb_label = hline_arb_label,
    line_vars = hline_vars, line_vars_colors = hline_vars_colors, line_vars_labels = hline_vars_labels
  )

  new_hline_col <- validated_res$new_line_col
  hline_vars_labels <- validated_res$line_vars_labels
  hline_arb_color <- validated_res$line_arb_color
  hline_arb_label <- validated_res$line_arb_label

  validated_res_vert <- validate_line_args(
    data = plot_data,
    line_arb = vline_arb, line_arb_color = vline_arb_color, line_arb_label = vline_arb_label,
    line_vars = vline_vars, line_vars_colors = vline_vars_colors, line_vars_labels = vline_vars_labels
  )
  new_vline_col <- validated_res_vert$new_line_col
  vline_vars_labels <- validated_res_vert$line_vars_labels
  vline_arb_color <- validated_res_vert$line_arb_color
  vline_arb_label <- validated_res_vert$line_arb_label

  draw_key_cust <- function(data, params, size) {
    if (data$orientation == "horizontal") {
      draw_key_path(data, params, size)
    } else {
      draw_key_vpath(data, params, size)
    }
  }

  # handling horizontal lines-------------------------------------------------------
  range_color <- c(
    if_null(hline_vars_colors, if_not_null(hline_vars, seq(length(hline_vars)))),
    if_not_null(hline_arb, hline_arb_color)
  )
  if (!is.null(hline_arb)) {
    hline_vars <- c(hline_vars, new_hline_col)
    for (index in seq_len(length(hline_arb))) {
      plot_data[new_hline_col[index]] <- hline_arb[index]
    }
  }

  j <- 1
  for (i in hline_vars) {
    plot <- plot +
      geom_hline(
        data = plot_data,
        aes_(yintercept = plot_data[[i]][1], linetype = as.factor(paste0("dashed_", i))),
        size = 0.5,
        color = range_color[j],
        key_glyph = draw_key_cust
      )
    j <- j + 1
  }
  #--------------------------------------------------------------------------------

  # handling vertical lines--------------------------------------------------------
  range_color_vert <- c(
    if_null(vline_vars_colors, if_not_null(vline_vars, seq(length(vline_vars)))),
    if_not_null(vline_arb, vline_arb_color)
  )
  if (!is.null(vline_arb)) {
    vline_vars <- c(vline_vars, new_vline_col)
    for (index in seq_len(length(vline_arb))) {
      plot_data[new_vline_col[index]] <- vline_arb[index]
    }
  }
  j <- 1
  for (i in vline_vars) {
    plot <- plot +
      geom_vline(
        data = plot_data,
        aes_(xintercept = plot_data[[i]][1], linetype = as.factor(paste0("vert_dashed_", i))),
        size = 0.5,
        color = range_color_vert[j],
        key_glyph = draw_key_cust
      )
    j <- j + 1
  }
  #------------------------------------------------------------------------------
  if (length(hline_vars) > 0 || length(vline_vars) > 0) {
    plot +
      scale_linetype_manual(
        name = paste0("Description of Horizontal ", if_not_null(vline_vars, "and Vertical "),  "Line(s)"),
        label = c(
          if_null(c(hline_vars_labels, hline_arb_label), hline_vars),
          agg_label,
          if_null(c(vline_vars_labels, vline_arb_label), vline_vars)
        ),
        values = c(rep(2, length(hline_vars)), if_not_null(agg_label, 1), rep(2, length(vline_vars)))
      ) +
      guides(linetype = guide_legend(override.aes =
        list(
          color = c(range_color, if_not_null(agg_label, color_comb), range_color_vert),
          orientation = c(rep("horizontal", length(c(hline_vars, agg_label))), rep("vertical", length(vline_vars))))
      )
      ) +
      theme(legend.key.size = unit(0.5, "in"))
  } else {
    plot
  }
}
