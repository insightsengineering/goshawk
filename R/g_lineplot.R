#' Function to create line plot of summary statistics over time.
#'
#' @param label text string to be displayed as plot label.
#' @param data `ADaM` structured analysis laboratory data frame e.g. `ADLB`.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param loq_flag_var name of variable containing `LOQ` flag e.g. `LOQFL`.
#' @param biomarker biomarker name to be analyzed.
#' @param value_var name of variable containing biomarker results.
#' @param unit_var name of variable containing biomarker result unit.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of `trt_group`.
#' @param shape categorical variable whose levels are used to split the plot lines.
#' @param shape_type vector of symbol types.
#' @param time name of variable containing visit names.
#' @param time_level vector that can be used to define the factor level of time. Only use it when
#' x-axis variable is character or factor.
#' @param color_manual vector of colors.
#' @param line_type vector of line types.
#' @param ylim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the y-axis
#'   if the default limits are not suitable.
#' @param median boolean whether to display median results.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param xtick a vector to define the tick values of time in x-axis.
#' Default value is `ggplot2::waiver()`.
#' @param xlabel vector with same length of `xtick` to define the label of x-axis tick values.
#' Default value is `ggplot2::waiver()`.
#' @param rotate_xlab boolean whether to rotate x-axis labels.
#' @param plot_font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control position dodge.
#' @param plot_height height of produced plot. 989 pixels by default.
#' @param count_threshold \code{integer} minimum number observations needed to show the appropriate
#' bar and point on the plot. Default: 0
#' @param table_font_size \code{float} controls the font size of the values printed in the table.
#' Default: 12
#' @param display_center_tbl boolean whether to include table of means or medians
#'
#'
#' @author Balazs Toth (toth.balazs@gene.com)
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details Currently, the output plot can display mean and median of input value. For mean, the
#' error bar denotes
#' 95\% confidence interval. For median, the error bar denotes median-25% quartile to median+75%
#' quartile.
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#'
#' library(stringr)
#' library(dplyr)
#' library(nestcolor)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", "C: Combination" = "Combination"
#' )
#' color_manual <- c("150mg QD" = "thistle", "Placebo" = "orange", "Combination" = "steelblue")
#' type_manual <- c("150mg QD" = "solid", "Placebo" = "dashed", "Combination" = "dotted")
#'
#' ADSL <- rADSL %>% filter(!(ARM == "B: Placebo" & AGE < 40))
#' ADLB <- rADLB
#' ADLB <- right_join(ADLB, ADSL[, c("STUDYID", "USUBJID")])
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#'
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~
#'       paste(
#'         "W",
#'         trimws(
#'           substr(
#'             AVISIT,
#'             start = 6,
#'             stop = str_locate(AVISIT, "DAY") - 1
#'           )
#'         )
#'       ),
#'     TRUE ~ NA_character_
#'   )) %>%
#'   mutate(AVISITCDN = case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0,
#'     grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#'     TRUE ~ NA_real_
#'   )) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)
#'     )
#'   )) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>%
#'     reorder(TRTORD))
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = ADLB,
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = NULL,
#'   time = "AVISITCDN",
#'   color_manual = color_manual,
#'   line_type = type_manual,
#'   median = FALSE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c(0, 1, 5),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 600
#' )
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = ADLB,
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = NULL,
#'   time = "AVISITCD",
#'   color_manual = NULL,
#'   line_type = type_manual,
#'   median = TRUE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c("BL", "W 1", "W 5"),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 600
#' )
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = ADLB,
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = NULL,
#'   time = "AVISITCD",
#'   color_manual = color_manual,
#'   line_type = type_manual,
#'   median = FALSE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c("BL", "W 1", "W 5"),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 600,
#'   count_threshold = 90,
#'   table_font_size = 15
#' )
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = ADLB,
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = NULL,
#'   time = "AVISITCDN",
#'   color_manual = color_manual,
#'   line_type = type_manual,
#'   median = TRUE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c(0, 1, 5),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 600
#' )
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = subset(ADLB, SEX %in% c("M", "F")),
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = "SEX",
#'   time = "AVISITCDN",
#'   color_manual = color_manual,
#'   line_type = type_manual,
#'   median = FALSE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c(0, 1, 5),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 1500
#' )
#'
#' g_lineplot(
#'   label = "Line Plot",
#'   data = subset(ADLB, SEX %in% c("M", "F")),
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   shape = "SEX",
#'   time = "AVISITCDN",
#'   color_manual = NULL,
#'   median = FALSE,
#'   hline_arb = c(.9, 1.1, 1.2, 1.5),
#'   hline_arb_color = c("green", "red", "blue", "pink"),
#'   hline_arb_label = c("A", "B", "C", "D"),
#'   xtick = c(0, 1, 5),
#'   xlabel = c("Baseline", "Week 1", "Week 5"),
#'   rotate_xlab = FALSE,
#'   plot_height = 1500
#' )
g_lineplot <- function(label = "Line Plot",
                       data,
                       biomarker_var = "PARAMCD",
                       biomarker_var_label = "PARAM",
                       biomarker,
                       value_var = "AVAL",
                       unit_var = "AVALU",
                       loq_flag_var = "LOQFL",
                       ylim = c(NA, NA),
                       trt_group,
                       trt_group_level = NULL,
                       shape = NULL,
                       shape_type = NULL,
                       time,
                       time_level = NULL,
                       color_manual = NULL,
                       line_type = NULL,
                       median = FALSE,
                       hline_arb = numeric(0),
                       hline_arb_color = "red",
                       hline_arb_label = "Horizontal line",
                       xtick = ggplot2::waiver(),
                       xlabel = xtick,
                       rotate_xlab = FALSE,
                       plot_font_size = 12,
                       dodge = 0.4,
                       plot_height = 989,
                       count_threshold = 0,
                       table_font_size = 12,
                       display_center_tbl = TRUE) {
  checkmate::assert_numeric(ylim, len = 2)

  ## Pre-process data
  table_font_size <- grid::convertX(grid::unit(table_font_size, "points"), "mm", valueOnly = TRUE)

  ## - convert to factors
  label_trt_group <- attr(data[[trt_group]], "label")
  data[[trt_group]] <- if (is.null(trt_group_level)) {
    factor(data[[trt_group]])
  } else {
    factor(data[[trt_group]], levels = trt_group_level)
  }
  attr(data[[trt_group]], "label") <- label_trt_group

  color_manual <- if (is.null(color_manual)) {
    temp <- if (!is.null(getOption("ggplot2.discrete.colour"))) {
      getOption("ggplot2.discrete.colour")[1:nlevels(data[[trt_group]])]
    } else {
      gg_color_hue(nlevels(data[[trt_group]]))
    }
    names(temp) <- levels(data[[trt_group]])
    temp
  } else {
    stopifnot(all(levels(data[[trt_group]]) %in% names(color_manual)))
    color_manual
  }

  line_type <- if (is.null(line_type)) {
    stats::setNames(rep("dashed", nlevels(data[[trt_group]])), levels(data[[trt_group]]))
  } else {
    stopifnot(all(levels(data[[trt_group]]) %in% names(line_type)))
    line_type
  }

  shape_type <- if (is.null(shape)) {
    NULL
  } else {
    if (is.null(shape_type)) {
      default_shapes <- c(15:18, 3:14, 0:2)
      res <- if (nlevels(data[[shape]]) > length(default_shapes)) {
        rep(default_shapes, ceiling(nlevels(data[[shape]]) / length(default_shapes)))
      } else {
        default_shapes[seq_len(nlevels(data[[shape]]))]
      }
      stats::setNames(res, levels(data[[shape]]))
    } else {
      stopifnot(all(levels(data[[shape]]) %in% names(shape_type)))
      shape_type
    }
  }

  xtype <- if (is.factor(data[[time]]) || is.character(data[[time]])) {
    "discrete"
  } else {
    "continuous"
  }
  if (xtype == "discrete") {
    data[[time]] <- if (is.null(time_level)) {
      factor(data[[time]])
    } else {
      factor(data[[time]], levels = time_level)
    }
  }

  groupings <- c(time, trt_group, shape)
  ## Summary statistics
  sum_data <- data %>%
    filter(!!sym(biomarker_var) == biomarker) %>%
    group_by_at(groupings) %>%
    summarise(
      count = sum(!is.na(!!sym(value_var))),
      mean = mean(!!sym(value_var), na.rm = TRUE),
      CIup = mean(!!sym(value_var), na.rm = TRUE) + 1.96 * stats::sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
      CIdown = mean(!!sym(value_var), na.rm = TRUE) - 1.96 * stats::sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
      median = stats::median(!!sym(value_var), na.rm = TRUE),
      quant25 = stats::quantile(!!sym(value_var), 0.25, na.rm = TRUE),
      quant75 = stats::quantile(!!sym(value_var), 0.75, na.rm = TRUE)
    ) %>%
    arrange_at(c(trt_group, shape))

  ## Filter out rows with insufficient number of counts
  listin <- list()
  listin[[trt_group]] <- sum_data[[trt_group]]

  if (!is.null(shape)) {
    listin[[shape]] <- sum_data[[shape]]
  }

  int <- unique_name("int", names(sum_data))
  sum_data[[int]] <- new_interaction(listin, sep = " ")
  sum_data[[int]] <- stringr::str_wrap(sum_data[[int]], 12)
  sum_data[[int]] <- factor(sum_data[[int]], sort(unique(sum_data[[int]])))

  unfiltered_data <- sum_data %>% mutate("met_threshold" = count >= count_threshold)
  sum_data <- unfiltered_data %>% filter(.data[["met_threshold"]])

  ## Base plot
  pd <- ggplot2::position_dodge(dodge)
  if (median) {
    line <- "median"
    up_limit <- "quant75"
    down_limit <- "quant25"
  } else {
    line <- "mean"
    up_limit <- "CIup"
    down_limit <- "CIdown"
  }

  filtered_data <- data %>%
    filter(!!sym(biomarker_var) == biomarker)

  unit <- filtered_data %>%
    pull(unit_var) %>%
    unique()

  unit1 <- if (is.na(unit) || unit == "") {
    " "
  } else {
    paste0(" (", unit, ") ")
  }

  biomarker1 <- filtered_data %>%
    pull(biomarker_var_label) %>%
    unique()

  gtitle <- paste0(biomarker1, unit1, stringr::str_to_title(line), " by Treatment @ Visits")
  gylab <- paste0(biomarker1, " ", stringr::str_to_title(line), " of ", value_var, " Values")

  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # Add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- h_caption_loqs_label(loqs_data = filtered_data, flag_var = loq_flag_var)

  if (is.null(shape)) {
    plot1 <- ggplot2::ggplot(
      data = sum_data,
      ggplot2::aes(
        x = .data[[time]],
        y = .data[[line]],
        color = .data[[trt_group]],
        linetype = .data[[trt_group]],
        group = .data[[int]]
      )
    ) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(position = pd) +
      ggplot2::scale_color_manual(
        values = color_manual, name = trt_label, guide = ggplot2::guide_legend(ncol = 3, order = 1)
      ) +
      ggplot2::scale_linetype_manual(
        values = line_type, name = trt_label, guide = ggplot2::guide_legend(ncol = 3, order = 1)
      )
  } else {
    mappings <- sum_data %>%
      ungroup() %>%
      select(!!sym(trt_group), !!sym(shape), int) %>%
      distinct() %>%
      mutate(
        cols = color_manual[as.character(!!sym(trt_group))],
        types = line_type[as.character(!!sym(trt_group))],
        shps = shape_type[!!sym(shape)]
      )

    col_mapping <- stats::setNames(mappings$cols, mappings$int)
    shape_mapping <- stats::setNames(mappings$shps, mappings$int)
    type_mapping <- stats::setNames(mappings$types, mappings$int)

    plot1 <- ggplot2::ggplot(
      data = sum_data,
      ggplot2::aes(
        x = .data[[time]],
        y = .data[[line]],
        color = .data[[int]],
        linetype = .data[[int]],
        group = .data[[int]],
        shape = .data[[int]]
      )
    ) +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(" ", values = col_mapping, guide = ggplot2::guide_legend(ncol = 3, order = 1)) +
      ggplot2::scale_linetype_manual(" ", values = type_mapping, guide = ggplot2::guide_legend(ncol = 3, order = 1)) +
      ggplot2::scale_shape_manual(" ", values = shape_mapping, guide = ggplot2::guide_legend(ncol = 3, order = 1)) +
      ggplot2::theme(legend.key.size = grid::unit(1, "cm")) +
      ggplot2::geom_point(position = pd, size = 3)
  }

  plot1 <- plot1 +
    ggplot2::geom_line(position = pd) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[[down_limit]], ymax = .data[[up_limit]]),
      width = 0.45, position = pd, linetype = "solid"
    ) +
    ggplot2::ggtitle(gtitle) +
    ggplot2::labs(caption = paste(
      "The output plot can display mean and median of input value.",
      "For mean, the error bar denotes 95% confidence interval.",
      "For median, the bar denotes the first to third quartile.\n",
      caption_loqs_label
    )) +
    ggplot2::xlab(time) +
    ggplot2::ylab(gylab) +
    ggplot2::theme(
      legend.box = "vertical",
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.title = ggplot2::element_text(size = plot_font_size, margin = ggplot2::margin(), hjust = 0.5),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20))
    )

  # Apply y-axis zoom range
  plot1 <- plot1 +
    ggplot2::coord_cartesian(ylim = ylim)

  # Format x-label
  if (xtype == "continuous") {
    plot1 <- plot1 +
      ggplot2::scale_x_continuous(breaks = xtick, labels = xlabel, limits = c(NA, NA))
  } else if (xtype == "discrete") {
    plot1 <- plot1 +
      ggplot2::scale_x_discrete(breaks = xtick, labels = xlabel)
  }

  if (rotate_xlab) {
    plot1 <- plot1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }


  plot1 <- plot1 + geom_axes_lines(
    sum_data,
    hline_arb = hline_arb, hline_arb_color = hline_arb_color, hline_arb_label = hline_arb_label
  )

  # Format font size
  if (!is.null(plot_font_size)) {
    plot1 <- plot1 +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = plot_font_size),
        axis.text.x = ggplot2::element_text(size = plot_font_size),
        axis.title.y = ggplot2::element_text(size = plot_font_size),
        axis.text.y = ggplot2::element_text(size = plot_font_size),
        legend.title = ggplot2::element_text(size = plot_font_size),
        legend.text = ggplot2::element_text(size = plot_font_size)
      )
  }

  labels <- levels(unfiltered_data[[int]])
  lines <- sum(stringr::str_count(unique(labels), "\n")) / 2 + length(unique(labels))
  minline <- 36
  tabletotal <- lines * minline * ifelse(display_center_tbl, 2, 1)
  plotsize <- plot_height - tabletotal
  if (plotsize <= 250) {
    stop("Due to number of line splitting levels the current plot height is not sufficient to display plot.
    If applicable, please try a combination of:
      * increasing the plot height using the Plot Aesthetic Settings,
      * increasing the relative height of plot to table(s),
      * increasing the initial maximum plot_height argument during creation of this app,
      * and / or consider removing the mean / median table.")
  }

  if (display_center_tbl) {
    unfiltered_data$center <- if (median) {
      sprintf(ifelse(unfiltered_data$count > 0, "%.2f", ""), unfiltered_data$median)
    } else {
      sprintf(ifelse(unfiltered_data$count > 0, "%.2f", ""), unfiltered_data$mean)
    }
    tbl_central_value_title <- if (median) "Median" else "Mean"
    tbl_central_value <- ggplot2::ggplot(
      unfiltered_data,
      ggplot2::aes(x = .data[[time]], y = .data[[int]], label = .data[["center"]])
    ) +
      ggplot2::geom_text(ggplot2::aes(color = .data[["met_threshold"]]), size = table_font_size) +
      ggplot2::ggtitle(tbl_central_value_title) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_discrete(labels = labels) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        legend.position = "none",
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = plot_font_size),
        plot.title = ggplot2::element_text(face = "bold", size = plot_font_size)
      ) +
      ggplot2::scale_color_manual(values = c("FALSE" = "red", "TRUE" = "black"))
  }

  tbl <- ggplot2::ggplot(
    unfiltered_data,
    ggplot2::aes(x = .data[[time]], y = .data[[int]], label = .data[["count"]])
  ) +
    ggplot2::geom_text(ggplot2::aes(color = .data[["met_threshold"]]), size = table_font_size) +
    ggplot2::ggtitle("Number of observations") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_discrete(labels = labels) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = plot_font_size),
      plot.title = ggplot2::element_text(face = "bold", size = plot_font_size)
    ) +
    ggplot2::scale_color_manual(values = c("FALSE" = "red", "TRUE" = "black"))

  # Plot the grobs using plot_grid
  if (display_center_tbl) {
    cowplot::plot_grid(plot1, tbl_central_value, tbl,
      align = "v", ncol = 1,
      rel_heights = c(plotsize, tabletotal / 2, tabletotal / 2)
    )
  } else {
    cowplot::plot_grid(plot1, tbl, align = "v", ncol = 1, rel_heights = c(plotsize, tabletotal))
  }
}

new_interaction <- function(args, drop = FALSE, sep = ".", lex.order = FALSE) { # nolint
  for (i in seq_along(args)) {
    if (is.null(args[[i]])) {
      args[[i]] <- NULL
    }
  }
  if (length(args) == 1) {
    return(paste0(names(args), ":", args[[1]]))
  }
  args <- mapply(function(n, val) paste0(n, ":", val), names(args), args, SIMPLIFY = FALSE)
  interaction(args, drop = drop, sep = sep, lex.order = lex.order)
}

unique_name <- function(newname, old_names) {
  if (newname %in% old_names) {
    unique_name(paste0(newname, "1"), old_names)
  }
  newname
}

gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
