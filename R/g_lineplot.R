#' Function to create line plot of summary statistics over time.
#'
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables to be summarized and generate statistics which will display
#'  in the plot.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param biomarker biomarker name to be analyzed.
#' @param value_var name of variable containing biomarker results.
#' @param unit_var name of variable containing biomarker result unit.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param shape categorical variable whose levels are used to split the plot lines.
#' @param shape_type vector of symbol types.
#' @param time name of variable containing visit names.
#' @param time_level vector that can be used to define the factor level of time. Only use it when
#' x-axis variable is character or factor.
#' @param color_manual vector of colors.
#' @param line_type vector of line types.
#' @param ylim numeric vector to define y-axis range.
#' @param median boolean whether to display median results.
#' @param hline numeric value representing intercept of horizontal line.
#' @param xtick a vector to define the tick values of time in x-axis.
#' Default value is waiver().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values.
#' Default value is waiver().
#' @param rotate_xlab boolean whether to rotate x-axis labels.
#' @param plot_font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control position dodge.
#' @param plot_height height of produced plot. 989 pixels by default.
#' @param count_threshold \code{integer} minimum number observations needed to show the appropriate
#' bar and point on the plot. Default: 0
#' @param table_font_size \code{float} controls the font size of the values printed in the table.
#' Default: 12
#'
#' @importFrom cowplot plot_grid
#' @importFrom grDevices hcl
#' @importFrom stringr str_count str_wrap str_to_title
#' @importFrom rlang .data
#' @importFrom grid unit convertX
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
#'
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(random.cdisc.data)
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo",
#' "C: Combination" = "Combination")
#' color_manual <- c("150mg QD" = "thistle", "Placebo" = "orange", "Combination" = "steelblue")
#' type_manual <- c("150mg QD" = "solid", "Placebo" = "dashed", "Combination" = "dotted")
#'
#' ASL <- cadsl[!(cadsl$ARM == "B: Placebo" & cadsl$AGE < 40), ]
#' ALB <- right_join(cadlb, ASL[, c("STUDYID", "USUBJID")])
#' var_labels <- lapply(ALB, function(x) attributes(x)$label)
#'
#' ALB <- ALB %>%
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
#'     TRUE ~ NA_character_)) %>%
#'   mutate(AVISITCDN = case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0,
#'     grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#'     TRUE ~ NA_real_)) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>%
#'   reorder(TRTORD))
#' attr(ALB[["ARM"]], "label") <- var_labels[["ARM"]]
#'
#' g_lineplot(label = "Line Plot",
#'            data = ALB,
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = NULL,
#'            time = "AVISITCD",
#'            color_manual = color_manual,
#'            line_type = type_manual,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c("BL", "W 1", "W 5"),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600)
#'
#' g_lineplot(label = "Line Plot",
#'            data = ALB,
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = NULL,
#'            time = "AVISITCD",
#'            color_manual = NULL,
#'            line_type = type_manual,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c("BL", "W 1", "W 5"),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600)
#'
#' g_lineplot(label = "Line Plot",
#'            data = ALB,
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = NULL,
#'            time = "AVISITCD",
#'            color_manual = color_manual,
#'            line_type = type_manual,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c("BL", "W 1", "W 5"),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600,
#'            count_threshold = 90,
#'            table_font_size = 2)
#'
#' g_lineplot(label = "Line Plot",
#'            data = ALB,
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = NULL,
#'            time = "AVISITCDN",
#'            color_manual = color_manual,
#'            line_type = type_manual,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c(0, 1, 5),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600)
#'
#' g_lineplot(label = "Line Plot",
#'            data = subset(ALB, SEX %in% c("M", "F")),
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = "SEX",
#'            time = "AVISITCDN",
#'            color_manual = color_manual,
#'            line_type = type_manual,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c(0, 1, 5),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600)
#'
#' g_lineplot(label = "Line Plot",
#'            data = subset(ALB, SEX %in% c("M", "F")),
#'            biomarker_var = "PARAMCD",
#'            biomarker = "CRP",
#'            value_var = "AVAL",
#'            trt_group = "ARM",
#'            shape = "SEX",
#'            time = "AVISITCDN",
#'            color_manual = NULL,
#'            median = FALSE,
#'            hline = 50,
#'            xtick = c(0, 1, 5),
#'            xlabel = c("Baseline", "Week 1", "Week 5"),
#'            rotate_xlab = FALSE,
#'            plot_height = 600)
#'
g_lineplot <- function(label = "Line Plot",
                       data,
                       biomarker_var = "PARAMCD",
                       biomarker_var_label = "PARAM",
                       biomarker,
                       value_var = "AVAL",
                       unit_var = "AVALU",
                       ylim = NULL,
                       trt_group,
                       trt_group_level = NULL,
                       shape = NULL,
                       shape_type = NULL,
                       time,
                       time_level = NULL,
                       color_manual = NULL,
                       line_type = NULL,
                       median = FALSE,
                       hline = NULL,
                       xtick = waiver(),
                       xlabel = xtick,
                       rotate_xlab = FALSE,
                       plot_font_size = 12,
                       dodge = 0.4,
                       plot_height = 989,
                       count_threshold = 0,
                       table_font_size = 12) {

  ## Pre-process data
  table_font_size <- convertX(unit(table_font_size, "points"), "mm", valueOnly = TRUE)

  ## - convert to factors
  label_trt_group <- attr(data[[trt_group]], "label")
  data[[trt_group]] <- if (is.null(trt_group_level)) {
    factor(data[[trt_group]])
  } else {
    factor(data[[trt_group]], levels = trt_group_level)
  }
  attr(data[[trt_group]], "label") <- label_trt_group

  color_manual <- if (is.null(color_manual)) {
    gg_color_hue(nlevels(data[[trt_group]]))
  } else {
    stopifnot(all(levels(data[[trt_group]]) %in% names(color_manual)))
    color_manual
  }

  line_type <- if (is.null(line_type)) {
    setNames(rep("dashed", nlevels(data[[trt_group]])), levels(data[[trt_group]]))
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
      setNames(res, levels(data[[shape]]))
    } else {
      stopifnot(all(shape_type %in% c(0:18)))
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
      CIup = mean(!!sym(value_var), na.rm = TRUE) + 1.96 * sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
      CIdown = mean(!!sym(value_var), na.rm = TRUE) - 1.96 * sd(!!sym(value_var), na.rm = TRUE) / sqrt(n()),
      median = median(!!sym(value_var), na.rm = TRUE),
      quant25 = quantile(!!sym(value_var), 0.25, na.rm = TRUE),
      quant75 = quantile(!!sym(value_var), 0.75, na.rm = TRUE)) %>%
    arrange_at(c(trt_group, shape))

  ## Filter out rows with insufficient number of counts
  listin <- list()
  listin[[trt_group]] <- sum_data[[trt_group]]

  if (!is.null(shape)) {
    listin[[shape]] <- sum_data[[shape]]
  }

  int <- unique_name("int", names(sum_data))
  sum_data[[int]] <- new_interaction(listin, sep = " ")
  sum_data[[int]] <- str_wrap(sum_data[[int]], 12)
  sum_data[[int]] <- factor(sum_data[[int]], sort(unique(sum_data[[int]])))

  unfiltered_data <- sum_data %>% mutate("met_threshold" = count >= count_threshold)
  sum_data <- unfiltered_data %>% filter(.data[["met_threshold"]])

  ## Base plot
  pd <- position_dodge(dodge)
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

  gtitle <- paste0(biomarker1, unit1, str_to_title(line), " by Treatment @ Visits")
  gylab <- paste0(biomarker1, " ", str_to_title(line), " of ", value_var, " Values")

  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # Add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- caption_loqs_label(loqs_data = filtered_data)

  if (is.null(shape)) {
    plot1 <- ggplot(
      data = sum_data,
      aes_string(x = time, y = line, color = trt_group, linetype = trt_group, group = int)
    ) +
      theme_bw() +
      geom_point(position = pd) +
      scale_color_manual(values = color_manual, name = trt_label) +
      scale_linetype_manual(values = line_type, name = trt_label)
  } else {
    mappings <- sum_data %>%
      ungroup() %>%
      select(!!sym(trt_group), !!sym(shape), int) %>%
      distinct() %>%
      mutate(
        cols = color_manual[!!sym(trt_group)],
        types = line_type[!!sym(trt_group)],
        shps = shape_type[!!sym(shape)]
      )

    col_mapping <- setNames(mappings$cols, mappings$int)
    shape_mapping <- setNames(mappings$shps, mappings$int)
    type_mapping <- setNames(mappings$types, mappings$int)

    plot1 <-  ggplot(
      data = sum_data,
      aes_string(x = time, y = line, color = int, linetype = int, group = int, shape = int)
    ) +
      theme_bw() +
      scale_color_manual(" ", values = col_mapping) +
      scale_linetype_manual(" ", values = type_mapping) +
      scale_shape_manual(" ", values = shape_mapping) +
      theme(legend.key.size = unit(1, "cm")) +
      geom_point(position = pd, size = 3)

  }

  plot1 <-  plot1 +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit, ymax = up_limit), width = 0.45, position = pd, linetype = "solid") +
    ggtitle(gtitle) +
    labs(caption = paste(
      "The output plot can display mean and median of input value.",
      "For mean, the error bar denotes 95% confidence interval.",
      "For median, the bar denotes the first to third quartile.\n",
      caption_loqs_label)) +
    xlab(time) +
    ylab(gylab) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      plot.title = element_text(size = plot_font_size, margin = margin(), hjust = 0.5),
      axis.title.y = element_text(margin = margin(r = 20))) +
    guides(color = guide_legend(ncol = 3, byrow = TRUE))

  # Apply y-axis zoom range
  if (!is.null(ylim)) {
    plot1 <- plot1 +
      coord_cartesian(ylim = ylim)
  }

  # Format x-label
  if (xtype == "continuous") {
    plot1 <- plot1 +
      scale_x_continuous(breaks = xtick, labels = xlabel, limits = c(NA, NA))
  } else if (xtype == "discrete") {
    plot1 <- plot1 +
      scale_x_discrete(breaks = xtick, labels = xlabel)
  }

  if (rotate_xlab) {
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  #Add horizontal line
  if (!is.null(hline)) {
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color = "red", linetype = "dashed", size = 0.5)
  }

  # Format font size
  if (!is.null(plot_font_size)) {
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = plot_font_size),
            axis.text.x = element_text(size = plot_font_size),
            axis.title.y = element_text(size = plot_font_size),
            axis.text.y = element_text(size = plot_font_size),
            legend.title = element_text(size = plot_font_size),
            legend.text = element_text(size = plot_font_size))
  }

  labels <- levels(unfiltered_data[[int]])
  lines <- sum(stringr::str_count(unique(labels), "\n")) / 2 + length(unique(labels))
  minline <- 36
  tabletotal <- lines * minline
  plotsize <- plot_height - tabletotal
  if (plotsize <= 250) {
    stop("Due to number of line splitting levels default plot height is not sufficient to display. Please adjust the
    plot height using the Plot Aesthetic Settings.")
  }

  tbl <- ggplot(unfiltered_data, aes_string(x = time, y = int, label = "count")) +
    geom_text(aes(color = .data[["met_threshold"]]), size = table_font_size) +
    ggtitle("Number of observations") +
    theme_minimal() +
    scale_y_discrete(labels = labels) +
    theme(
      panel.grid.major = element_blank(),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = plot_font_size),
      plot.title = element_text(face = "bold", size = plot_font_size)
    ) +
    scale_color_manual(values = c("FALSE" = "red", "TRUE" = "black"))

  #Plot the two grobs using plot_grid
  plot_grid(plot1, tbl, align = "v", ncol = 1, rel_heights = c(plotsize, tabletotal))
}

new_interaction <- function(args, drop = FALSE, sep = ".", lex.order = FALSE) { #nolint
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
  hcl(h = hues, l = 65, c = 100)[1:n]
}
