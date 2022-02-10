#' Function to create a spaghetti plot.
#'
#' This function is rendered by teal.goshawk module
#'
#' @param data data frame with variables to be summarized and generate statistics which will display
#'  in the plot.
#' @param subj_id unique subject id variable name.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param biomarker biomarker name to be analyzed.
#' @param value_var name of variable containing biomarker results.
#' @param unit_var name of variable containing biomarker units.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param time name of variable containing visit names.
#' @param time_level vector that can be used to define the factor level of time. Only use it when
#' x-axis variable is character or factor.
#' @param color_manual vector of colors.
#' @param color_comb name or hex value for combined treatment color.
#' @param ylim numeric vector to define y-axis range.
#' @param alpha subject line transparency (0 = transparent, 1 = opaque)
#' @param facet_ncol number of facets per row.
#' @param xtick a vector to define the tick values of time in x-axis.
#' Default value is waiver().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values. Default
#'  value is waiver().
#' @param rotate_xlab boolean whether to rotate x-axis labels.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param group_stats control group mean or median overlay.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param hline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the ggplot2 object must also contain the columns with these variable names
#' @param hline_vars_colors ('character vector') colors for the horizontal lines defined by variables.
#' @param hline_vars_labels ('character vector') labels for the legend to the horizontal lines defined by variables.
#'
#'
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @importFrom magrittr extract2
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(scda)
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", "C: Combination" = "Combination")
#' color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#'
#' ASL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~
#'     paste(
#'       "W",
#'       trimws(
#'         substr(
#'           AVISIT,
#'           start = 6,
#'           stop = str_locate(AVISIT, "DAY") - 1
#'         )
#'       )
#'     ),
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
#'     reorder(TRTORD)) %>%
#'   mutate(ANRLO = 30, ANRHI = 75) %>%
#'   rowwise() %>%
#'   group_by(PARAMCD) %>%
#'   mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'   )) %>%
#'   mutate(LBSTRESC = ifelse(USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'   )) %>%
#'   ungroup()
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#' attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#' # add LLOQ and ULOQ variables
#' ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#' ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
#'
#' g_spaghettiplot(
#'   data = ADLB,
#'   subj_id = "USUBJID",
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   time = "AVISITCD",
#'   color_manual = color_manual,
#'   color_comb = "#39ff14",
#'   alpha = .02,
#'   xtick = c("BL", "W 1", "W 4"),
#'   xlabel = c("Baseline", "Week 1", "Week 4"),
#'   rotate_xlab = FALSE,
#'   group_stats = "median",
#'   hline_vars = c("ANRHI", "ANRLO"),
#'   hline_vars_colors = c("pink", "brown")
#' )
#'
#' g_spaghettiplot(
#'   data = ADLB,
#'   subj_id = "USUBJID",
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   time = "AVISITCD",
#'   color_manual = color_manual,
#'   color_comb = "#39ff14",
#'   alpha = .02,
#'   xtick = c("BL", "W 1", "W 4"),
#'   xlabel = c("Baseline", "Week 1", "Week 4"),
#'   rotate_xlab = FALSE,
#'   group_stats = "median",
#'   hline_arb = 55,
#'   hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'   hline_vars_colors = c("pink", "brown", "purple", "gray")
#' )
#'
#' g_spaghettiplot(
#'   data = ADLB,
#'   subj_id = "USUBJID",
#'   biomarker_var = "PARAMCD",
#'   biomarker = "CRP",
#'   value_var = "AVAL",
#'   trt_group = "ARM",
#'   time = "AVISITCDN",
#'   color_manual = color_manual,
#'   color_comb = "#39ff14",
#'   alpha = .02,
#'   xtick = c(0, 1, 4),
#'   xlabel = c("Baseline", "Week 1", "Week 4"),
#'   rotate_xlab = FALSE,
#'   group_stats = "median",
#'   hline_arb = c(40, 50, 60),
#'   hline_arb_color = c("blue", "red", "green"),
#'   hline_arb_label = c("Arb_Hori_line_A", "Arb_Hori_line_B", "Arb_Hori_line_C"),
#'   hline_vars = c("ANRHI", "ANRLO")
#' )
g_spaghettiplot <- function(data,
                            subj_id = "USUBJID",
                            biomarker_var = "PARAMCD",
                            biomarker_var_label = "PARAM",
                            biomarker,
                            value_var = "AVAL",
                            unit_var = "AVALU",
                            trt_group,
                            trt_group_level = NULL,
                            time,
                            time_level = NULL,
                            color_manual = NULL,
                            color_comb = "#39ff14",
                            ylim = NULL,
                            alpha = 1.0,
                            facet_ncol = 2,
                            xtick = waiver(),
                            xlabel = xtick,
                            rotate_xlab = FALSE,
                            font_size = 12,
                            group_stats = "NONE",
                            hline_arb = numeric(0),
                            hline_arb_color = "red",
                            hline_arb_label = "Horizontal line",
                            hline_vars = character(0),
                            hline_vars_colors = "green",
                            hline_vars_labels = hline_vars) {

  ## Pre-process data
  label_trt_group <- attr(data[[trt_group]], "label")
  data[[trt_group]] <- if (!is.null(trt_group_level)) {
    factor(data[[trt_group]], levels = trt_group_level)
  } else {
    factor(data[[trt_group]])
  }
  attr(data[[trt_group]], "label") <- label_trt_group


  xtype <- ifelse(is.factor(data[[time]]) | is.character(data[[time]]), "discrete", "continuous")
  if (xtype == "discrete") {
    data[[time]] <- if (!is.null(time_level)) {
      factor(data[[time]], levels = time_level)
    } else {
      factor(data[[time]])
    }
  }

  # Plot
  plot_data <- data %>%
    filter(!!sym(biomarker_var) %in% biomarker)
  unit <- plot_data %>%
    select(!!sym(unit_var)) %>%
    unique() %>%
    extract2(1)
  unit1 <- ifelse(is.na(unit) | unit == "", " ", paste0(" (", unit, ") "))
  biomarker1 <- plot_data %>%
    select(!!sym(biomarker_var_label)) %>%
    unique() %>%
    extract2(1)
  gtitle <- paste0(biomarker1, unit1, value_var, " Values by Treatment @ Visits")
  gxlab <- if (is.null(attr(data[[time]], "label"))) time else attr(data[[time]], "label")
  gylab <- paste0(biomarker1, " ", value_var, " Values")

  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # Add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- h_caption_loqs_label(loqs_data = plot_data)

  plot <- ggplot(data = plot_data, aes_string(x = time, y = value_var, color = trt_group, group = subj_id)) +
    geom_point(size = 0.8, na.rm = TRUE) +
    geom_line(size = 0.4, alpha = alpha, na.rm = TRUE) +
    facet_wrap(trt_group, ncol = facet_ncol) +
    labs(caption = caption_loqs_label) +
    theme_bw() +
    ggtitle(gtitle) +
    xlab(gxlab) +
    ylab(gylab) +
    theme(plot.title = element_text(size = font_size, margin = margin(), hjust = 0.5))
  # Apply y-axis zoom range
  if (!is.null(ylim)) {
    plot <- plot + coord_cartesian(ylim = ylim)
  }
  # add group statistics
  # can't use stat_summary() because of presenting values for groups with all missings
  if (group_stats != "NONE") {
    if (group_stats == "MEAN") {
      plot_data_groupped <- plot_data %>%
        group_by(!!sym(trt_group), !!sym(time)) %>%
        transmute(AGG_VAL = mean(!!sym(value_var), na.rm = TRUE))

      plot_data_groupped$metric <- "Mean"
    } else {
      plot_data_groupped <- plot_data %>%
        group_by(!!sym(trt_group), !!sym(time)) %>%
        transmute(AGG_VAL = median(!!sym(value_var), na.rm = TRUE))

      plot_data_groupped$metric <- "Median"
    }
    plot <- plot +
      geom_line(
        aes_string(x = time, y = "AGG_VAL", group = 1, linetype = "metric"),
        data = plot_data_groupped,
        lwd = 1,
        color = color_comb,
        na.rm = TRUE
      ) +
      guides(linetype = guide_legend("Group statistic"))
  }
  # Format x-label
  if (xtype == "continuous") {
    plot <- plot +
      scale_x_continuous(breaks = xtick, labels = xlabel, limits = c(NA, NA))
  } else if (xtype == "discrete") {
    plot <- plot +
      scale_x_discrete(breaks = xtick, labels = xlabel)
  }
  if (rotate_xlab) {
    plot <- plot +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  # Add manual color
  if (!is.null(color_manual)) {
    plot <- plot +
      scale_color_manual(values = color_manual, name = trt_label)
  }

  # Format font size
  if (!is.null(font_size)) {
    plot <- plot +
      theme(
        plot.title = element_text(size = font_size, margin = margin()),
        axis.title.x = element_text(size = font_size),
        axis.text.x = element_text(size = font_size),
        axis.title.y = element_text(size = font_size),
        axis.text.y = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.text = element_text(size = font_size),
        strip.text.x = element_text(size = font_size),
        strip.text.y = element_text(size = font_size)
      )
  }
  # Add horizontal line for range based on option
  plot + geom_axes_lines(
    plot_data,
    hline_arb = hline_arb,
    hline_arb_color = hline_arb_color,
    hline_arb_label = hline_arb_label,
    hline_vars = hline_vars,
    hline_vars_colors = hline_vars_colors,
    hline_vars_labels = hline_vars_labels
  )
}
