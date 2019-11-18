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
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time. Only use it when
#' x-axis variable is character or factor.
#' @param color_manual vector of colors.
#' @param color_comb name or hex value for combined treatment color.
#' @param ylim numeric vector to define y-axis range.
#' @param alpha subject line transparency (0 = transparent, 1 = opaque)
#' @param facet_ncol number of facets per row.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric.
#' Default value is waiver().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values. Default
#'  value is waiver().
#' @param rotate_xlab boolean whether to rotate x-axis labels.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param group_stats control group mean or median overlay.
#'
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(random.cdisc.data)
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo",
#' "C: Combination" = "Combination")
#' color_manual <-  c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#'
#' ASL <- cadsl
#' ALB <- cadlb
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
#'
#' g_spaghettiplot(data = ALB,
#'                 subj_id = "USUBJID",
#'                 biomarker_var = "PARAMCD",
#'                 biomarker = "CRP",
#'                 value_var = "AVAL",
#'                 trt_group = "ARM",
#'                 time = "AVISITCD",
#'                 color_manual = color_manual,
#'                 color_comb = "#39ff14",
#'                 alpha = .02,
#'                 hline = NULL,
#'                 rotate_xlab = FALSE,
#'                 group_stats = "median")
#'
#'}
#'
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
                            hline = NULL,
                            xtick = waiver(), xlabel = xtick,
                            rotate_xlab = FALSE,
                            font_size = 12,
                            group_stats = "NONE"){
  ## Pre-process data
  if (!is.null(trt_group_level)){
    data[[trt_group]] <- factor(data[[trt_group]],
                                levels = trt_group_level)
  } else {
    data[[trt_group]] <- factor(data[[trt_group]])
  }
  if (is.factor(data[[time]]) | is.character(data[[time]])){
    xtype <- "discrete"
  } else {
    xtype <- "continuous"
  }
  if (xtype == "discrete"){
    if (!is.null(time_level)){
      data[[time]] <- factor(data[[time]],
                             levels = time_level)
    } else {
      data[[time]] <- factor(data[[time]])
    }
  }
  # Plot
  for.plot <- data[data[[biomarker_var]] %in% biomarker, ]
  unit <- unique(filter(data, !!sym(biomarker_var) == biomarker)[[unit_var]])
  unit1 <- ifelse(is.na(unit) | unit == "", " ", paste0(" (", unit, ") "))
  biomarker1 <- unique(filter(data, !!sym(biomarker_var) == biomarker)[[biomarker_var_label]])
  gtitle <- paste0(biomarker1, unit1, value_var, " Values by Treatment @ Visits")
  gylab <- paste0(biomarker1, " ", value_var, " Values")
  # re-establish treatment variable label
  if (trt_group == "ARM"){
    attributes(for.plot$ARM)$label <- "Planned Arm"
  } else {
    attributes(for.plot$ACTARM)$label <- "Actual Arm"
  }
  # Setup legend label
  trt_label <- `if`(is.null(attr(for.plot[[trt_group]], "label")),
                    "Dose",
                    attr(for.plot[[trt_group]], "label"))
  plot <- ggplot(data = for.plot,
                 aes_string(x = time, y = value_var, color = trt_group, group = subj_id)) +
    geom_point(size = 0.8) +
    geom_line(size = 0.4, alpha = alpha) +
    facet_wrap(trt_group, ncol = facet_ncol) +
    theme_bw() +
    ggtitle(gtitle) +
    xlab(time) +
    ylab(gylab) +
    theme(plot.title = element_text(size = font_size, margin = margin(), hjust = 0.5))
  # Apply y-axis zoom range
  if (!is.null(ylim)){
    plot <- plot + coord_cartesian(ylim = ylim)
  }
  # add group statistics
  if (group_stats != "NONE"){
    if (group_stats == "MEAN"){
      plot <- plot +
        stat_summary(aes(group = 1, linetype = "Group Mean"),
                     fun.y = mean, geom = "line", lwd = 1, color = color_comb) +
        scale_linetype_manual(name = "", label = "Group Mean", values = c(1))
    } else{
      plot <- plot +
        stat_summary(aes(group = 1, linetype = "Group Median"),
                     fun.y = median, geom = "line", lwd = 1, color = color_comb) +
        scale_linetype_manual(name = "", label = "Group Median", values = c(1))
    }
  }
  # Format x-label
  if (xtype == "continuous") {
    plot <- plot +
      scale_x_continuous(breaks = xtick, labels = xlabel, limits = c(NA, NA))
  }
  if (rotate_xlab){
    plot <- plot +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  # Add manual color
  if (!is.null(color_manual)){
    plot <- plot +
      scale_color_manual(values = color_manual, name = trt_label)
  }
  #Add horizontal line
  if (!is.null(hline)){
    plot <- plot +
      geom_hline(aes(yintercept = hline), color = "red", linetype = "dashed", size = 0.5)
  }
  # Format font size
  if (!is.null(font_size)){
    plot <- plot +
      theme(plot.title = element_text(size = font_size, margin = margin()),
            axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }
  plot
}
