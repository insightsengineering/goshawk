#' Function to create a scatter plot.
#'
#' Default plot displays scatter facetted by visit with color attributed treatment arms and symbol
#' attributed LOQ values.
#'
#' @param label text string to used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param visit name of variable containing nominal visits e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LOQFL.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xmin x-axis lower zoom limit.
#' @param xmax x-axis upper zoom limit.
#' @param ymin y-axis lower zoom limit.
#' @param ymax y-axis upper zoom limit.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in
#' visualization. Use with facet = TRUE.
#' @param hline y-axis value to position a horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @importFrom stats as.formula cor median quantile sd
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses deming model.
#'
#' @importFrom stats setNames
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
#' color_manual <-  c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <-  c("N" = 1, "Y" = 2, "NA" = 0)
#'
#' ASL <- cadsl
#' ALB <- cadlb
#' var_labels <- lapply(ALB, function(x) attributes(x)$label)
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
#'  attr(ALB[["ARM"]], "label") <- var_labels[["ARM"]]
#'
#' g_scatterplot(label = "Scatter Plot",
#'            data = ALB,
#'            param_var = "PARAMCD",
#'            param = c("ALT"),
#'            xaxis_var = "BASE",
#'            yaxis_var = "AVAL",
#'            trt_group = "ARM",
#'            visit = "AVISITCD",
#'            loq_flag_var = "LOQFL",
#'            unit = "AVALU",
#'            xmin = 0,
#'            xmax = 200,
#'            ymin = 0,
#'            ymax = 200,
#'            color_manual = color_manual,
#'            shape_manual = shape_manual,
#'            facet_ncol = 2,
#'            facet = TRUE,
#'            facet_var = "ARM",
#'            reg_line = TRUE,
#'            hline = NULL,
#'            vline = .5,
#'            rotate_xlab = TRUE,
#'            font_size = 14,
#'            dot_size = 2,
#'            reg_text_size = 3)
#'
#'
g_scatterplot <- function(label = "Scatter Plot",
                          data,
                          param_var = "PARAMCD",
                          param = "CRP",
                          xaxis_var = "BASE",
                          yaxis_var = "AVAL",
                          trt_group = "ARM",
                          visit = "AVISITCD",
                          loq_flag_var = "LOQFL",
                          unit = "AVALU",
                          xmin = NA,
                          xmax = NA,
                          ymin = NA,
                          ymax = NA,
                          color_manual = NULL,
                          shape_manual = NULL,
                          facet_ncol = 2,
                          facet = FALSE,
                          facet_var = "ARM",
                          reg_line = FALSE,
                          hline = NULL,
                          vline = NULL,
                          rotate_xlab = FALSE,
                          font_size = 12,
                          dot_size = NULL,
                          reg_text_size = 3) {
  # create scatter plot over time pairwise per treatment arm
  plot_data <- data %>%
    filter(!!sym(param_var) == param)
  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitle_label <- ifelse(is.null(unit), paste0(plot_data$PARAM, "@ Visits"),
                          ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, "@ Visits"),
                                 paste0(plot_data$PARAM, " (", plot_data[[unit]], ") @ Visits"))
  )
  # Setup the x-axis label.  Combine the biomarker and the units (if available)
  x_axis_label <- ifelse(is.null(unit), paste(plot_data$PARAM, xaxis_var, "Values"),
                         ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, xaxis_var, "Values"),
                                paste0(plot_data$PARAM, " (", plot_data[[unit]], ") ", xaxis_var, " Values"))
  )
  # Setup the y-axis label.  Combine the biomarker and the units (if available)
  y_axis_label <- ifelse(is.null(unit), paste(plot_data$PARAM, yaxis_var, "Values"),
                         ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, yaxis_var, "Values"),
                                paste0(plot_data$PARAM, " (", plot_data[[unit]], ") ", yaxis_var, " Values"))
  )
  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))
  # create plot foundation
  plot1 <- ggplot2::ggplot(data = plot_data, aes_string(x = xaxis_var, y = yaxis_var, color = trt_group)) +
    geom_point(aes_string(shape = loq_flag_var), size = dot_size, na.rm = TRUE) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    facet_wrap(as.formula(paste0(" ~ ", visit)), ncol = facet_ncol) +
    theme_bw() +
    ggtitle(ggtitle_label) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(x_axis_label) +
    ylab(y_axis_label)
  # add grid faceting to foundation
  if (facet) {
    plot1 <- plot1 +
      facet_grid(as.formula(paste0(facet_var, " ~ ", visit)))
  }
  # add regression line
  if (reg_line) {
    slope <- function(x, y) {
      ratio <- sd(x) / sd(y)
      res <- if (!is.na(ratio) & ratio > 0) {
        reg <- mc.deming(y, x, ratio)
        # return the evaluation of the ratio condition as third value in numeric vector
        # for conttroling downstream processing
        c(round(reg$b0, 2), round(reg$b1, 2),
          ifelse(!is.na(ratio) && ratio > 0, cor(y, x, method = "spearman", use = "complete.obs"), NA_real_))
      } else {
        # if ratio condition is not met then assign NA to returned vector
        # so that NULL condition does not throw error below
        as.numeric(c(NA, NA, NA))
      }
      return(as_tibble(setNames(as.list(res), c("intercept", "slope", "corr"))))
    }
    sub_data <- plot_data %>%
      select(!!sym(trt_group), !!sym(visit), !!sym(xaxis_var), !!sym(yaxis_var)) %>%
      filter(!is.na(!!sym(yaxis_var)) & !is.na(!!sym(xaxis_var))) %>%
      group_by(!!sym(trt_group), !!sym(visit)) %>%
      do(slope(.data[[yaxis_var]], .data[[xaxis_var]]))

    if (!(all(is.na(sub_data$intercept)) && all(is.na(sub_data$slope)))) {
      plot1 <- plot1 +
        geom_abline(data = sub_data,
                    # has to put some neutral values for missings, i.e. big & negative intercept + 0 slope
                    aes(intercept = vapply(.data$intercept, if_na, numeric(1), -9999),
                        slope = vapply(.data$slope, if_na, numeric(1), 0),
                        color = !!sym(trt_group)))
    }
    plot1 <- plot1 +
      geom_text(
        data = filter(sub_data, row_number() == 1),
        aes(
          x = -Inf,
          y = Inf,
          hjust = 0,
          vjust = 1,
          label = ifelse(
            !is.na(.data$intercept) & !is.na(.data$slope) & !is.na(.data$corr),
            sprintf("y = %.2f+%.2fX\ncor = %.2f", .data$intercept, .data$slope, .data$corr),
            paste0("Insufficient Data For Regression")),
          color = !!sym(trt_group)),
        size = reg_text_size) +
      labs(caption = paste("Deming Regression Model, Spearman Correlation Method"))
  }
  # Add abline
  if (yaxis_var %in% c("AVAL", "AVALL2", "BASE2", "BASE2L2", "BASE", "BASEL2")) {
    plot1 <- plot1 + geom_abline(intercept = 0, slope = 1)
  }
  if (yaxis_var %in% c("CHG2", "CHG")) {
    plot1 <- plot1 + geom_abline(intercept = 0, slope = 0)
  }
  if (yaxis_var %in% c("PCHG2", "PCHG")) {
    plot1 <- plot1 + geom_abline(intercept = 100, slope = 0)
  }
  # Format font size
  if (!is.null(font_size)) {
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }
  # Format treatment color
  if (!is.null(color_manual)) {
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = trt_label)
  }
  # Format LOQ flag symbol shape
  if (!is.null(shape_manual)) {
    plot1 <- plot1 +
      scale_shape_manual(values = shape_manual, name = "LOQ")
  }
  # Format dot size
  if (!is.null(dot_size)) {
    plot1 <- plot1 +
      geom_point(aes_string(shape = loq_flag_var), size = dot_size, na.rm = TRUE)
  }
  # Format x-label
  if (rotate_xlab) {
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  # Add horizontal line
  if (!is.null(hline)) {
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color = "red", linetype = "dashed", size = 0.5)
  }
  # Add vertical line
  if (!is.null(vline)) {
    plot1 <- plot1 +
      geom_vline(aes(xintercept = vline), color = "red", linetype = "dashed", size = 0.5)
  }
  plot1
}
