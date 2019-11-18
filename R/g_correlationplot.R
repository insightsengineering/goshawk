#' Function to create a correlation plot.
#'
#' Default plot displays correlation facetted by visit with color attributed treatment arms and
#' symbol attributed LOQ values.
#'
#' @param label text string to used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ALB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param xaxis_param x-axis biomarker to visualize e.g. IGG.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param xvar x-axis analysis variable from transposed data set.
#' @param yaxis_param y-axis biomarker to visualize e.g. IGG.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param yvar y-axis analysis variable from transposed data set.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param visit name of variable containing nominal visits e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LOQFL.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xmin x-axis lower zoom limit.
#' @param xmax x-axis upper zoom limit.
#' @param ymin y-axis lower zoom limit.
#' @param ymax y-axis upper zoom limit.
#' @param title_text plot title.
#' @param xaxis_lab x-axis label.
#' @param yaxis_lab y-axis label.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient.
#' Use with facet = TRUE.
#' @param hline y-axis value to position a horizontal line.
#' @param vline x-axis value to position a vertical line.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses deming model.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(ggplot2)
#' library(goshawk)
#' library(random.cdisc.data)
#' library(stringr)
#' library(tidyr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#' color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
#'
#' ASL <- radsl(N = 20, seed = 1)
#' ALB <- radlb(ASL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#' ALB <- ALB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL", grepl("WEEK", AVISIT) ~ paste("W", trimws(substr(AVISIT,
#'       start = 6,
#'       stop = str_locate(AVISIT, "DAY") - 1
#'     ))),
#'     TRUE ~ as.character(NA)
#'   )) %>%
#'   mutate(AVISITCDN = case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0, grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#'     TRUE ~ as.numeric(NA)
#'   )) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)
#'     )
#'   )) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>% reorder(TRTORD))
#'
#' # given the 2 param and 2 analysis vars we need to transform the data
#' plot_data_t1 <- ALB %>%
#'   gather(ANLVARS, ANLVALS, BASE2, BASE, AVAL, BASE, LOQFL) %>%
#'   mutate(ANL.PARAM = ifelse(ANLVARS == "LOQFL", paste0(ANLVARS, "_", PARAMCD), paste0(
#'     ANLVARS,
#'     ".", PARAMCD
#'   ))) %>%
#'   select(USUBJID, ARM, ARMCD, AVISITN, AVISITCD, ANL.PARAM, ANLVALS) %>%
#'   spread(ANL.PARAM, ANLVALS)
#' # the transformed analysis value variables are character and need to be converted to numeric for
#' # ggplot
#' # remove records where either of the analysis variables are NA since they will not appear on the
#' # plot and will ensure that LOQFL = NA level is removed
#' plot_data_t2 <- plot_data_t1 %>%
#'   subset(!is.na(BASE.CRP) & !is.na(AVAL.ALT)) %>%
#'   mutate_at(vars(contains(".")), as.numeric) %>%
#'   mutate(LOQFL_COMB = ifelse(LOQFL_CRP == "Y" | LOQFL_ALT == "Y", "Y", "N"))
#'
#' g_correlationplot(
#'   label = "Correlation Plot",
#'   data = plot_data_t2,
#'   param_var = "PARAMCD",
#'   xaxis_param = c("CRP"),
#'   xaxis_var = "AVAL",
#'   xvar = "AVAL.CRP",
#'   yaxis_param = c("ALT"),
#'   yaxis_var = "BASE",
#'   yvar = "BASE.ALT",
#'   trt_group = "ARM",
#'   visit = "AVISITCD",
#'   loq_flag_var = "LOQFL",
#'   unit = "AVALU",
#'   xmin = 0,
#'   xmax = 200,
#'   ymin = 0,
#'   ymax = 2000,
#'   title_text = "Test",
#'   xaxis_lab = "Test x",
#'   yaxis_lab = "Test y",
#'   color_manual = color_manual,
#'   shape_manual = shape_manual,
#'   facet_ncol = 4,
#'   facet = FALSE,
#'   facet_var = "ARM",
#'   reg_line = TRUE,
#'   hline = NULL,
#'   vline = .5,
#'   rotate_xlab = FALSE,
#'   font_size = 14,
#'   dot_size = 2,
#'   reg_text_size = 3
#' )
#' }
#'
g_correlationplot <- function(label = "Correlation Plot",
                              data,
                              param_var = "PARAMCD",
                              xaxis_param = "CRP",
                              xaxis_var = "BASE",
                              xvar = xvar,
                              yaxis_param = "IGG",
                              yaxis_var = "AVAL",
                              yvar = yvar,
                              trt_group = "ARM",
                              visit = "AVISITCD",
                              loq_flag_var = "LOQFL",
                              unit = "AVALU",
                              xmin = NA,
                              xmax = NA,
                              ymin = NA,
                              ymax = NA,
                              title_text = title_text,
                              xaxis_lab = xaxis_lab,
                              yaxis_lab = yaxis_lab,
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
  # create correlation plot over time pairwise per treatment arm
  plot_data <- data
  # Setup legend label
  if (is.null(attr(data[[trt_group]], "label"))) {
    trt_label <- "Dose"
  } else {
    trt_label <- attr(data[[trt_group]], "label")
  }
  # create plot foundation - titles and axes labels are defined in
  # teal.goshawk.tm_g_correlationplot.R
  plot1 <- ggplot2::ggplot(
    data = plot_data,
    aes_string(
      x = xvar,
      y = yvar,
      color = trt_group
    )
  ) +
    geom_point(aes_string(shape = "LOQFL_COMB"), size = dot_size, na.rm = TRUE) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    facet_wrap(as.formula(paste0(" ~ ", visit)), ncol = facet_ncol) +
    theme_bw() +
    ggtitle(title_text) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(xaxis_lab) +
    ylab(yaxis_lab)
  # add grid faceting to foundation
  if (facet) {
    plot1 <- plot1 +
      facet_grid(as.formula(paste0(facet_var, " ~ ", visit)))
  }
  # add regression line
  if (reg_line) {
    slope <- function(x, y) {
      ratio <- sd(x) / sd(y)
      if (!is.na(ratio) & ratio > 0) {
        reg <- mc.deming(y, x, ratio)
        # return the evaluation of the ratio condition as third value in numeric vector to control
        # downstream processing
        return(c(round(reg$b0, 2), round(reg$b1, 2), !is.na(ratio) & ratio > 0))
      }
      # if ratio condition is not met then assign NA to vector so that NULL condition does not throw
      # error below
      return(as.numeric(c(NA, NA, NA)))
    }
    sub_data <- subset(
      plot_data,
      !is.na(eval(parse(text = yvar))) & !is.na(eval(parse(text = xvar)))
    ) %>%
      group_by_(.dots = c(trt_group, visit)) %>%
      mutate(intercept = slope(eval(parse(text = yvar)), eval(parse(text = xvar)))[1]) %>%
      mutate(slope = slope(eval(parse(text = yvar)), eval(parse(text = xvar)))[2]) %>%
      mutate(
        corr = ifelse(slope(eval(parse(text = yvar)),
                            eval(parse(text = xvar)))[3],
                      cor(eval(parse(text = yvar)),
                          eval(parse(text = xvar)),
                          method = "spearman",
                          use = "complete.obs"),
                      NA
        ))
    plot1 <- plot1 +
      geom_abline(
        data = filter(sub_data, row_number() == 1), # only need to return 1 row within group_by
        aes_string(
          intercept = "intercept",
          slope = "slope",
          color = trt_group
        )
      ) +
      geom_text(
        data = filter(sub_data, row_number() == 1),
        aes_(
          x = -Inf,
          y = Inf,
          hjust = 0,
          vjust = 1,
          label = ~ ifelse(!is.na(intercept) & !is.na(slope) & !is.na(corr),
                           sprintf("y = %.2f+%.2fX\ncor = %.2f", intercept, slope, corr),
                           paste0("Insufficient Data For Regression")
          ),
          color = sym(trt_group)
        ),
        size = reg_text_size
      ) +
      labs(caption = paste("Deming Regression Model, Spearman Correlation Method"))
  }
  # Format font size
  if (!is.null(font_size)) {
    plot1 <- plot1 +
      theme(
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
      geom_point(aes_string(shape = "LOQFL_COMB"), size = dot_size, na.rm = TRUE)
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
