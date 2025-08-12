#' Function to create a scatter plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#'  `g_scatterplot()` is deprecated. Please use
#'  [g_correlationplot()] instead. Default plot displays scatter facetted by
#'  visit with color attributed treatment arms and symbol attributed `LOQ` values.
#'
#' @param label text string to used to identify plot.
#' @param data `ADaM` structured analysis laboratory data frame e.g. `ADLB`.
#' @param param_var name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param param biomarker to visualize e.g. `IGG`.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. `BASE`.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axis e.g. `AVAL`.
#' @param trt_group name of variable representing treatment group e.g. `ARM`.
#' @param visit name of variable containing nominal visits e.g. `AVISITCD`.
#' @param loq_flag_var name of variable containing `LOQ` flag e.g. `LOQFL`.
#' @param unit name of variable containing biomarker unit e.g. `AVALU`.
#' @param xlim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the x-axis
#'   if the default limits are not suitable.
#' @param ylim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the y-axis
#'   if the default limits are not suitable.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values.
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
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses `deming` model.
#'
#' @export
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
                          xlim = c(NA, NA),
                          ylim = c(NA, NA),
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
  lifecycle::deprecate_stop(
    when = "0.1.15",
    what = "g_scatterplot()",
    details = "You should use goshawk::g_correlationplot instead of goshawk::g_scatterplot"
  )
}
