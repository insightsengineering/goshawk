#' The goshawk package provides longitudinal visualizations of lab/biomarker data
#'
#' @description The goshawk package provides longitudinal visualizations of lab/biomarker data
#'
#' @import dplyr
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr extract2
#' @importFrom rlang .data := !!
#'
#' @keywords internal
"_PACKAGE"

#' @import mcr
#' @importFrom utils getFromNamespace
mc.deming <- utils::getFromNamespace("mc.deming", "mcr") # nolint
