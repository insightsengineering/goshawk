#' The goshawk package provides longitudinal visualizations of lab/biomarker data
#'
#' @description The goshawk package provides longitudinal visualizations of lab/biomarker data
#'
#' @docType package
#'
#' @name goshawk
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data := !!
#' @keywords internal
NULL

#' @import mcr
#' @importFrom utils getFromNamespace
mc.deming <- getFromNamespace("mc.deming", "mcr") # nolint
