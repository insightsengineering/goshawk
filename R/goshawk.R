#' The goshawk package provides longitudinal visualizations of lab/biomarker data
#'
#' @docType package
#'
#' @name goshawk
#'
#' @import dplyr
#' @import ggplot2
#' @import utils.nest
#'
NULL

#' @importFrom rlang .data := !!
#' @importFrom utils getFromNamespace
#' @import mcr
mc.deming <- getFromNamespace("mc.deming", "mcr") #nolint
