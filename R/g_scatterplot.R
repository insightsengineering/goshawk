#' Function to create a scatter plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `g_scatterplot()` is deprecated. 
#' Please use [g_correlationplot()] instead.
#'
#' @param ... function is deprecated.
#'
#' @export
#'
g_scatterplot <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.1.15",
    what = "g_scatterplot()",
    details = "You should use goshawk::g_correlationplot instead of goshawk::g_scatterplot"
  )
}
