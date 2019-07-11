#' To prevent "no visible binding for global variable" note during R CMD check.
#' 
#' @name global_variables

utils::globalVariables(c("intercept", "slope", "corr",
                         ".", "str_count",
                         "hcl",
                         "PctLOQ", "TRTORD", ":=", "MAXTRTORDVIS", "Biomarker", "Visit"))