#' Helper for identifying any LLOQ and ULOQ values in LBSTRESC. Outcome drives
#' horizontal line functionality display and legend labeling along with display
#' of values in footnote.
#'
#' @details Biomarker Sciences would like to have LLOQ and ULOQ values available for
#' reference in the visualizations. This also aids in setting the data constraint
#' ranges when goshawk functions are run from teal.goshawk UI.
#'
#' @param loqs_data (`data frame`)\cr loqs_data data set containing assay data with potential LOQ values
#'
#' @import dplyr
#'
#' @examples
#' library(scda)
#'
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' h_identify_loq_values(loqs_data = ADLB)
#'
h_identify_loq_values <- function(loqs_data) {
  ifelse(
    !grep("PARAM", names(loqs_data)),
    stop("Assay dataset must include variable PARAM to use the caption_loqs_label function."),
    1)
  ifelse(
    !grep("LBSTRESC", names(loqs_data)),
    stop("Assay dataset must include variable LBSTRESC to use the caption_loqs_label function."),
    1)

  # get LLOQ value
  lloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl("<", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(LLOQ_VALUE_C = .data$LBSTRESC, LLOQ_VALUE_N = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
    select(-.data$LBSTRESC)

  # get ULOQ value
  uloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(ULOQ_VALUE_C = .data$LBSTRESC, ULOQ_VALUE_N = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
    select(-.data$LBSTRESC)

  # return LOQ data
  loq_values <- merge(lloq, uloq, by = "PARAM", all = TRUE)
  if (nrow(loq_values) == 0) {
    message(paste("Number of rows is:", nrow(loq_values)))
    loq_values <- data.frame(
      PARAM = names(table(droplevels(loqs_data$PARAM))),
      LLOQ_VALUE_C = NA,
      LLOQ_VALUE_N = NA,
      ULOQ_VALUE_C = NA,
      ULOQ_VALUE_N = NA
    )
  }

  attr(loq_values[["PARAM"]], "label") <- "Parameter"
  attr(loq_values[["LLOQ_VALUE_C"]], "label") <- "Lower Limit of Quantitation (C)"
  attr(loq_values[["LLOQ_VALUE_N"]], "label") <- "Lower Limit of Quantitation"
  attr(loq_values[["ULOQ_VALUE_C"]], "label") <- "Upper Limit of Quantitation (C)"
  attr(loq_values[["ULOQ_VALUE_N"]], "label") <- "Upper Limit of Quantitation"

  return(loq_values)
}

#' Add footnote to identify LLOQ and ULOQ values identified from data
#'
#' @param loqs_data (`data frame`)\cr loqs_data data set containing assay data with potential LOQ values
#'
#' @import dplyr
#'
#' @examples
#' library(scda)
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' caption_label <- goshawk:::h_caption_loqs_label(loqs_data = ADLB)
#'
h_caption_loqs_label <- function(loqs_data) {

  loq_values <- h_identify_loq_values(loqs_data)

  if (is.na(loq_values$LLOQ_VALUE_C)) {
    lloq_value <- "NA"
  } else {
    lloq_value <- loq_values$LLOQ_VALUE_C
  }

  if (is.na(loq_values$ULOQ_VALUE_C)) {
    uloq_value <- "NA"
  } else {
    uloq_value <- loq_values$ULOQ_VALUE_C
  }

  # create caption
  caption_loqs_label <- paste0(
    "Limits of quantification read from study data for ",
    loqs_data$PARAM,
    ": LLOQ is ",
    lloq_value,
    ", ULOQ is ",
    uloq_value)

  return(caption_loqs_label)

}
