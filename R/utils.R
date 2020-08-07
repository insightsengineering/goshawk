#' Helper for identifying the LLOQ and ULOQ values used in the visualization footnote.
#'
#' @details Biomarker Sciences would like to have LLOQ and ULOQ values available for
#' reference in the visualizations. This also aids in setting the data constraint
#' ranges when goshawk functions are run from teal.goshawk UI.
#'
#' @param loqs_data dataset containing assay data with potential LOQ values
#'
#' @import dplyr
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADLB <- radlb(cached = TRUE)
#'
#' # add footnote to identify LLOQ and ULOQ values pulled from data
#' caption_label <- goshawk:::caption_loqs_label(loqs_data = ADLB)
caption_loqs_label <- function(loqs_data) {
  ifelse (!grep("PARAM", names(loqs_data)),
          stop("Assay dataset must include variable PARAM to use the caption_loqs_label function."),
          1)
  ifelse (!grep("LBSTRESC", names(loqs_data)),
          stop("Assay dataset must include variable LBSTRESC to use the caption_loqs_label function."),
          1)

  # get LLOQ value
  lloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl("<", .data$LBSTRESC, fixed = FALSE)) %>%
    group_by(.data$LBSTRESC) %>%
    slice(1)

  if (nrow(lloq) == 0) {
    lloq_value <- "NA"
  } else {
    lloq_value <- lloq$LBSTRESC
  }

  # get ULOQ value
  uloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
    group_by(.data$LBSTRESC) %>%
    slice(1)

  if (nrow(uloq) == 0) {
    uloq_value <- "NA"
  } else {
    uloq_value <- uloq$LBSTRESC
  }

  # create caption
  caption_loqs_label <- paste0("Limits of quantification read from study data for ",
                               loqs_data$PARAM, ": LLOQ = ", lloq_value, ", ULOQ = ", uloq_value)

  return(caption_loqs_label)

}