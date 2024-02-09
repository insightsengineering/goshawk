#' Helper for identifying any `LLOQ` and `ULOQ` values in `LBSTRESC`. Outcome drives
#' horizontal line functionality display and legend labeling along with display
#' of values in footnote.
#'
#' @details Biomarker Sciences would like to have `LLOQ` and `ULOQ` values available for
#' reference in the visualizations. This also aids in setting the data constraint
#' ranges when `goshawk` functions are run from `teal.goshawk` `UI`.
#'
#' @param loqs_data (`data frame`)\cr `loqs_data` data set containing assay data with potential `LOQ` values
#'
#' @import dplyr
#' @keywords internal
#'
#' @examples
#'
#' goshawk:::h_identify_loq_values(loqs_data = goshawk::rADLB, flag_var = "LOQFL")
h_identify_loq_values <- function(loqs_data, flag_var) {
  ifelse(
    !grep("PARAM", names(loqs_data)),
    stop("Assay dataset must include variable PARAM to use the caption_loqs_label function."),
    1
  )
  ifelse(
    !grep("LBSTRESC", names(loqs_data)),
    stop("Assay dataset must include variable LBSTRESC to use the caption_loqs_label function."),
    1
  )

  # filter for records only relevant to loq.
  # get LLOQ value
    lloq <- loqs_data %>%
      filter(!!sym(flag_var) == "Y") %>%
      select("PARAM", "LBSTRESC") %>%
      filter(grepl("<", .data$LBSTRESC, fixed = FALSE)) %>%
      mutate(LLOQC = .data$LBSTRESC, LLOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
      group_by(.data$PARAM) %>%
      slice(1) %>%
      ungroup() %>%
      select(-"LBSTRESC")


  # get ULOQ value
    uloq <- loqs_data %>%
      filter(!!sym(flag_var) == "Y") %>%
      select("PARAM", "LBSTRESC") %>%
      filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
      mutate(ULOQC = .data$LBSTRESC, ULOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
      group_by(.data$PARAM) %>%
      slice(1) %>%
      ungroup() %>%
      select(-"LBSTRESC")


  # return LOQ data
  loq_values <- merge(lloq, uloq, by = "PARAM", all = TRUE)
  if (nrow(loq_values) == 0) {
    loq_values <- data.frame(
      PARAM = names(table(droplevels(as.factor(loqs_data$PARAM)))),
      LLOQC = NA,
      LLOQN = NA,
      ULOQC = NA,
      ULOQN = NA
    )
  }

  attr(loq_values[["PARAM"]], "label") <- "Parameter"
  attr(loq_values[["LLOQC"]], "label") <- "Lower Limit of Quantitation (C)"
  attr(loq_values[["LLOQN"]], "label") <- "Lower Limit of Quantitation"
  attr(loq_values[["ULOQC"]], "label") <- "Upper Limit of Quantitation (C)"
  attr(loq_values[["ULOQN"]], "label") <- "Upper Limit of Quantitation"

  return(loq_values)
}

#' Add footnote to identify `LLOQ` and `ULOQ` values identified from data
#'
#' @param loqs_data (`data frame`)\cr `loqs_data` data set containing assay data with potential `LOQ` values
#'
#' @import dplyr
#' @keywords internal
#'
#' @examples
#' caption_label <- goshawk:::h_caption_loqs_label(loqs_data = goshawk::rADLB, flag_var = "LOQFL")
h_caption_loqs_label <- function(loqs_data, flag_var) {
  loq_values <- h_identify_loq_values(loqs_data, flag_var)

  lloqc <- ifelse(is.na(loq_values$LLOQC), "NA", loq_values$LLOQC)
  uloqc <- ifelse(is.na(loq_values$ULOQC), "NA", loq_values$ULOQC)

  # create caption
  caption_loqs_label <- paste0(
    "Limits of quantification read from study data for ",
    loqs_data$PARAM,
    ": LLOQ is ",
    lloqc,
    ", ULOQ is ",
    uloqc
  )

  return(caption_loqs_label)
}

#' Check that argument is a valid color
#'
#' Checks if the argument can be converted to valid RGB values space. See `grDevices::col2rgb`.
#'
#' @inheritParams checkmate::checkCharacter
#' @param color (`character`)\cr
#'  Valid color convertible to RGB scale by [grDevices::col2rgb()]
#'
#' @inherit checkmate::checkCharacter return
#' @keywords internal
check_color <- function(color,
                        min.len = NULL, # nolint
                        max.len = NULL, # nolint
                        any.missing = TRUE, # nolint
                        all.missing = TRUE, # nolint
                        len = NULL,
                        null.ok = FALSE) { # nolint
  string_check <- checkmate::check_character(
    color,
    min.len = min.len, max.len = max.len, any.missing = any.missing, len = len, null.ok = null.ok
  )
  if (!isTRUE(string_check)) {
    return(string_check)
  }

  res <- sapply(color, function(col) {
    tryCatch(
      is.matrix(grDevices::col2rgb(col)),
      error = function(e) FALSE
    )
  })

  if (any(!res)) {
    "Must be a valid color, convertible to rgb by 'col2rgb'"
  } else {
    TRUE
  }
}

#' @rdname check_color
assert_color <- checkmate::makeAssertionFunction(check_color)
