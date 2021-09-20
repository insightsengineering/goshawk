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
#' goshawk:::h_identify_loq_values(loqs_data = ADLB)
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
    mutate(LLOQC = .data$LBSTRESC, LLOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
    select(-.data$LBSTRESC)

  # get ULOQ value
  uloq <- loqs_data %>%
    select(.data$PARAM, .data$LBSTRESC) %>%
    filter(grepl(">", .data$LBSTRESC, fixed = FALSE)) %>%
    mutate(ULOQC = .data$LBSTRESC, ULOQN = as.numeric(gsub("[^0-9.-]", "", .data$LBSTRESC))) %>%
    group_by(.data$PARAM) %>%
    slice(1) %>%
    ungroup %>%
    select(-.data$LBSTRESC)

  # return LOQ data
  loq_values <- merge(lloq, uloq, by = "PARAM", all = TRUE)
  if (nrow(loq_values) == 0) {
    message(paste("Number of rows is:", nrow(loq_values)))
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

  if (is.na(loq_values$LLOQC)) {
    lloq_value <- "NA"
  } else {
    lloq_value <- loq_values$LLOQC
  }

  if (is.na(loq_values$ULOQC)) {
    uloq_value <- "NA"
  } else {
    uloq_value <- loq_values$ULOQC
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

#' validate arbitrary horizontal lines
#'
validate_hori_line_args <- function(data,
                                               hline_arb = NULL,
                                               hline_arb_color = "red",
                                               hline_arb_label = NULL,
                                               hline_vars = NULL,
                                               hline_vars_colors = NULL,
                                               hline_vars_labels = NULL) {

  new_hline_col <- if (!is.null(hline_arb)) {
    if (is.null(hline_arb_color)) {
      hline_arb_color <- "red"
    } else {
      stopifnot(is_character_single(hline_arb_color))
    }
    if (is.null(hline_arb_label)) {
      hline_arb_label <- "Arbitrary Horizontal Line"
    } else {
      stopifnot(is_character_single(hline_arb_label))
    }
    stopifnot(is_numeric_single(hline_arb))

    new_hline_col <- "Arbitrary_Horizontal_Line"
    i <- 1
    while (new_hline_col %in% names(data)) {
      new_hline_col <- paste0(new_hline_col, "_", i)
      i <- i + 1
    }
    new_hline_col
  }

  if (!is.null(hline_vars)) {
    stopifnot(is_character_vector(hline_vars, min_length = 1, max_length = length(data)))
    stopifnot(all(hline_vars %in% names(data)))
    stopifnot(
      all(vapply(
        hline_vars,
        FUN = function(x) is.numeric(data[[x]]) == 1,
        FUN.VALUE = logical(1)
      )
      )
    )
    if (!is.null(hline_vars_labels)) {
      stopifnot(is_character_vector(
        hline_vars_labels, min_length = length(hline_vars),
        max_length = (length(hline_vars)))
      )
    } else {
      hline_vars_labels <- vapply(
        hline_vars,
        FUN = function(x) if_null(attributes(data[[x]])$label, ""),
        FUN.VALUE = character(1)
      )
      hline_vars_labels <- vapply(
        seq_along(hline_vars_labels),
        FUN = function(x) `if`(hline_vars_labels[x] == "", hline_vars[x], hline_vars_labels[x]),
        FUN.VALUE = character(1)
      )
    }
    if (!is.null(hline_vars_colors)) {
      stopifnot(is_character_vector(
        hline_vars_colors,
        min_length = length(hline_vars),
        max_length = (length(hline_vars)))
      )
    }
  }
  return(new_hline_col)
}

#' Add horizontal lines and their legend labels to a plot
#'
add_horizontal_lines <- function(plot,
                                 plot_data,
                                 agg_label,
                                 color_comb,
                                 new_hline_col,
                                 hline_arb = NULL,
                                 hline_arb_color = "red",
                                 hline_arb_label = NULL,
                                 hline_vars = NULL,
                                 hline_vars_colors = NULL,
                                 hline_vars_labels = NULL) {

  range_color <- c(
    if_null(hline_vars_colors, if_not_null(hline_vars, seq(length(hline_vars)))),
    if_not_null(hline_arb, hline_arb_color)
  )
  if (!is.null(hline_arb)) {
    hline_vars <- c(hline_vars, new_hline_col)
    plot_data[new_hline_col] <- hline_arb
  }

  j <- 1
  for (i in hline_vars) {
    plot <- plot +
      geom_hline(
        data = plot_data,
        aes_(yintercept = plot_data[[i]][1], linetype = as.factor(paste0("dashed_", i))),
        size = 0.5,
        color = range_color[j]
      )
    j <- j + 1
  }

  plot <- plot +
    scale_linetype_manual(
      name = "Description of Horizontal Line(s)",
      label = c(if_null(c(hline_vars_labels, hline_arb_label), hline_vars), agg_label),
      values = c(rep(2, length(hline_vars)), if_not_null(agg_label, 1))
    ) +
    guides(linetype = guide_legend(override.aes = list(color = c(range_color, if_not_null(agg_label, color_comb))))) + # nolint
    theme(legend.key.size = unit(0.5, "in"))
}
