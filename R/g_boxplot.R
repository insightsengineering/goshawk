#' Function to create a boxplot.
#'
#' A box plot is a method for graphically depicting groups of numerical data
#' through their quartiles. Box plots may also have lines extending vertically
#' from the boxes (whiskers) indicating variability outside the upper and lower
#' quartiles, hence the term box-and-whisker. Outliers may be plotted as
#' individual points. Box plots are non-parametric: they display variation in
#' samples of a statistical population without making any assumptions of the
#' underlying statistical distribution. The spacings between the different parts
#' of the box indicate the degree of dispersion (spread) and skewness in the
#' data, and show outliers. In addition to the points themselves, they allow one
#' to visually estimate various L-estimators, notably the interquartile range,
#' midhinge, range, mid-range, and trimean.
#'
#' @param data ADaM structured analysis laboratory data frame e.g. ADLB.
#' @param biomarker biomarker to visualize e.g. IGG.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param yaxis_var name of variable containing biomarker results displayed on
#'   Y-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment trt_group e.g. ARM.
#' @param loq_flag_var  name of variable containing LOQ flag e.g. LOQFL.
#' @param loq_legend `logical` whether to include LoQ legend.
#' @param unit biomarker unit label e.g. (U/L)
#' @param color_manual vector of color for trt_group
#' @param shape_manual vector of shapes (used with loq_flag_var)
#' @param box add boxes to the plot (boolean)
#' @param ylim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the y-axis
#'   if the default limits are not suitable.
#' @param facet_var variable to facet the plot by, or "None" if no faceting
#'   required.
#' @param xaxis_var variable used to group the data on the x-axis.
#' @param facet_ncol number of facets per row.  NULL = Use the default for ggplot2::facet_wrap
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size point size of tex to use.  NULL is use default size
#' @param dot_size plot dot size.
#' @param alpha dot transparency (0 = transparent, 1 = opaque)
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param hline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the ggplot2 object must also contain the columns with these variable names
#' @param hline_vars_colors ('character vector') colors for the horizontal lines defined by variables.
#' @param hline_vars_labels ('character vector') labels for the legend to the horizontal lines defined by variables.
#'
#' @author Balazs Toth
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(nestcolor)
#'
#' ADLB <- goshawk::rADLB
#' var_labels <- lapply(ADLB, function(x) attributes(x)$label)
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~
#'       paste(
#'         "W",
#'         trimws(
#'           substr(
#'             AVISIT,
#'             start = 6,
#'             stop = stringr::str_locate(AVISIT, "DAY") - 1
#'           )
#'         )
#'       ),
#'     TRUE ~ NA_character_
#'   )) %>%
#'   mutate(AVISITCDN = case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0,
#'     grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#'     TRUE ~ NA_real_
#'   )) %>%
#'   mutate(ANRLO = .5, ANRHI = 1) %>%
#'   rowwise() %>%
#'   group_by(PARAMCD) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = .5, max = 1))), LBSTRESC
#'   )) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 1, max = 1.5))), LBSTRESC
#'   )) %>%
#'   ungroup()
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#' attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#'
#' # add LLOQ and ULOQ variables
#' ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB)
#' ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
#'
#' g_boxplot(ADLB,
#'   biomarker = "CRP",
#'   param_var = "PARAMCD",
#'   yaxis_var = "AVAL",
#'   trt_group = "ARM",
#'   loq_flag_var = "LOQFL",
#'   loq_legend = FALSE,
#'   unit = "AVALU",
#'   shape_manual = c("N" = 1, "Y" = 2, "NA" = NULL),
#'   facet_var = "AVISIT",
#'   xaxis_var = "STUDYID",
#'   alpha = 0.5,
#'   rotate_xlab = TRUE,
#'   hline_arb = c(.9, 1.2),
#'   hline_arb_color = "blue",
#'   hline_arb_label = "Hori_line_label",
#'   hline_vars = c("ANRHI", "ANRLO", "ULOQN", "LLOQN"),
#'   hline_vars_colors = c("pink", "brown", "purple", "gray"),
#'   hline_vars_labels = c("A", "B", "C", "D")
#' )
g_boxplot <- function(data,
                      biomarker,
                      param_var = "PARAMCD",
                      yaxis_var,
                      trt_group,
                      xaxis_var = NULL,
                      loq_flag_var = "LOQFL",
                      loq_legend = TRUE,
                      unit = NULL,
                      color_manual = NULL,
                      shape_manual = NULL,
                      box = TRUE,
                      ylim = c(NA, NA),
                      dot_size = 2,
                      alpha = 1.0,
                      facet_ncol = NULL,
                      rotate_xlab = FALSE,
                      font_size = NULL,
                      facet_var = NULL,
                      hline_arb = numeric(0),
                      hline_arb_color = "red",
                      hline_arb_label = "Horizontal line",
                      hline_vars = character(0),
                      hline_vars_colors = "green",
                      hline_vars_labels = hline_vars) {
  if (is.null(data[[param_var]])) {
    stop(paste("param_var", param_var, "is not in data."))
  }

  if (!any(data[[param_var]] == biomarker)) {
    stop(paste("biomarker", biomarker, "is not found in param_var", param_var, "."))
  }
  checkmate::assert_flag(loq_legend)
  checkmate::assert_number(dot_size)
  checkmate::assert_numeric(ylim, len = 2)

  # filter input data
  data <- data %>%
    filter(!!sym(param_var) == biomarker)

  if (!is.null(unit)) {
    # check unit is in the dataset
    if (is.null(data[[unit]])) {
      stop(paste("unit variable", unit, "is not in data."))
    }
    # extract the most common unit
    # if there are ties, take the use alphabetic order
    tmp_unit <- data %>%
      count(!!sym(unit)) %>%
      top_n(1, n) %>%
      arrange(!!sym(unit)) %>%
      slice(1) %>%
      select(!!sym(unit)) %>%
      as.character()
    if (is.factor(data[[unit]])) {
      unit <- levels(data[[unit]])[as.numeric(tmp_unit)]
    } else {
      unit <- tmp_unit
    }
  }
  # Setup the Y axis label.  Combine the biomarker and the units (if available)
  y_axis_label <- ifelse(is.null(unit), paste(data$PARAM[1], yaxis_var, "Values"),
    ifelse(unit == "", paste(data$PARAM[1], yaxis_var, "Values"),
      paste0(data$PARAM[1], " (", unit, ") ", yaxis_var, " Values")
    )
  )
  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitle_label <- ifelse(is.null(unit), paste(data$PARAM[1], "Distribution by Treatment @ Visits"),
    ifelse(unit == "", paste(data$PARAM[1], "Distribution by Treatment @ Visits"),
      paste0(data$PARAM[1], " (", unit, ") Distribution by Treatment @ Visits")
    )
  )
  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- h_caption_loqs_label(loqs_data = data)
  # Base plot
  plot1 <- ggplot2::ggplot(data)
  # Add boxes if required
  if (box) {
    plot1 <- plot1 +
      ggplot2::geom_boxplot(
        data = data,
        ggplot2::aes_string(
          x = xaxis_var,
          y = yaxis_var,
          fill = NULL
        ),
        outlier.shape = NA,
        na.rm = TRUE
      )
  }
  # Extend is.infinite to include zero length objects.
  is_finite <- function(x) {
    if (length(x) == 0) {
      return(FALSE)
    }
    return(is.finite(x))
  }

  plot1 <- plot1 +
    ggplot2::labs(color = trt_label, x = NULL, y = y_axis_label, caption = caption_loqs_label) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(ggtitle_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = font_size, hjust = 0.5))
  # Colors supplied?  Use color_manual, otherwise default ggplot coloring.
  plot1 <- if (!is.null(color_manual)) {
    plot1 +
      ggplot2::scale_color_manual(values = color_manual, guide = ggplot2::guide_legend(order = 1))
  } else {
    plot1 +
      ggplot2::scale_color_discrete(guide = ggplot2::guide_legend(order = 1))
  }

  # Format LOQ flag symbol shape
  if (is.null(shape_manual)) {
    shape_names <- unique(data[!is.na(data[[loq_flag_var]]), ][[loq_flag_var]])
    shape_manual <- seq_along(shape_names)
    names(shape_manual) <- shape_names
  }
  # add LOQ legend conditionally
  plot1 <- if (!loq_legend) {
    plot1 + ggplot2::scale_shape_manual(values = shape_manual, name = "LoQ", guide = "none")
  } else {
    plot1 + ggplot2::scale_shape_manual(values = shape_manual, name = "LoQ", guide = ggplot2::guide_legend(order = 2))
  }

  plot1 <- plot1 +
    ggplot2::geom_jitter(
      data = data,
      ggplot2::aes_string(x = xaxis_var, y = yaxis_var, shape = loq_flag_var, color = trt_group),
      alpha = alpha, position = ggplot2::position_jitter(width = 0.1, height = 0), size = dot_size, na.rm = TRUE
    )

  # Any limits for the Y axis?
  plot1 <- plot1 + ggplot2::coord_cartesian(ylim = ylim)


  # Add facetting.
  if (!is.null(facet_var)) {
    if (facet_var != "None" & facet_var %in% names(data)) {
      if (!is_finite(facet_ncol)) facet_ncol <- 0
      if (facet_ncol >= 1) {
        plot1 <- plot1 +
          ggplot2::facet_wrap(stats::as.formula(paste0(" ~ ", facet_var)), ncol = round(facet_ncol))
      } else {
        plot1 <- plot1 +
          ggplot2::facet_wrap(stats::as.formula(paste0(" ~ ", facet_var)))
      }
    }
  }




  # Format font size
  if (is_finite(font_size)) {
    plot1 <- plot1 +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = font_size),
        axis.text.x = ggplot2::element_text(size = font_size),
        axis.title.y = ggplot2::element_text(size = font_size),
        axis.text.y = ggplot2::element_text(size = font_size),
        legend.title = ggplot2::element_text(size = font_size),
        legend.text = ggplot2::element_text(size = font_size),
        strip.text.x = ggplot2::element_text(size = font_size),
        strip.text.y = ggplot2::element_text(size = font_size)
      )
  }
  # Format x-label
  if (rotate_xlab) {
    plot1 <- plot1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  # Add horizontal line for range based on option
  plot1 + geom_axes_lines(data,
    hline_arb = hline_arb, hline_arb_color = hline_arb_color, hline_arb_label = hline_arb_label,
    hline_vars = hline_vars, hline_vars_colors = hline_vars_colors, hline_vars_labels = hline_vars_labels
  )
}
