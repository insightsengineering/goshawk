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
#' @param data data frame with variables which will be displayed in the plot.
#' @param biomarker biomarker to visualize e.g. IGG.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param yaxis_var name of variable containing biomarker results displayed on
#'   Y-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment trt_group e.g. ARM.
#' @param loq_flag_var  name of variable containing LOQ flag e.g. LOQFL.
#' @param loq_legend `logical` whether to include LoQ legend.
#' @param unit biomarker unit label e.g. (U/L)
#' @param color_manual vector of colour for trt_group
#' @param shape_manual vector of shapes (used with loq_flag_var)
#' @param box add boxes to the plot (boolean)
#' @param ymin_scale minimum value for the Y axis
#' @param ymax_scale maximum value for the Y axis
#' @param facet_var variable to facet the plot by, or "None" if no faceting
#'   required.
#' @param xaxis_var variable used to group the data on the x-axis.
#' @param facet_ncol number of facets per row.  NULL = Use the default for facet_wrap
#' @param hline y-axis value to position a horizontal line.  NULL = No line
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size point size of tex to use.  NULL is use default size
#' @param dot_size plot dot size.
#' @param alpha dot transparency (0 = transparent, 1 = opaque)
#'
#' @importFrom utils.nest stop_if_not if_null
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
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#'
#' g_boxplot(ADLB,
#'           biomarker = "CRP",
#'           param_var = "PARAMCD",
#'           yaxis_var = "AVAL",
#'           trt_group = "ARM",
#'           loq_flag_var = "LOQFL",
#'           loq_legend = FALSE,
#'           unit = "AVALU",
#'           shape_manual = c("N" = 1, "Y" = 2, "NA" = NULL),
#'           hline = NULL,
#'           facet_var = "AVISIT",
#'           xaxis_var = "STUDYID",
#'           alpha = 0.5,
#'           rotate_xlab = TRUE
#'           )
#'
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
                      ymax_scale = NULL,
                      ymin_scale = NULL,
                      dot_size = 2,
                      alpha = 1.0,
                      facet_ncol = NULL,
                      hline = NULL,
                      rotate_xlab = FALSE,
                      font_size = NULL,
                      facet_var = NULL
) {
  stop_if_not(list(!is.null(data[[param_var]]), paste("param_var", param_var, "is not in data.")))
  stop_if_not(list(
    any(data[[param_var]] == biomarker), paste("biomarker", biomarker, "is not found in param_var", param_var, ".")))
  stop_if_not(list(is_logical_single(loq_legend), "loq_legend must be a logical scalar."))
  stop_if_not(list(is_numeric_single(dot_size), "dot_size must be numeric."))

  # filter input data
  data <- data %>%
    filter(!!sym(param_var) == biomarker)
  if (!is.null(unit)) {
    # check unit is in the dataset
    stop_if_not(list(!is.null(data[[unit]]), paste("unit variable", unit, "is not in data.")))
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
                                paste0(data$PARAM[1], " (", unit, ") ", yaxis_var, " Values"))
  )
  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitle_label <- ifelse(is.null(unit), paste(data$PARAM[1], "Distribution by Treatment @ Visits"),
                          ifelse(unit == "", paste(data$PARAM[1], "Distribution by Treatment @ Visits"),
                                 paste0(data$PARAM[1], " (", unit, ") Distribution by Treatment @ Visits"))
  )
  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- h_caption_loqs_label(loqs_data = data)
  # Base plot
  plot1 <-  ggplot()
  # Add boxes if required
  if (box) {
    plot1 <- plot1 +
      geom_boxplot(
        data = data,
        aes_string(
          x = xaxis_var,
          y = yaxis_var,
          fill = NULL),
        outlier.shape = NA,
        na.rm = TRUE)
  }
  # Extend is.infinite to include zero length objects.
  is_finite <- function(x) {
    if (length(x) == 0) return(FALSE)
    return(is.finite(x))
  }
  # Add horizontal line
  if (is_finite(hline)) {
    plot1 <- plot1 +
      geom_hline(yintercept = hline, color = "red", linetype = "dashed", size = 0.5)
  }
  plot1 <- plot1 +
    labs(color = trt_label, x = NULL, y = y_axis_label, caption = caption_loqs_label) +
    theme_bw() +
    ggtitle(ggtitle_label) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5))
  # Colors supplied?  Use color_manual, otherwise default ggplot coloring.
  if (!is.null(color_manual)) {
    cols <- color_manual
    plot1 <- plot1 +
      scale_color_manual(values = cols) +
      scale_fill_manual(values = cols)
  }


  # Format LOQ flag symbol shape
  if (is.null(shape_manual)) {
    shape_names <- unique(data[!is.na(data[[loq_flag_var]]), ][[loq_flag_var]])
    shape_manual <- seq_along(shape_names)
    names(shape_manual) <- shape_names
  }
  # add LOQ legend conditionally
  plot1 <- if (!loq_legend) {
    plot1 + scale_shape_manual(values = shape_manual, name = "LoQ", guide = "none")
  } else {
    plot1 + scale_shape_manual(values = shape_manual, name = "LoQ")
  }

  plot1 <- plot1 +
    geom_jitter(
      data = data,
      aes_string(x = xaxis_var, y = yaxis_var, shape = loq_flag_var, color = trt_group),
      alpha = alpha, position = position_jitter(width = 0.1, height = 0), size = dot_size, na.rm = TRUE
    )

  # Any limits for the Y axis?
  if (!is.null(ymin_scale) & !is.null(ymax_scale)) {
    plot1 <- plot1 + coord_cartesian(ylim = c(ymin_scale, ymax_scale))
  }

  # Add facetting.
  if (!is.null(facet_var)) {
    if (facet_var != "None" & facet_var %in% names(data)) {
      if (!is_finite(facet_ncol)) facet_ncol <- 0
      if (facet_ncol >= 1) {
        plot1 <- plot1 +
          facet_wrap(as.formula(paste0(" ~ ", facet_var)), ncol = round(facet_ncol))
      } else {
        plot1 <- plot1 +
          facet_wrap(as.formula(paste0(" ~ ", facet_var)))
      }
    }
  }

  # Format font size
  if (is_finite(font_size)) {
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }
  # Format x-label
  if (rotate_xlab) {
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plot1)
}
