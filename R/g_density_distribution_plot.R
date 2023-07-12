#' Function to create a density distribution plot.
#'
#' Default plot displays overall density facetted by visit with treatment arms and combined
#' treatment overlaid.
#'
#' @param label text string used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ADLB.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xlim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the x-axis
#'   if the default limits are not suitable.
#' @param ylim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the y-axis
#'   if the default limits are not suitable.
#' @param color_manual vector of colors applied to treatment values.
#' @param color_comb name or hex value for combined treatment color.
#' @param comb_line display combined treatment line toggle.
#' @param facet_var variable to use for facetting.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param facet_ncol number of facets per row.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' @param rug_plot should a rug plot be displayed under the density plot. Note this
#'   option is most useful if the data only contains a single treatment group.
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @export
#'
#' @examples
#'
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", "C: Combination" = "Combination"
#' )
#' color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
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
#'             stop = str_locate(AVISIT, "DAY") - 1
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
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)
#'     )
#'   )) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>%
#'     reorder(TRTORD))
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#'
#' g_density_distribution_plot(
#'   label = "Density Distribution Plot",
#'   data = ADLB,
#'   param_var = "PARAMCD",
#'   param = c("CRP"),
#'   xaxis_var = "AVAL",
#'   unit = "AVALU",
#'   color_manual = color_manual,
#'   color_comb = "#39ff14",
#'   comb_line = FALSE,
#'   facet_var = "AVISITCD",
#'   hline_arb = 1.75,
#'   hline_arb_color = "black",
#'   hline_arb_label = "Horizontal Line A",
#'   facet_ncol = 2,
#'   rotate_xlab = FALSE,
#'   font_size = 10,
#'   line_size = .5
#' )
g_density_distribution_plot <- function(label = "Density Distribution Plot",
                                        data,
                                        param_var = "PARAMCD",
                                        param = "CRP",
                                        xaxis_var = "AVAL",
                                        trt_group = "ARM",
                                        unit = "AVALU",
                                        xlim = c(NA, NA),
                                        ylim = c(NA, NA),
                                        color_manual = NULL,
                                        color_comb = "#39ff14",
                                        comb_line = TRUE,
                                        facet_var = "AVISITCD",
                                        hline_arb = character(0),
                                        hline_arb_color = "red",
                                        hline_arb_label = "Horizontal line",
                                        facet_ncol = 2,
                                        rotate_xlab = FALSE,
                                        font_size = 12,
                                        line_size = 2,
                                        rug_plot = FALSE) {
  checkmate::assert_numeric(xlim, len = 2)
  checkmate::assert_numeric(ylim, len = 2)

  plot_data <- data %>%
    filter(!!sym(param_var) == param)

  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitle_label <- ifelse(
    is.null(unit),
    paste(plot_data$PARAM, "Density: Combined Treatment (Comb.) & by Treatment @ Visits"),
    ifelse(
      plot_data[[unit]] == "",
      paste(plot_data$PARAM, "Density: Combined Treatment (Comb.) & by Treatment @ Visits"),
      paste0(plot_data$PARAM, " (", plot_data[[unit]], ") Density: Combined Treatment (Comb.) & by Treatment @ Visits")
    )
  )

  # Setup the x-axis label.  Combine the biomarker and the units (if available)
  x_axis_label <- ifelse(
    is.null(unit),
    paste(plot_data$PARAM, xaxis_var, "Values"),
    ifelse(
      plot_data[[unit]] == "",
      paste(plot_data$PARAM, xaxis_var, "Values"),
      paste0(plot_data$PARAM, " (", plot_data[[unit]], ") ", xaxis_var, " Values")
    )
  )

  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  if (comb_line) {
    plot_data <- dplyr::bind_rows(
      plot_data,
      plot_data %>%
        dplyr::mutate(!!sym(trt_group) := "Combined Dose")
    )
  }

  color_manual <- if (is.null(color_manual)) {
    group_names <- unique(plot_data[[trt_group]])
    color_values <- seq_along(group_names)
    if (!is.null(getOption("ggplot2.discrete.colour"))) {
      color_values <- getOption("ggplot2.discrete.colour")[color_values]
    }
    names(color_values) <- group_names
    color_values
  } else {
    color_manual
  }

  if (comb_line) {
    if (!is.null(color_comb)) {
      color_manual["Combined Dose"] <- color_comb
    } else if (!"Combined Dose" %in% names(color_manual)) {
      color_manual["Combined Dose"] <- length(color_manual) + 1
    }
  }

  # Add footnote to identify LLOQ and ULOQ values pulled from data
  caption_loqs_label <- h_caption_loqs_label(loqs_data = plot_data)

  plot1 <- ggplot2::ggplot(plot_data) +
    ggplot2::stat_density(
      ggplot2::aes_string(x = xaxis_var, colour = trt_group),
      size = line_size,
      geom = "line",
      position = "identity"
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::facet_wrap(stats::as.formula(paste0(" ~ ", facet_var)), ncol = facet_ncol) +
    ggplot2::labs(caption = caption_loqs_label) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(ggtitle_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = font_size, hjust = 0.5)) +
    ggplot2::xlab(paste(x_axis_label)) +
    ggplot2::ylab(paste("Density")) +
    ggplot2::scale_color_manual(values = color_manual, name = trt_label, guide = ggplot2::guide_legend(order = 1))

  if (rug_plot) {
    plot1 <- plot1 +
      ggplot2::geom_rug(ggplot2::aes(x = !!sym(xaxis_var), colour = !!sym(trt_group)))
  }

  # Format font size
  if (!is.null(font_size)) {
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

  # Add horizontal line
  plot1 + geom_axes_lines(
    plot_data,
    hline_arb = hline_arb,
    hline_arb_color = hline_arb_color,
    hline_arb_label = hline_arb_label
  )
}
