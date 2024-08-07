#' Function to create a correlation plot.
#'
#' Default plot displays correlation facetted by visit with color attributed treatment arms and
#' symbol attributed `LOQ` values.
#'
#' @param label text string to used to identify plot.
#' @param data `ADaM` structured analysis laboratory data frame e.g. `ADLB`.
#' @param param_var name of variable containing biomarker codes e.g. `PARAMCD`.
#' @param xaxis_param x-axis biomarker to visualize e.g. `IGG`.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. `BASE`.
#' @param xvar x-axis analysis variable from transposed data set.
#' @param yaxis_param y-axis biomarker to visualize e.g. `IGG`.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axis.g. `AVAL`.
#' @param yvar y-axis analysis variable from transposed data set.
#' @param trt_group name of variable representing treatment group e.g. `ARM`.
#' @param visit name of variable containing nominal visits e.g. `AVISITCD`.
#' @param loq_flag_var  name of variable containing `LOQ` flag e.g. `LOQFL_COMB`.
#' @param visit_facet visit facet toggle.
#' @param loq_legend `logical` whether to include `LoQ` legend.
#' @param unit name of variable containing biomarker unit e.g. `AVALU`.
#' @param xlim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the x-axis
#'   if the default limits are not suitable.
#' @param ylim ('numeric vector') optional, a vector of length 2 to specify the minimum and maximum of the y-axis
#'   if the default limits are not suitable.
#' @param title_text plot title.
#' @param xaxis_lab x-axis label.
#' @param yaxis_lab y-axis label.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to `LOQ` values. (used with `loq_flag_var`).
#' @param facet_ncol number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for treatment facetting.
#' @param reg_line include regression line and annotations for slope and coefficient.
#' Use with facet = TRUE.
#' @param hline_arb ('numeric vector') value identifying intercept for arbitrary horizontal lines.
#' @param hline_arb_color ('character vector') optional, color for the arbitrary horizontal lines.
#' @param hline_arb_label ('character vector') optional, label for the legend to the arbitrary horizontal lines.
#' @param hline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the `ggplot2` object must also contain the columns with these variable names
#' @param hline_vars_colors ('character vector') colors for the horizontal lines defined by variables.
#' @param hline_vars_labels ('character vector') labels for the legend to the horizontal lines defined by variables.
#' @param vline_arb ('numeric vector') value identifying intercept for arbitrary vertical lines.
#' @param vline_arb_color ('character vector') optional, color for the arbitrary vertical lines.
#' @param vline_arb_label ('character vector') optional, label for the legend to the arbitrary vertical lines.
#' @param vline_vars ('character vector'), names of variables `(ANR*)` or values `(*LOQ)` identifying intercept values.
#'   The data inside of the `ggplot2` object must also contain the columns with these variable names
#' @param vline_vars_colors ('character vector') colors for the vertical lines defined by variables.
#' @param vline_vars_labels ('character vector') labels for the legend to the vertical lines defined by variables.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses `deming` model.
#'
#' @export
#'
#' @examples
#' # Example using ADaM structure analysis dataset.
#'
#' library(stringr)
#' library(tidyr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list(
#'   "A: Drug X" = "150mg QD",
#'   "B: Placebo" = "Placebo",
#'   "C: Combination" = "Combination"
#' )
#' color_manual <- c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' # assign LOQ flag symbols: circles for "N" and triangles for "Y", squares for "NA"
#' shape_manual <- c("N" = 1, "Y" = 2, "NA" = 0)
#'
#' ADLB <- rADLB
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
#'     reorder(TRTORD)) %>%
#'   mutate(
#'     ANRHI = case_when(
#'       PARAMCD == "ALT" ~ 60,
#'       PARAMCD == "CRP" ~ 70,
#'       PARAMCD == "IGA" ~ 80,
#'       TRUE ~ NA_real_
#'     ),
#'     ANRLO = case_when(
#'       PARAMCD == "ALT" ~ 20,
#'       PARAMCD == "CRP" ~ 30,
#'       PARAMCD == "IGA" ~ 40,
#'       TRUE ~ NA_real_
#'     )
#'   ) %>%
#'   rowwise() %>%
#'   group_by(PARAMCD) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste("<", round(runif(1, min = 25, max = 30))), LBSTRESC
#'   )) %>%
#'   mutate(LBSTRESC = ifelse(
#'     USUBJID %in% sample(USUBJID, 1, replace = TRUE),
#'     paste(">", round(runif(1, min = 70, max = 75))), LBSTRESC
#'   )) %>%
#'   ungroup()
#' attr(ADLB[["ARM"]], "label") <- var_labels[["ARM"]]
#' attr(ADLB[["ANRHI"]], "label") <- "Analysis Normal Range Upper Limit"
#' attr(ADLB[["ANRLO"]], "label") <- "Analysis Normal Range Lower Limit"
#'
#' # add LLOQ and ULOQ variables
#' ADLB_LOQS <- goshawk:::h_identify_loq_values(ADLB, flag_var = "LOQFL")
#' ADLB <- left_join(ADLB, ADLB_LOQS, by = "PARAM")
#'
#' # given the 2 param and 2 analysis vars we need to transform the data
#' plot_data_t1 <- ADLB %>%
#'   gather(
#'     ANLVARS, ANLVALS, PARAM, LBSTRESC, BASE2, BASE, AVAL, BASE, LOQFL,
#'     ANRHI, ANRLO, ULOQN, LLOQN
#'   ) %>%
#'   mutate(ANL.PARAM = ifelse(ANLVARS %in% c("PARAM", "LBSTRESC", "LOQFL"),
#'     paste0(ANLVARS, "_", PARAMCD),
#'     paste0(ANLVARS, ".", PARAMCD)
#'   )) %>%
#'   select(USUBJID, ARM, ARMCD, AVISITN, AVISITCD, ANL.PARAM, ANLVALS) %>%
#'   spread(ANL.PARAM, ANLVALS)
#'
#' # the transformed analysis value variables are character and need to be converted to numeric for
#' # ggplot
#' # remove records where either of the analysis variables are NA since they will not appear on the
#' # plot and will ensure that LOQFL = NA level is removed
#' plot_data_t2 <- plot_data_t1 %>%
#'   filter(!is.na(BASE.CRP) & !is.na(AVAL.ALT)) %>%
#'   mutate_at(vars(contains(".")), as.numeric) %>%
#'   mutate(
#'     LOQFL_COMB = ifelse(LOQFL_CRP == "Y" | LOQFL_ALT == "Y", "Y", "N")
#'   )
#'
#' g_correlationplot(
#'   label = "Correlation Plot",
#'   data = plot_data_t2,
#'   param_var = "PARAMCD",
#'   xaxis_param = c("CRP"),
#'   xaxis_var = "AVAL",
#'   xvar = "AVAL.CRP",
#'   yaxis_param = c("ALT"),
#'   yaxis_var = "BASE",
#'   yvar = "BASE.ALT",
#'   trt_group = "ARM",
#'   visit = "AVISITCD",
#'   visit_facet = TRUE,
#'   loq_legend = TRUE,
#'   unit = "AVALU",
#'   title_text = "Correlation of ALT to CRP",
#'   xaxis_lab = "CRP",
#'   yaxis_lab = "ALT",
#'   color_manual = color_manual,
#'   shape_manual = shape_manual,
#'   facet_ncol = 4,
#'   facet = FALSE,
#'   facet_var = "ARM",
#'   reg_line = FALSE,
#'   hline_arb = c(15, 25),
#'   hline_arb_color = c("gray", "green"),
#'   hline_arb_label = "Hori_line_label",
#'   vline_arb = c(.5, 1),
#'   vline_arb_color = c("red", "black"),
#'   vline_arb_label = c("Vertical Line A", "Vertical Line B"),
#'   hline_vars = c("ANRHI.ALT", "ANRLO.ALT", "ULOQN.ALT", "LLOQN.ALT"),
#'   hline_vars_colors = c("green", "blue", "purple", "cyan"),
#'   hline_vars_labels = c("ANRHI ALT Label", "ANRLO ALT Label", "ULOQN ALT Label", "LLOQN ALT Label"),
#'   vline_vars = c("ANRHI.CRP", "ANRLO.CRP", "ULOQN.CRP", "LLOQN.CRP"),
#'   vline_vars_colors = c("yellow", "orange", "brown", "gold"),
#'   vline_vars_labels = c("ANRHI CRP Label", "ANRLO CRP Label", "ULOQN CRP Label", "LLOQN CRP Label"),
#'   rotate_xlab = FALSE,
#'   font_size = 14,
#'   dot_size = 2,
#'   reg_text_size = 3
#' )
g_correlationplot <- function(label = "Correlation Plot",
                              data,
                              param_var = "PARAMCD",
                              xaxis_param = "CRP",
                              xaxis_var = "BASE",
                              xvar,
                              yaxis_param = "IGG",
                              yaxis_var = "AVAL",
                              yvar,
                              trt_group = "ARM",
                              visit = "AVISITCD",
                              loq_flag_var = "LOQFL_COMB",
                              visit_facet = TRUE,
                              loq_legend = TRUE,
                              unit = "AVALU",
                              xlim = c(NA, NA),
                              ylim = c(NA, NA),
                              title_text = title_text,
                              xaxis_lab = xaxis_lab,
                              yaxis_lab = yaxis_lab,
                              color_manual = NULL,
                              shape_manual = NULL,
                              facet_ncol = 2,
                              facet = FALSE,
                              facet_var = "ARM",
                              reg_line = FALSE,
                              hline_arb = numeric(0),
                              hline_arb_color = "red",
                              hline_arb_label = "Horizontal line",
                              hline_vars = character(0),
                              hline_vars_colors = "green",
                              hline_vars_labels = hline_vars,
                              vline_arb = numeric(0),
                              vline_arb_color = "red",
                              vline_arb_label = "Vertical line",
                              vline_vars = character(0),
                              vline_vars_colors = "green",
                              vline_vars_labels = vline_vars,
                              rotate_xlab = FALSE,
                              font_size = 12,
                              dot_size = 2,
                              reg_text_size = 3) {
  checkmate::assert_flag(loq_legend)
  checkmate::assert_number(dot_size, lower = 1)
  checkmate::assert_numeric(xlim, len = 2)
  checkmate::assert_numeric(ylim, len = 2)

  # create correlation plot over time pairwise per treatment arm
  plot_data <- data

  # identify param and lbstresc combinations in transposed data variable name
  t_param_var_x <- paste("PARAM", xaxis_param, sep = "_")
  t_lbstresc_var_x <- paste("LBSTRESC", xaxis_param, sep = "_")
  t_param_var_y <- paste("PARAM", yaxis_param, sep = "_")
  t_lbstresc_var_y <- paste("LBSTRESC", yaxis_param, sep = "_")

  xaxis_param_loqs_data <- data %>%
    mutate(
      PARAM = !!sym(t_param_var_x), LBSTRESC = !!sym(t_lbstresc_var_x),
      sym(loq_flag_var) == !!sym(loq_flag_var)
    ) %>%
    select("PARAM", "LBSTRESC", loq_flag_var)

  yaxis_param_loqs_data <- data %>%
    mutate(
      PARAM = !!sym(t_param_var_y), LBSTRESC = !!sym(t_lbstresc_var_y),
      sym(loq_flag_var) == !!sym(loq_flag_var)
    ) %>%
    select("PARAM", "LBSTRESC", loq_flag_var)

  # add footnote to identify xaxis assay LLOQ and ULOQ values pulled from data
  caption_loqs_label_x <- h_caption_loqs_label(loqs_data = xaxis_param_loqs_data, flag_var = loq_flag_var)
  caption_loqs_label_y <- h_caption_loqs_label(loqs_data = yaxis_param_loqs_data, flag_var = loq_flag_var)
  caption_loqs_label_x_y <- paste0(union(caption_loqs_label_x, caption_loqs_label_y), collapse = "\n")

  # Setup legend label
  trt_label <- `if`(is.null(attr(data[[trt_group]], "label")), "Dose", attr(data[[trt_group]], "label"))

  # create plot foundation - titles and axes labels are defined in
  # teal.goshawk.tm_g_correlationplot.R
  plot1 <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = !!sym(xvar),
      y = !!sym(yvar),
      color = !!sym(trt_group)
    )
  ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw() +
    ggplot2::labs(caption = caption_loqs_label_x_y) +
    ggplot2::ggtitle(title_text) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = font_size, hjust = 0.5)) +
    ggplot2::xlab(xaxis_lab) +
    ggplot2::ylab(yaxis_lab)

  # conditionally facet
  plot1 <- if (visit_facet && facet) {
    plot1 +
      ggplot2::facet_grid(stats::as.formula(paste0(facet_var, " ~ ", visit)))
  } else if (visit_facet) {
    plot1 +
      ggplot2::facet_wrap(stats::as.formula(paste0(" ~ ", visit)), ncol = facet_ncol)
  } else if (facet) {
    plot1 +
      ggplot2::facet_wrap(stats::as.formula(paste0(" ~ ", facet_var)), ncol = facet_ncol)
  } else {
    plot1
  }

  plot1 <- plot1 +
    ggplot2::geom_point(ggplot2::aes(shape = !!sym(loq_flag_var)), size = dot_size, na.rm = TRUE)

  # add regression line
  if (reg_line) {
    slope <- function(x, y) {
      ratio <- stats::sd(x) / stats::sd(y)
      if (!is.na(ratio) && ratio > 0) {
        reg <- mc.deming(y, x, ratio)
        # return the evaluation of the ratio condition as third value in numeric vector to control
        # downstream processing
        return(c(round(reg$b0, 2), round(reg$b1, 2), !is.na(ratio) & ratio > 0))
      }
      # if ratio condition is not met then assign NA to vector so that NULL condition does not throw
      # the error below
      return(as.numeric(c(NA, NA, NA)))
    }

    sub_data <- filter(plot_data, !is.na(!!sym(yvar)) & !is.na(!!sym(xvar))) %>%
      group_by(!!sym(trt_group), !!sym(visit)) %>%
      mutate(intercept = slope(!!sym(yvar), !!sym(xvar))[1]) %>%
      mutate(slope = slope(!!sym(yvar), !!sym(xvar))[2]) %>%
      mutate(corr = ifelse(
        slope(!!sym(yvar), !!sym(xvar))[3],
        stats::cor(!!sym(yvar), !!sym(xvar), method = "spearman", use = "complete.obs"),
        NA
      ))
    plot1 <- plot1 +
      ggplot2::geom_abline(
        data = filter(sub_data, row_number() == 1), # only need to return 1 row within group_by
        ggplot2::aes(intercept = .data$intercept, slope = .data$slope, color = !!sym(trt_group))
      ) +
      ggplot2::geom_text(
        data = filter(sub_data, row_number() == 1),
        ggplot2::aes_(
          x = -Inf,
          y = Inf,
          hjust = 0,
          vjust = 1,
          label = ~ ifelse(
            !is.na(intercept) & !is.na(slope) & !is.na(corr),
            sprintf("y = %.2f+%.2fX\ncor = %.2f", intercept, slope, corr),
            paste0("Insufficient Data For Regression")
          ),
          color = sym(trt_group)
        ),
        size = reg_text_size,
        show.legend = FALSE
      ) +
      ggplot2::labs(caption = paste0(
        "Deming Regression Model, Spearman Correlation Method.\n",
        caption_loqs_label_x_y
      ))
  }
  # Format font size
  if (!is.null(font_size)) {
    plot1 <- plot1 + ggplot2::theme(
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
  # Format treatment color
  plot1 <- if (!is.null(color_manual)) {
    plot1 +
      ggplot2::scale_color_manual(values = color_manual, name = trt_label, guide = ggplot2::guide_legend(order = 1))
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
  # Format x-label
  if (rotate_xlab) {
    plot1 <- plot1 +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  plot1 + geom_axes_lines(
    plot_data,
    hline_arb = hline_arb, hline_arb_color = hline_arb_color, hline_arb_label = hline_arb_label,
    hline_vars = hline_vars, hline_vars_colors = hline_vars_colors, hline_vars_labels = hline_vars_labels,
    vline_arb = vline_arb, vline_arb_color = vline_arb_color, vline_arb_label = vline_arb_label,
    vline_vars = vline_vars, vline_vars_colors = vline_vars_colors, vline_vars_labels = vline_vars_labels
  )
}
