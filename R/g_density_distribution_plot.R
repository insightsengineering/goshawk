#' Function to create a density distribution plot. 
#' 
#' Default plot displays overall density facetted by visit with treatment arms and combined 
#' treatment overlaid.
#' 
#' @param label text string used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ALB.  
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xmin x-axis lower zoom limit.
#' @param xmax x-axis upper zoom limit.
#' @param color_manual vector of colors applied to treatment values.
#' @param color_comb name or hex value for combined treatment color.
#' @param facet_var variable to use for facetting.
#' @param hline y-axis value to position a horizontal line.
#' @param facet_ncol number of facets per row.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param line_size plot line thickness.
#' 
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' 
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", 
#' "C: Combination" = "Combination")
#' color_manual <-  c("150mg QD" = "#000000", "Placebo" = "#3498DB", "Combination" = "#E74C3C")
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(random.cdisc.data)
#' library(stringr)
#' 
#' ASL <- radsl(N = 20, seed = 1)
#' ALB <- radlb(ASL, visit_format = "WEEK", n_assessments = 7, seed = 2)
#' ALB <- ALB %>% 
#' mutate(AVISITCD = case_when(
#' AVISIT == "SCREENING" ~ "SCR",
#' AVISIT == "BASELINE" ~ "BL", grepl("WEEK", AVISIT) ~ paste("W",trimws(substr(AVISIT, start=6, 
#' stop=str_locate(AVISIT, "DAY")-1))),
#' TRUE ~ as.character(NA))) %>%
#' mutate(AVISITCDN = case_when(AVISITCD == "SCR" ~ -2,
#' AVISITCD == "BL" ~ 0, grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)), 
#' TRUE ~ as.numeric(NA))) %>%
#' # use ARMCD values to order treatment in visualization legend
#' mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#' ifelse(grepl("B", ARMCD), 2,
#' ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#' mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#' mutate(ARM = factor(ARM) %>% reorder(TRTORD))
#'
#' param <- c('CRP')
#' 
#' plot1 <- g_density_distribution_plot(label = 'Density Distribution Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD',
#'            param = param,
#'            xaxis_var = 'AVAL',
#'            trt_group = 'ARM',
#'            xmin = 0,
#'            xmax = 200,
#'            unit = 'AVALU',
#'            color_manual = color_manual,
#'            color_comb = "#39ff14",
#'            facet_var = 'AVISITCD',
#'            hline = NULL,
#'            facet_ncol = 2,
#'            rotate_xlab = FALSE,
#'            font_size = 10,
#'            line_size = .5)
#' plot1 
#' 
#' }
#' 

g_density_distribution_plot <- function(label = 'Density Distribution Plot',
                                        data,
                                        param_var = "PARAMCD",
                                        param = "CRP",
                                        xaxis_var = "AVAL",
                                        trt_group = "ARM",
                                        unit = "AVALU",
                                        xmin = NA,
                                        xmax = NA,
                                        color_manual = NULL,
                                        color_comb = "#39ff14",
                                        facet_var = "AVISITCD",
                                        hline = NULL,
                                        facet_ncol = 2,
                                        rotate_xlab = FALSE,
                                        font_size = 12,
                                        line_size = 2){
  
  plot_data <- data %>%
    filter(eval(parse(text = param_var)) == param)
  
  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitleLabel <- ifelse(is.null(unit), paste(plot_data$PARAM, "Density: Combined Treatment (Comb.) & by Treatment @ Visits"), 
                         ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, "Density: Combined Treatment (Comb.) & by Treatment @ Visits"), 
                                paste0(plot_data$PARAM," (", plot_data[[unit]],") Density: Combined Treatment (Comb.) & by Treatment @ Visits"))
  )
  
  # Setup the x-axis label.  Combine the biomarker and the units (if available)
  xaxisLabel <- ifelse(is.null(unit), paste(plot_data$PARAM, xaxis_var, "Values"), 
                       ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, xaxis_var, "Values"), 
                              paste0(plot_data$PARAM," (", plot_data[[unit]],") ", xaxis_var, " Values"))
  )
  
  # Setup legend label
  if(is.null(attr(data[[trt_group]], "label"))){
    trtLabel <- "Dose"
  } else {
    trtLabel <- attr(data[[trt_group]], "label")
  }
  
  plot1 <- ggplot(plot_data) +
    geom_density(aes_string(x = xaxis_var, colour = trt_group), size = line_size) +
    geom_density(aes(x = eval(parse(text = xaxis_var)), linetype = 'Comb.'), color = color_comb, size = line_size) + 
    scale_linetype_manual(name = "Combined Dose", values = c(Comb.="solid", per_dose="solid")) +
    coord_cartesian(xlim = c(xmin, xmax)) +
    facet_wrap(as.formula(paste0('~', facet_var)), ncol = facet_ncol) +
    theme_bw() +
    ggtitle(ggtitleLabel) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(paste(xaxisLabel)) +
    ylab(paste("Density"))
  
  # Format treatment color and label legend
  if (!is.null(color_manual)){
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = trtLabel)
  }
  
  # Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  # Format font size
  if (!is.null(font_size)){
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
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  plot1
  
}
