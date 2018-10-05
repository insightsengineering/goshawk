#' Function to generate a density distribution plot
#' Output rendered by teal.goshawk module \code{g_density_distribution_plot} returns
#' distribution overlay plot visualiztion
#'
#' This function displays an overall density plot with treatment arms overlaid.
#'
#' @param label text string to used to identify plot.
#' @param data data frame with variables which will be displayed in the plot.
#'   Note that the data are expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient per visit.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param color_manual vector of treatment colors. assigned values in app.R otherwise uses default colors.
#' @param hline y-axis value to position of horizontal line.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param facet_var variable to use for facetting.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param line_size plot line thickness.
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' 
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details This function displays an overall density plot with treatment arms overlaid. link to specification file \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using analysis dataset for example ASL or ADSL,
#' # ALB points to biomarker data stored in a typical LB structure. for example ALB or ADLB.
#' 
#' # need a test data set created using random.cdisc.data.
#' # example call uses expects ALB structure 
#'
#' param <- c('CRP') # FOR TESTING: woud come from teal.goshawk.tm_g_density_distribution_plot.R
#' 
#' plot1 <- g_density_distribution_plot(label = 'Density Distribution Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD',
#'            param = param,
#'            xaxis_var = 'AVAL',
#'            trt_group = 'ARM',
#'            unit = 'AVALU',
#'            color_manual = color_manual,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            facet_var = 'AVISITCD',
#'            font_size = 10,
#'            line_size = .5)
#' plot1 
#' 
#' }
# 

g_density_distribution_plot <- function(label = 'Density Distribution Plot',
                                data = ALB,
                                param_var = "PARAMCD",
                                param = "CRP",
                                xaxis_var = "AVAL",
                                trt_group = "ARM",
                                unit = "AVALU",
                                xmin_scale = NULL,
                                xmax_scale = NULL,
                                color_manual = NULL,
                                hline = NULL,
                                rotate_xlab = FALSE,
                                facet_var = "AVISITCD",
                                font_size = 12,
                                line_size = 2){

  plot_data <- data %>%
    filter(eval(parse(text = param_var)) == param)
  
  plot1 <- ggplot(plot_data) +
    geom_density(aes_string(x = xaxis_var, colour = trt_group), size = line_size) +
    geom_density(aes(x = eval(parse(text = xaxis_var)), linetype = 'Comb.'), color = '#ffbb52', size = line_size, ) + 
    scale_linetype_manual(name = "Combined Dose", values = c(Comb.="solid", per_dose="solid")) +
    facet_wrap(as.formula(paste0('~', facet_var))) +
    theme_bw() +
    ggtitle(paste(plot_data$PARAM, "(",  plot_data[[unit]], ") Density: Combined Treatment (Comb.) & by Treatment @ Visits")) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(paste(plot_data$PARAM, xaxis_var, "Values")) +
    ylab(paste("Density"))

  # Dynamic x-axis range
  if (!is.null(xmin_scale) & !is.null(xmax_scale)) {
    plot1 <- plot1 + xlim(xmin_scale, xmax_scale) 
  }
  
  # Format treatment color
  if (!is.null(color_manual)){
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = 'Dose')
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
