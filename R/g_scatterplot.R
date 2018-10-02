#' Function to generate a scatter plot
#' Output rendered by teal.goshawk module \code{g_scatterplot} returns scatter plot visualiztion
#'
#' A scatter plot is a type of plot using Cartesian coordinates to display values for typically two variables or
#' for one variable at different timepoints for a set of data.
#' If the points are color-coded, one additional variable can be displayed.
#' The data are displayed as a collection of points, each having the value of one variable or timepoint determining
#' the position on the horizontal axis and the value of the other variable or timepoint determining the position 
#' on the vertical axis.
#'
#' @param label text string to used to identify plot.
#' @param data data frame with variables which will be displayed in the plot.
#'   Note that the data are expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient per visit.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param visit name of variable containing nominal visits e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xmin_scale minimum value of xaxis_var variable for selected biomarker.
#' @param xmax_scale maximum value of xaxis_var variable for selected biomarker.
#' @param ymin_scale minimum value of yaxis_var variable for selected biomarker.
#' @param ymax_scale maximum value of yaxis_var variable for selected biomarker.
#' @param color_manual vector of colors. Currently plot uses default colors and this control is set to NULL.
#' @param shape_manual vector of shapes to display LOQ values vs. non-LOQ values.
#' @param facet set layout to use facetting.
#' @param facet_var variable to use for facetting.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet TRUE.
#' @param rotate_xlab 45 degree rotation of x-axis values.
#' @param hline y-axis value to position of horizontal line.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dot_size scatter dot size.
#' @param reg_text_size regression line annotation font size.
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' 
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details This function displays a scatter plot. link to specification file \url{http://rstudio.com}
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
#' param <- c('CRP') # FOR TESTING: woud come from teal.goshawk.tm_g_scatterplot.R
#' 
#' plot1 <- g_scatterplot(label = 'Scatter Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD', 
#'            param = param,
#'            xaxis_var = 'BASE',
#'            yaxis_var = 'AVAL',
#'            trt_group = 'ARM',
#'            visit = 'AVISITCD',
#'            loq_flag_var = 'LOQFL',
#'            unit = 'AVALU',
#'            xmin_scale = 0,
#'            xmax_scale = .5,
#'            ymin_scale = 0,
#'            ymax_scale = .5,
#'            color_manual = NULL,
#'            shape_manual = c('N' = 1, 'Y' = 2, 'NA' = 0),
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            facet = TRUE,
#'            facet_var = "ARM",
#'            reg_line = TRUE,
#'            font_size = 14,
#'            dot_size = 2,
#'            reg_text_size = 3)
#' plot1 
#' 
#' }

g_scatterplot <- function(label = 'Scatter Plot',
                          data = ALB,
                          param_var = 'PARAMCD',
                          param = "CRP",
                          xaxis_var = 'BASE',
                          yaxis_var = 'AVAL',
                          trt_group = "ARM",
                          visit = "AVISITCD",
                          loq_flag_var = "LOQFL",
                          unit = "AVALU",
                          xmin_scale = 0,
                          xmax_scale = 200,
                          ymin_scale = 0,
                          ymax_scale = 200,
                          color_manual = NULL,
                          shape_manual = c('N' = 1, 'Y' = 2, 'NA' = 0),
                          facet = FALSE,
                          facet_var = "ARM",
                          reg_line = FALSE,
                          rotate_xlab = FALSE,
                          hline = NULL,
                          font_size = 12,
                          dot_size = NULL,
                          reg_text_size = 3){

# create scatter plot over time pairwise per treatment arm 
plot_data <- data %>%
  filter(eval(parse(text = param_var)) == param)

# create plot foundation
  plot1 <- ggplot2::ggplot(data = plot_data,
                   aes_string(x = xaxis_var,
                              y = yaxis_var,
                              color = trt_group)) +
    geom_point(aes_string(shape = loq_flag_var), size = dot_size, na.rm = TRUE) +
    facet_wrap(as.formula(paste0('~', visit))) +
    theme_bw() +
    scale_shape_manual(values = shape_manual, name = 'LOQ') +
    xlim(xmin_scale, xmax_scale) + ylim(ymin_scale, ymax_scale) +
    ggtitle(paste0('Biomarker ', param, ' (',  plot_data[[unit]], ')')) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(paste0('Biomarker ', xaxis_var, ' Values')) +
    ylab(paste0('Biomarker ', yaxis_var,' Values'))
    
    
# add grid faceting to foundation 
    if (facet){
      plot1 <- plot1 +
        facet_grid(as.formula(paste0(facet_var ,' ~ ', visit)))
    }

# add regression line
    if (reg_line){
      
      slope <- function(x, y) {
        ratio <- sd(x)/sd(y)
        reg <- mcr:::mc.deming(y, x, ratio)
        return(c(round(reg$b0,2),round(reg$b1,2)))
      }
      
      sub_data <- subset(plot_data, !is.na(eval(parse(text = yaxis_var))) &
                           !is.na(eval(parse(text = xaxis_var)))) %>%
        group_by_(.dots = c(trt_group, visit)) %>%
        filter(n() > 1) %>%
        mutate(intercept = slope(eval(parse(text = yaxis_var)),
                                 eval(parse(text = xaxis_var)))[1]) %>%
        mutate(slope = slope(eval(parse(text = yaxis_var)),
                             eval(parse(text = xaxis_var)))[2]) %>%
        mutate(corr = cor(eval(parse(text = yaxis_var)),
                          eval(parse(text = xaxis_var)),
                          method = "spearman",
                          use = 'complete.obs'))
      
        plot1 <- plot1 +
          geom_abline(data = sub_data,
                      aes_string(intercept = 'intercept',
                                 slope = 'slope',
                                 color = trt_group)) +
          geom_text(data = sub_data, aes(x = -Inf,
                                         y = Inf,
                                         hjust = 0,
                                         vjust = 1,
                                         label = paste0('y=',round(intercept,2),
                                                        '+',
                                                        round(slope,2),
                                                        'X', "\n",
                                                        'cor=',
                                                        round(corr,2)),
                                         color = eval(parse(text = trt_group))),
                    size = reg_text_size) +
          labs(caption = paste("Deming Regression Model"))
        
    } 
  
  # Add abline
    if (yaxis_var %in% c('AVAL','AVALL2', 'BASE', 'BASEL2', 'BASE2', 'BASE2L2')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 1)}
    
    if (yaxis_var == 'CHG') {plot1 <- plot1 + geom_abline(intercept = 0, slope = 0)}
    
    if (yaxis_var == 'PCHG') {plot1 <- plot1 + geom_abline(intercept = 100, slope = 0)}
    

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

    # Format dot size
  if (!is.null(dot_size)){
    plot1 <- plot1 +
      geom_point(aes_string(shape = loq_flag_var), size = dot_size, na.rm = TRUE)
  }
  
  # Format x-label
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Add manual color
  if (!is.null(color_manual)){
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = 'Dose')
  }
  
  # Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  plot1
  
}
