#' Function to create a scatter plot.
#'
#' Default plot displays scatter facetted by visit with color attributed treatment arms and symbol attributed LOQ values.
#'
#' @param label text string to used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ALB.  
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param visit name of variable containing nominal visits e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for facetting beyond visit.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet = TRUE.
#' @param hline y-axis value to position a horizontal line.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
#' @import mcr
#' 
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses deming model.
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#' 
#' param <- c('ADIGG') # FOR TESTING: woud come from teal.goshawk.tm_g_scatterplot.R
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
#'            color_manual = color_manual,
#'            shape_manual = shape_manual,
#'            facet = TRUE,
#'            facet_var = "ARM",
#'            reg_line = TRUE,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            font_size = 14,
#'            dot_size = 2,
#'            reg_text_size = 3)
#' plot1 
#' 
#' }
#' 

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
                          color_manual = NULL,
                          shape_manual = NULL,
                          facet = FALSE,
                          facet_var = "ARM",
                          reg_line = FALSE,
                          hline = NULL,
                          rotate_xlab = FALSE,
                          font_size = 12,
                          dot_size = NULL,
                          reg_text_size = 3){

# create scatter plot over time pairwise per treatment arm 
plot_data <- data %>%
  filter(eval(parse(text = param_var)) == param)

# Setup the ggtitle label.  Combine the biomarker and the units (if available)
ggtitleLabel <- ifelse(is.null(unit), paste0(plot_data$PARAM, "@ Visits"), 
                       ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, "@ Visits"), 
                              paste0(plot_data$PARAM," (", plot_data[[unit]],") @ Visits"))
)

# Setup the x-axis label.  Combine the biomarker and the units (if available)
xaxisLabel <- ifelse(is.null(unit), paste(plot_data$PARAM, xaxis_var, "Values"), 
                     ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, xaxis_var, "Values"), 
                            paste0(plot_data$PARAM," (", plot_data[[unit]],") ", xaxis_var, " Values"))
)

# Setup the y-axis label.  Combine the biomarker and the units (if available)
yaxisLabel <- ifelse(is.null(unit), paste(plot_data$PARAM, yaxis_var, "Values"), 
                     ifelse(plot_data[[unit]] == "", paste(plot_data$PARAM, yaxis_var, "Values"), 
                            paste0(plot_data$PARAM," (", plot_data[[unit]],") ", yaxis_var, " Values"))
)

# create plot foundation
  plot1 <- ggplot2::ggplot(data = plot_data,
                   aes_string(x = xaxis_var,
                              y = yaxis_var,
                              color = trt_group)) +
    geom_point(aes_string(shape = loq_flag_var), size = dot_size, na.rm = TRUE) +
    facet_wrap(as.formula(paste0('~', visit))) +
    theme_bw() +
    ggtitle(ggtitleLabel) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(xaxisLabel) +
    ylab(yaxisLabel)
    
    
# add grid faceting to foundation 
    if (facet){
      plot1 <- plot1 +
        facet_grid(as.formula(paste0(facet_var ,' ~ ', visit)))
    }

# add regression line
    if (reg_line){
      slope <- function(x, y) {
        ratio <- sd(x)/sd(y)
        if (!is.na(ratio) & ratio > 0){
          reg <- mcr:::mc.deming(y, x, ratio)
          # return the evaluation of the ratio condition as third value in numeric vector for conttroling downstream processing
          return(c(round(reg$b0,2), round(reg$b1,2), !is.na(ratio) & ratio > 0))
        }
        # if ratio condition is not met then assign NA to returned vector so that NULL condition does not throw error below
        return(as.numeric(c(NA, NA, NA)))
      }
      
      sub_data <- subset(plot_data, !is.na(eval(parse(text = yaxis_var))) &
                           !is.na(eval(parse(text = xaxis_var)))) %>%
        group_by_(.dots = c(trt_group, visit)) %>%
        mutate(intercept =  slope(eval(parse(text = yaxis_var)),
                                  eval(parse(text = xaxis_var)))[1]) %>%
        mutate(slope = slope(eval(parse(text = yaxis_var)),
                             eval(parse(text = xaxis_var)))[2]) %>%
        mutate(corr = ifelse(((slope(eval(parse(text = yaxis_var)),
                                          eval(parse(text = xaxis_var))))[3]), 
                             cor(eval(parse(text = yaxis_var)),
                                 eval(parse(text = xaxis_var)),
                                 method = "spearman",
                                 use = 'complete.obs'),
                             NA))
        
        plot1 <- plot1 +
        geom_abline(data = filter(sub_data, row_number() == 1), # only need to return 1 row within group_by to create annotations
                    aes_string(intercept = 'intercept',
                               slope = 'slope',
                               color = trt_group)) +
        geom_text(data = filter(sub_data, row_number() == 1), 
                  aes( x = -Inf,
                       y = Inf,
                       hjust = 0,
                       vjust = 1,
                       label = ifelse(!is.na(intercept) & !is.na(slope) & !is.na(corr),
                       sprintf("y=%.2f+%.2fX\ncor=%.2f", intercept, slope, corr),
                       paste0("Insufficient Data For Regression")),
                       color = eval(parse(text = trt_group))),
                       size = reg_text_size) +
                       labs(caption = paste("Deming Regression Model, Spearman Correlation Method"))
      }
 
  # Add abline
    if (yaxis_var %in% c('AVAL', 'AVALL2', 'BASE2', 'BASE2L2', 'BASE', 'BASEL2')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 1)}
    
    if (yaxis_var %in% c('CHG2', 'CHG')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 0)}
    
    if (yaxis_var %in% c('PCHG2', 'PCHG')) {plot1 <- plot1 + geom_abline(intercept = 100, slope = 0)}
    
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

  # Format treatment color
  if (!is.null(color_manual)){
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = 'Dose')
  }
  
  # Format LOQ flag symbol shape
  if (!is.null(shape_manual)){
    plot1 <- plot1 +
      scale_shape_manual(values = shape_manual, name = 'LOQ')
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
  
  # Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  plot1
  
}
