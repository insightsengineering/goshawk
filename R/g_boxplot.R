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
#' @param yaxis_var name of variable containing biomarker results displayed on
#'   Y-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment trt_group e.g. ARM.
#' @param loq_flag  name of variable containing LOQ flag e.g. LOQFL.
#' @param unit biomarker unit label e.g. (U/L)
#' @param color_manual vector of colour for trt_group
#' @param shape_manual vector of shapes (used with log_flag)
#' @param box add boxes to the plot (boolean)
#' @param ymin_scale minimum value for the Y axis
#' @param ymax_scale maximum value for the Y axis
#' @param facet variable to facet the plot by, or "None" if no faceting
#'   required. 
#' @param xaxis_var variable used to group the data on the x-axis.
#' @param armlabel label for the treatment symbols in the legend.
#'        If not specified then the label attribute for trt_group will be used. 
#'        If there is no label attribute for trt_group, then the name of the parameter (in title case) will be used.
#' @param facet_ncol number of facets per row.  NULL = Use the default for facet_wrap
#' @param hline y-axis value to position a horizontal line.  NULL = No line
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size point size of tex to use.  NULL is use default size
#' @param dot_size plot dot size.
#' @param alpha dot transparency (0 = transparent, 1 = opaque)
#'   
#' @import ggplot2
#' @import dplyr
#' @import grid
#' @importFrom gridExtra grid.arrange
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
#'\dontrun{
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, seed = 1)
#' ADLB <- radlb(ADSL, seed = 2)
#' ADLB <- ADLB %>% subset(PARAMCD == "CRP")
#' 
#' # Example 1.
#' g_boxplot(ADLB
#'           , biomarker = "CRP"
#'           , yaxis_var = "AVAL"
#'           , trt_group = "ARM"
#'           , loq_flag = "LOQFL"
#'           , unit = "AVALU"
#'           , shape_manual = c('N' = 1, 'Y' = 2, 'NA' = NULL)
#'           , hline = NULL
#'           , facet = "AVISIT"
#'           , xaxis_var = "STUDYID"
#'           , alpha = 0.5
#'           , rotate_xlab = TRUE
#' )
#' 
#'}
#'
g_boxplot <- function(data,
                      biomarker,
                      yaxis_var,
                      trt_group,
                      xaxis_var = NULL,
                      loq_flag = "LOQFL",
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
                      armlabel = NULL,
                      facet = NULL                      
) { 
  
  # Setup the Y axis label.  Combine the biomarker and the units (if available)
  yAxisLabel <- ifelse(is.null(unit), paste(data$PARAM, yaxis_var, "Values"), 
                       ifelse(unit == "", paste(data$PARAM, yaxis_var, "Values"), 
                              paste0(data$PARAM, " (", unit, ") ", yaxis_var, " Values"))
  )
  
  # Setup the ggtitle label.  Combine the biomarker and the units (if available)
  ggtitleLabel <- ifelse(is.null(unit), paste(data$PARAM, "Distribution by Treatment @ Visits"), 
                         ifelse(unit == "", paste(data$PARAM, "Distribution by Treatment @ Visits"), 
                                paste0(data$PARAM," (", unit,") Distribution by Treatment @ Visits"))
  )
  
  # If supplied, then use armlabel as specified.
  # otherwise If present, use the label for the trt_group parameter,
  # otherwise if not then use the name of the parameter (in title case)
  t_label <- attr(data[,names(data) == trt_group],'label')
  armlabel <- ifelse(!is.null(armlabel), armlabel,
                     ifelse(!is.null(t_label), t_label,
                            gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(trt_group), perl=TRUE)))
  
  # Base plot
  plot1 <-  ggplot()
  
  # Add boxes if required 
  if (box) {
    plot1 <- plot1 +
      geom_boxplot(data = data,
                   aes_string( x = xaxis_var,
                               y = yaxis_var,
                               fill = NULL),
                   outlier.shape = NA) 
  } 
  
  # Extend is.infinite to include zero length objects.
  is_finite <- function(x){
    if(length(x) == 0) return(FALSE)
    return(is.finite(x))
  }
  
  # Add horizontal line
  if (is_finite(hline)) {
    plot1 <- plot1 +
      geom_hline(yintercept = hline, color="red", linetype="dashed", size=0.5)
  }
  
  plot1 <- plot1 +
    geom_jitter(data = data,
                aes_string( x = xaxis_var,
                            y = yaxis_var,
                            color = trt_group,
                            shape = loq_flag),
                alpha = alpha,
                position = position_jitter(width = 0.1, height = 0),
                size=dot_size) +
    labs(color = armlabel, x = NULL, y = yAxisLabel) +
    theme_bw() +
    ggtitle(ggtitleLabel) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5))
  
  # Colors supplied?  Use color_manual, otherwise default ggplot coloring.  
  if (!is.null(color_manual)) {
    cols <- color_manual
    plot1 <- plot1 +
      scale_color_manual(values = cols) +
      scale_fill_manual(values = cols)  
  }
  
  # LOQ needed?
  if (!is.null(shape_manual) ) {
    plot1 <- plot1 +
      scale_shape_manual(values = shape_manual, name = 'LoQ')
  }
  
  # Any limits for the Y axis?
  if (!is.null(ymin_scale) & !is.null(ymax_scale)) {
    plot1 <- plot1 + coord_cartesian(ylim = c(ymin_scale, ymax_scale)) 
  }
  
  # Add facetting.
  if (!is.null(facet)){
    if (facet != "None" & facet %in% names(data)) {
      if (!is_finite(facet_ncol)) facet_ncol <- 0
      
      if (facet_ncol >= 1){
        plot1 <- plot1 +
          facet_wrap(as.formula(paste0('~',facet)), ncol = round(facet_ncol))
      } else {
        plot1 <- plot1 +
          facet_wrap(as.formula(paste0('~',facet)))
      }
    }
  }
  
  # Format font size
  if (is_finite(font_size)){
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x  = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y  = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text  = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }
  
  # Format x-label
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  return(plot1)
  
}
