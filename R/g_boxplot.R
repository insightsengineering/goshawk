#' Function to generate a boxplot
#' Output rendered by teal.goshawk module \code{g_boxplot} returns boxplot visualization
#'
#' A box plot is a method for graphically depicting groups of numerical data through their quartiles. Box 
#' plots may also have lines extending vertically from the boxes (whiskers) indicating variability 
#' outside the upper and lower quartiles, hence the term box-and-whisker. Outliers may be plotted as 
#' individual points. Box plots are non-parametric: they display variation in samples of a statistical 
#' population without making any assumptions of the underlying statistical distribution. The spacings 
#' between the different parts of the box indicate the degree of dispersion (spread) and skewness in the 
#' data, and show outliers. In addition to the points themselves, they allow one to visually estimate 
#' various L-estimators, notably the interquartile range, midhinge, range, mid-range, and trimean. 
#'
#' @param data data frame with variables which will be displayed in the plot.
#' @param biomarker biomarker to visualize e.g. IGG. 
#' @param value_var name of variable containing biomarker results displayed on Y-axis e.g. AVAL.
#' @param arm name of variable representing treatment arm e.g. ARM.
#' @param loq_flag  name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit biomarker unit label e.g. (U/L)
#' @param timepoint 
#' @param color_manual vector of colour for arm
#' @param shape_manual vector of shapes (used with log_flag)
#' @param box add boxes to the plot (boolean)
#' @param logscale use a log scale for the Y axis (boolean) 
#' @param ymin_scale minimum value for the Y axis
#' @param ymax_scale maximum value for the Y axis
#' @param facet variable to facet the plot by, or "None" if no faceting required.
#' 
#'
#' @author Balazs Toth
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#'
#' @details provide additional information as needed. perhaps link to specification file.\url{http://rstudio.com}
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # ALB is a VAD or ADAM format dataset, subsetted for the appropriate population.
#' # Select the parameter of interest to plot.
#' albplot <- ALB %>% 
#'   filter(PARAMCD == "IGA") 
#' 
#' # Plot boxplots for IGA by Treatment (ARM) faceted by AVISIT, to display a 
#' # boxplot for each visit.  
#' # Specific colors are given for each treatment group.
#' # Values that are flagged by LOQFL are given a different symbol.
#' g_boxplot(albplot
#'           , biomarker = "IGA"
#'           , value_var = "AVAL"
#'           , arm = "ARM"
#'           , loq_flag = 'LOQFL'
#'           , timepoint = "over time"
#'           , unit = "U/L"
#'           , color_manual = c('ARM 1' = "#1F78B4", 'ARM 2' = "#33A02C", 'ARM 3' = "#601010")
#'           , shape_manual = c('N' = 1, 'Y' = 2, 'NA' = NULL)
#'           , facet = "AVISIT"
#'           , box = TRUE
#' )
#' 
#'}
#'
#'
g_boxplot <- function(data,
                      biomarker,
                      value_var,
                      arm,
                      loq_flag = NULL,
                      unit = NULL,
                      timepoint = NULL,
                      color_manual = NULL,
                      shape_manual = NULL,
                      box = TRUE,
                      logscale = FALSE,
                      ymax_scale = NULL,
                      ymin_scale = NULL,
                      facet = NULL) {
  
  # Setup the Y axis label.  Combine the biomarker and the units (if available)
  yAxisLabel <- ifelse(is.null(unit) | unit == "", biomarker, str_c(biomarker,' (',unit,')'))
  
  # A useable name for the X axis.
  # If present, use the label for the arm parameter, if not then use the name
  # of the parameter (in title case)
  if (has_attr(data[[arm]], "label")) {
    armlabel <- attr(data[[arm]], "label")
  } else {
    armlabel <- str_to_title(arm)
  }
  
  # Base plot
  plot1 <-  ggplot() +
    geom_jitter(data = data,
                aes_string( x = arm,
                            y = value_var,
                            color = arm,
                            shape = loq_flag),
                position = position_jitter(width = 0.1, height = 0),
                size=1) +
    xlab(armlabel) +
    ylab(yAxisLabel) +
    theme_bw() +
    ggtitle(paste0(value_var,' distribution @ ',timepoint,' per arm')) 
  
  # Colors supplied?
  if (!is.null(color_manual)) {
    cols <- color_manual
  } else {
    cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", 
              "#66A61E", "#E6AB02", "#A6761D", "#666666")
  }  
  plot1 <- plot1 +
    scale_color_manual(values = cols, name = armlabel) +
    scale_fill_manual(values = cols, name = armlabel)  
  
  # LOQ needed?
  if (!is.null(shape_manual) ) {
    plot1 <- plot1 +
      scale_shape_manual(values = shape_manual, name = 'LoQ')
  }
  
  # Any limits for the Y axis?
  if (!is.null(ymin_scale) & !is.null(ymax_scale)) {
    plot1 <- plot1 + coord_cartesian(ylim = c(ymin_scale, ymax_scale)) 
  }
  
  # Add boxes if required 
  if (box) {
    plot1 <- plot1 +
      geom_boxplot(data = data,
                   aes_string( x = arm,
                               y = value_var,
                               color = arm,
                               fill = NULL),
                   outlier.shape = NA) +
      geom_jitter(data = data,
                  aes_string( x = arm,
                              y = value_var,
                              color = arm,
                              shape = loq_flag),
                  position = position_jitter(width = 0.1, height = 0),
                  size=1)
  } 
  
  #Adjust scale to log
  if (logscale){
    plot1 <- plot1 +
      coord_trans( y = "log10")
  }
  
  #Add facetting
  if (!is.null(facet) ){
    if (facet != "None") {
      plot1 <- plot1 +
        facet_wrap(as.formula(paste0('.~',facet)))
    }
  }
  
  return(plot1)
  
}
