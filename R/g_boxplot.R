#' Function to generate a boxplot
#' Output rendered by teal.goshawk module \code{g_boxplot} returns boxplot visualization
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
#' @param value_var name of variable containing biomarker results displayed on
#'   Y-axis e.g. AVAL.
#' @param trt_group name of variable representing treatment trt_group e.g. ARM.
#' @param loq_flag  name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit biomarker unit label e.g. (U/L)
#' @param timepoint text to include on the plot title
#' @param color_manual vector of colour for trt_group
#' @param shape_manual vector of shapes (used with log_flag)
#' @param box add boxes to the plot (boolean)
#' @param logscale use a log scale for the Y axis (boolean)
#' @param ymin_scale minimum value for the Y axis
#' @param ymax_scale maximum value for the Y axis
#' @param facet variable to facet the plot by, or "None" if no faceting
#'   required. 
#' @param font_size point size of tex to use.  NULL is use default size
#' @param alpha transparency for the points (0 = transparent, 1 = opaque)
#'   
#' @import ggplot2
#' @import dplyr
#' @import grid
#' @importFrom gridExtra grid.arrange
#'
#' @author Balazs Toth
#' @author Jeff Tomlinson (tomlinsj) jeffrey.tomlinson@roche.com
#'
#' @details provide additional information as needed. perhaps link to
#'   specification file.\url{http://rstudio.com}
#'   
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(goshawk)
#' 
#' # Example data for 100 patients from 3 treatment groups
#' ASL <- radam('ASL', start_with = list(
#'   STUDYID = "XX99999",
#'   USUBJID = paste0("XX99999-999-",1:100),
#'   ITTFL = 'Y',
#'   SEX = c("M", "F"),
#'   ARM = paste("ARM", LETTERS[1:3])
#' ))
#' 
#' # Create an example ALB dataset from the 100 patient, with 6 visits with 3 
#' # parameters for each subject.  
#' ALB <- expand.grid(USUBJID = ASL$USUBJID
#'               , AVISIT = c("BASELINE",  paste0("VISIT ", 1:5))
#'               , PARAMCD = c("CRP", "IGA", "IGG")
#'               , stringsAsFactors = FALSE
#'     ) 
#'     
#' ALB <- ALB %>%
#'   mutate( AVAL = rnorm(nrow(ALB), mean = 50, sd = 8) )
#' 
#' ALB <- ALB %>% 
#'   left_join(ALB %>% 
#'               filter(AVISIT == "BASELINE") %>% 
#'               select(USUBJID, PARAMCD, BASE = AVAL)
#'             , by = c("USUBJID", "PARAMCD")) %>% 
#'   left_join(ASL, by = "USUBJID") %>% 
#'   mutate(CHG = AVAL - BASE, PCHG = 100 * CHG/BASE) %>% 
#'   mutate(LOQFL = ifelse(AVAL < 32, "Y", "N"))
#' 
#' # Example 1.
#' 
#' # Plot boxplots for IGA by Treatment (ARM) faceted by AVISIT, to display a
#' # boxplot for each visit.
#' # Specific colors are given for each treatment group.
#' # Values that are flagged by LOQFL are given a different symbol.
#' # Log scale for the Y axis.
#' # Points are plot with a transparency
#' 
#' # ALB is a VAD or ADAM format dataset, subsetted for the appropriate population.
#' # Select the parameter of interest to plot.
#' albplot <- ALB %>%
#'   filter(PARAMCD == "IGA")
#' 
#' g_boxplot(albplot
#'           , biomarker = "IGA"
#'           , value_var = "AVAL"
#'           , trt_group = "ARM"
#'           , loq_flag = 'LOQFL'
#'           , timepoint = "over time"
#'           , unit = "U/L"
#'           #, color_manual = c('ARM A' = "#1F78B4", 'ARM B' = "#33A02C", 'ARM C' = "#601010")
#'           , shape_manual = c('N' = 1, 'Y' = 2, 'NA' = NULL)
#'           , facet = "AVISITCD"
#'           , alpha = 0.5
#'           , logscale = FALSE
#' )
#' 
#' 
#' # Example 2
#' #
#' # A plot using mostly default values. 
#' albplot <- ALB %>%
#'   filter(PARAMCD == "CRP" & AVISIT == "BASELINE")
#' 
#' g_boxplot(albplot
#'           , biomarker = "CRP"
#'           , value_var = "AVAL"
#'           , trt_group = "ARM"
#' )
#' 
#'}
#'
#'
g_boxplot <- function(data,
                       biomarker,
                       value_var,
                       trt_group,
                       loq_flag = NULL,
                       unit = NULL,
                       timepoint = NULL,
                       color_manual = NULL,
                       shape_manual = NULL,
                       box = TRUE,
                       logscale = FALSE,
                       ymax_scale = NULL,
                       ymin_scale = NULL,
                       dot_size = 2,
                       alpha = 1.0,
                       font_size = NULL,
                       facet = NULL) { 

  # Setup the Y axis label.  Combine the biomarker and the units (if available)
  yAxisLabel <- ifelse(is.null(unit), biomarker, 
                       ifelse(unit == "", biomarker, paste0(biomarker,' (',unit,')'))
                       )

  # A useable name for the X axis.
  # If present, use the label for the trt_group parameter, if not then use the name
  # of the parameter (in title case)
  if (!is.null(attr(data[[trt_group]], "label", exact = TRUE))) {
    armlabel <- attr(data[[trt_group]], "label")
  } else {
    armlabel <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(trt_group), perl=TRUE)
  }

  # Base plot
  plot1 <-  ggplot()
    
  # Add boxes if required 
  if (box) {
    plot1 <- plot1 +
      geom_boxplot(data = data,
                   aes_string( x = trt_group,
                               y = value_var,
                               color = trt_group,
                               fill = NULL),
                   outlier.shape = NA) 
  } 
  
  plot1 <- plot1 +
    geom_jitter(data = data,
                aes_string( x = trt_group,
                            y = value_var,
                            color = trt_group,
                            shape = loq_flag),
                alpha = alpha,
                position = position_jitter(width = 0.1, height = 0),
                size=dot_size) +
    xlab(armlabel) +
    ylab(yAxisLabel) +
    theme_bw() +
    ggtitle(paste0(value_var,' distribution @ ',timepoint,' per arm')) 

  # Colors supplied?  Use color_manual, otherwise default ggplot coloring.  
  if (!is.null(color_manual)) {
    cols <- color_manual
    plot1 <- plot1 +
      scale_color_manual(values = cols, name = armlabel) +
      scale_fill_manual(values = cols, name = armlabel)  
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

  #Adjust scale to log
  if (logscale){
    plot1 <- plot1 +
      coord_trans( y = "log10")
  }

  #Add facetting.
  if (!is.null(facet) ){
    if (facet != "None" & facet %in% names(data)) {
      plot1 <- plot1 +
        facet_wrap(as.formula(paste0('~',facet)))
    }
  }

  # Format font size
  if (!is.null(font_size)){
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
  

   return(plot1)
  
}
