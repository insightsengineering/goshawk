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
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables which will be displayed in the plot.
#' @param biomarker_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param biomaker biomarker to visualize e.g. IGG. 
#' @param value_var_bl name of variable containing biomarker results at baseline e.g. BASE.
#' @param value_var name of variable containing biomarker results e.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param time name of variable containing visit codes e.g. AVISITCD.
#' @param loq_flag name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit name of variable containing biomarker unit.
#' @param timepoint x axis visit selected label.
#' @param color_manual vector of colors.
#' @param shape_manual vector of shapes.
#' @param logscale set axis values to log scale.
#' @param facet set layout to use facet.
#' @param pct set axis values to percent scale.
#' @param reg_line include regression line in visualization.
#' @param dot_size scatter dot size.
#' 
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using analysis dataset for example ASL or ADSL,
#' # ALB points to biomarker data stored in a typical LB structure. for example ALB or ADLB.
#' 
#'# for development team testing
#'ASL_path <- "~/btk/lupus/dataadam/asl.sas7bdat"
#'ALB_path <- "~/btk/lupus/dataadam/alb3arm.sas7bdat"
#'
#'# list of biomarkers of interest. see ALB2 assignment below
#'param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")
#'
#'ASL <- read_bce(ASL_path)
#'ALB0 <- read_bce(ALB_path)
#'
#'# post process the data to subset records per specification and to create new variables
#'ALB1 <- subset(ALB0,
#'               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
#'               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG'))
#'
#'# create a visit code - baseline record code is "BB" week records coded to "W NN"
#'ALB <- ALB1 %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4)))
#'     
#'ALB$AVISITCDN <- as.numeric(ALB$AVISITCDN) # coerce character into numeric
#'
#'# for proper chronological ordering of visits in visualizations
#'ALB <- ALB %>%
#'       mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#'
#' biomarker <- c('ADIGG') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' 
#' plot1 <- g_scatterplot(label = 'Scatter Plot',
#'            data = ALB,
#'            biomarker_var = 'PARAMCD', # name of variable containing the biomarker names. tm = param_var
#'            biomarker = biomarker, # the PARAMCD value selected. tm = param
#'            value_var = 'AVAL', # name of variable containing the analysis values. tm = value_var
#'            value_var_bl = 'BASE',
#'            trt_group = 'ARM',
#'            visit = 'AVISITCD',
#'            loq_flag = NULL,
#'            unit = 'AVALU',
#'            #timepoint = 'Baseline', # build flexibility into app to be able to change x axis visit
#'            xmin_scale = 0,
#'            xmax_scale = 2500,
#'            ymin_scale = 0,
#'            ymax_scale = 2500,
#'            color_manual = NULL,
#'            shape_manual = NULL,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            logscale = FALSE,
#'            f_facet = FALSE,
#'            facet = "ARM",
#'            reg_line = FALSE, # slope and correlation values for each ARM overwrite
#'            dot_size = 1)
#' plot1 
#' 
#' }

g_scatterplot <- function(label = 'Scatter Plot',
                          data = ALB,
                          biomarker_var = 'PARAMCD',
                          biomarker = "CRP",
                          value_var = 'AVAL',
                          value_var_bl = 'BASE',
                          trt_group = "ARM",
                          visit = "AVISITCD",
                          loq_flag = NULL,
                          unit = "AVALU",
                          #timepoint = "Baseline",
                          xmin_scale = 0,
                          xmax_scale = 200,
                          ymin_scale = 0,
                          ymax_scale = 200,
                          color_manual = NULL,
                          man_color = NULL,
                          shape_manual = NULL,
                          hline = NULL,
                          rotate_xlab = FALSE,
                          logscale = FALSE,
                          f_facet = FALSE,
                          facet = "ARM",
                          reg_line = FALSE,
                          dot_size = NULL){


# create scatter plot over time pairwise per treatment arm 
plot_data <- data %>%
  filter(eval(parse(text = biomarker_var)) == biomarker)

#plot_data_g <<- plot_data # save to global environment for debugging the sub_data statements below
#print (plot_data) # print data for debugging

# # identify min and max values of BM range ignoring NA values
#   min_scale <- min(plot_data[[value_var]], na.rm = TRUE)
#   max_scale <- max(plot_data[[value_var]], na.rm = TRUE)

  
# create plot foundation
  plot1 <- ggplot2::ggplot(data = plot_data,
                   aes_string(x = value_var_bl,
                              y = value_var,
                              color = trt_group)) +
    geom_point(aes_string(shape = loq_flag), size = 1, na.rm = TRUE) +
    facet_wrap(as.formula(paste0('~',visit))) +
    theme_bw() +
    #scale_color_manual(values = color_manual, name = 'Dose') +
    #scale_shape_manual(values = shape_manual, name = 'LoQ') +
    xlim(xmin_scale, xmax_scale) + ylim(ymin_scale, ymax_scale) +
    ggtitle(paste0('Biomarker ', biomarker, ' (',  plot_data[[unit]], ')')) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(paste0('Biomarker ', value_var_bl, ' Values')) +
    ylab(paste0('Biomarker ', value_var,' Values'))
    
    
# add grid faceting to foundation 
    if (f_facet){
      plot1 <- plot1 +
        facet_grid(as.formula(paste0(facet ,' ~ ', visit)))
    }

# add regression line
    if (reg_line){
      
      slope <- function(x, y) {
        ratio <- sd(x)/sd(y)
        reg <- mcr:::mc.deming(y, x, ratio)
        return(c(round(reg$b0,2),round(reg$b1,2)))
      }
      
      
      sub_data <- subset(plot_data, !is.na(eval(parse(text = value_var))) &
                           !is.na(eval(parse(text = value_var_bl)))) %>%
        group_by_(.dots = c(trt_group, visit)) %>%
        filter(n() > 1) %>%
        mutate(intercept = slope(eval(parse(text = value_var)),
                                 eval(parse(text = value_var_bl)))[1]) %>%
        mutate(slope = slope(eval(parse(text = value_var)),
                             eval(parse(text = value_var_bl)))[2]) %>%
        mutate(corr = cor(eval(parse(text = value_var)),
                          eval(parse(text = value_var_bl)),
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
                    size = 3)
        
    } 
  
  # Add abline
    if (value_var %in% c('AVAL','AVALL2', 'BASE', 'BASEL2', 'BASE2', 'BASE2L2')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 1)}
    
    if (value_var == 'CHG') {plot1 <- plot1 + geom_abline(intercept = 0, slope = 0)}
    
    if (value_var == 'PCHG') {plot1 <- plot1 + geom_abline(intercept = 100, slope = 0)}
    

  # Format dot size
  if (!is.null(dot_size)){
    plot1 <- plot1 +
      geom_point(size = dot_size)
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
  
  #Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  
  plot1
  
}
