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
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param time name of variable containing visit codes e.g. AVISITCD.
#' @param loq_flag name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit name of variable containing biomarker unit.
#' @param timepoint x axis visit selected label.
#' @param color_manual vector of colors.
#' @param shape_manual vector of shapes.
#' @param logscale set axis values to log scale.
#' @param facet set layout to use facetting.
#' @param facet_var variable to use for facetting.
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
#'ASL0 <- read_bce(ASL_path)
#'ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')
#'
#'ALB0 <- read_bce(ALB_path)
#'
#'# post process the data to subset records per specification
#'ALB_SUBSET <- subset(ALB0,
#'               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
#'               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
#'                'LBSTRESC', 'LBSTRESN'))
#'
#' # calculate the minimum AVAL for each PARAMCD
#' PARAM_MINS <- ALB_SUBSET %>%
#' select(USUBJID, PARAMCD, AVAL) %>%
#'   filter(PARAMCD %in% param_choices) %>%
#'   group_by(PARAMCD) %>%
#'   summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))
#'   
#'# post process the data to create several new variables and adjust existing record specific valules per specification
#'# - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
#'# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
#'ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BB", 0, substr(AVISITCD,start=2, stop=4))) %>%
#'                mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
#'                mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
#'                mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG))
#'                # may need to add similar code for BASE2 related variables
#'
#'   
#' # merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
#' ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
#'        mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
#'        mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
#'        #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))
#'
#'# for proper chronological ordering of visits in visualizations
#'ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
#'ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#'
#'# to test loq_flag
#'ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
#'
#' param <- c('CRP') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' shape_manual <- c('N' = 1, 'Y' = 2, 'NA' = 0)
#' 
#' plot1 <- g_scatterplot(label = 'Scatter Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD', # name of variable containing the biomarker names.
#'            param = param, # the PARAMCD value selected.
#'            xaxis_var = 'BASE',
#'            yaxis_var = 'AVAL', # name of variable containing the analysis values.
#'            trt_group = 'ARM',
#'            visit = 'AVISITCD',
#'            loq_flag = 'LOQFL',
#'            unit = 'AVALU',
#'            #timepoint = 'Baseline', # build flexibility into app to be able to change x axis visit
#'            xmin_scale = 0,
#'            xmax_scale = .5,
#'            ymin_scale = 0,
#'            ymax_scale = .5,
#'            color_manual = NULL,
#'            shape_manual,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            logscale = FALSE,
#'            facet = FALSE,
#'            facet_var = "ARM",
#'            reg_line = FALSE, # slope and correlation values for each ARM overwrite
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
                          loq_flag = "LOQFL",
                          unit = "AVALU",
                          #timepoint = "Baseline",
                          xmin_scale = 0,
                          xmax_scale = 200,
                          ymin_scale = 0,
                          ymax_scale = 200,
                          color_manual = NULL,
                          man_color = NULL,
                          shape_manual = c('N' = 1, 'Y' = 2, 'NA' = 0),
                          hline = NULL,
                          rotate_xlab = FALSE,
                          logscale = FALSE,
                          facet = FALSE,
                          facet_var = "ARM",
                          reg_line = FALSE,
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
    geom_point(aes_string(shape = loq_flag), size = dot_size, na.rm = TRUE) +
    facet_wrap(as.formula(paste0('~',visit))) +
    theme_bw() +
    #scale_color_manual(values = color_manual, name = 'Dose') +
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
                    size = reg_text_size)
        
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
      geom_point(aes_string(shape = loq_flag), size = dot_size, na.rm = TRUE)
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
