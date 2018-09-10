#' Function to generate a density distribution plot
#' Output rendered by teal.goshawk module \code{g_density_distribution_plot} returns
#' distribution overlay plot visualiztion
#'
#' Create overall density plot with treatment arms overlaid
#'
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables which will be displayed in the plot.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LBLOQFL.
#' @param unit name of variable containing biomarker unit.
#' @param color_manual vector of colors.
#' @param logscale set axis values to log scale.
#' @param facet set layout to use facetting.
#' @param facet_var variable to use for facetting.
#' @param pct set axis values to percent scale.
#' @param reg_line include regression line in visualization.
#' @param line_size line size.
#' 
#' @import DescTools
#' @import dplyr
#' @import ggplot2
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
#'# to test loq_flag_var
#'ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
#'
#' param <- c('CRP') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' 
#' plot1 <- g_density_distribution_plot(label = 'Density Distribution Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD',
#'            param = param,
#'            xaxis_var = 'AVAL',
#'            trt_group = 'ARM',
#'            unit = 'AVALU',
#'            xmin_scale = 0,
#'            xmax_scale = 200,
#'            color_manual = NULL,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            logscale = FALSE,
#'            facet = TRUE,
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
                                xmin_scale = 0,
                                xmax_scale = 200,
                                color_manual = NULL,
                                hline = NULL,
                                rotate_xlab = FALSE,
                                logscale = FALSE,
                                facet = FALSE,
                                facet_var = "AVISITCD",
                                font_size = 12,
                                line_size = 2){

  
  # for debugging
  print(paste0('Inside goshawk, FACET is now: ', facet))
  
  plot_data <- data %>%
    filter(eval(parse(text = param_var)) == param) %>%
    mutate(JUNK = 1)
  
  plot1 <- ggplot() +
    geom_density(data = plot_data, aes(x = eval(parse(text = xaxis_var)), linetype = 'all'), size = line_size) + 
    geom_density(data = plot_data, aes_string(x = xaxis_var, colour = trt_group), size = line_size) +
    #scale_color_manual(values = color_manual, name = 'Arm') +
    scale_linetype_manual(name = "All", values = c(all="solid", per_dose="solid")) +
    #facet_wrap(as.formula(paste0('~', facet_var))) +
    theme_bw() +
    xlim(xmin_scale, xmax_scale) +
    ggtitle(paste0('Biomarker ', param, ' (',  plot_data[[unit]], ') Density Overall & by ARM @ Visits')) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(paste0('Biomarker ', xaxis_var, ' Values')) +
    ylab(paste0('Density'))

  print(plot1$facet)
  
  # for debugging
  print(paste0("Inside goshawk just before facet if. FACET is now: ", facet))
  
  # add grid faceting to foundation - this block works when running the function but honks when
  # calling function from teal module
  # if (facet){
  # 
  #   # for debugging
  #   print(paste0("Inside goshawk inside facet if. FACET is now: ", facet))
  #   print(paste0("Inside goshawk inside facet if. FACET_VAR is now: ", facet_var))
  # 
  #   plot1 <- plot1 +
  #   facet_grid(as.formula(paste0('.~ ', facet_var)))
  # }

  #Add horizontal line
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
