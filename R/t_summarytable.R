#' Function to generate a table of descriptive summary statistics accompany
#' plots.
#' 
#' Output render by teal.goshawk module \code{t_summarytable} returns
#' descriptive summary statistics table
#'
#' @param data frame name
#' @param trt_group treatment group variable name e.g. ARM
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param visit_var name of variable containing visit values e.g. AVISITCD
#' @param loq_flag_var loq variable name e.g. loq_flag_var (not in current ALB)
#'
#' @author Balazs Toth
#' @author Nick Paszty
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @return \code{tbl} object
#'
#' @export
#'
#' @examples
#'
#' ALB <- read_bce("/opt/bee/home_nas/npaszty/btk/lupus/dataadam/alb3arm.sas7bdat")
#' param <- c('IGG') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' 
#' # call function
#' t_summarytable(data = ALB,
#'                trt_group = 'ARM',
#'                param_var = 'PARAMCD',
#'                param = param, # the PARAMCD value
#'                xaxis_var = 'AVAL',
#'                visit_var = 'AVISITCD',
#'                font_size = 12,
#'                loq_flag_var = 'LOQFL')
#'
#' table1
#'
t_summarytable <- function(data,
                           trt_group,
                           param_var,
                           param,
                           xaxis_var,
                           visit_var = 'AVISITCD',
                           font_size = 12,
                           loq_flag_var = 'LOQFL', ...){
  
  table_data <- data %>%
    filter(eval(parse(text = param_var)) == param)
  
  # by ARM table
  sum_data_by_arm <- table_data %>%
    filter(eval(parse(text = param_var)) == param) %>%
    group_by_(.dots = c(param_var, trt_group, visit_var)) %>%
    summarise(n = sum(!is.na(eval(parse(text = xaxis_var)))),
              Mean = round(mean(eval(parse(text = xaxis_var)), na.rm = TRUE),1),
              Median = round(median(eval(parse(text = xaxis_var)), na.rm = TRUE),1),
              StdDev = round(sd(eval(parse(text = xaxis_var)), na.rm = TRUE),1),
              Min = round(min(eval(parse(text = xaxis_var)), na.rm = TRUE),1),
              Max = round(max(eval(parse(text = xaxis_var)), na.rm = TRUE),1),
              PctMiss = round(100 * sum(is.na(eval(parse(text = xaxis_var))))/length(is.na(eval(parse(text = xaxis_var)))),1),
              PctLOQ =  round(100 * sum(eval(parse(text = loq_flag_var))=='Y')/length(eval(parse(text = loq_flag_var))=='Y'),2)
              )
}
