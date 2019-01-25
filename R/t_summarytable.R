#' Function to create a table of descriptive summary statistics to accompany plots
#' 
#' Output descriptive summary statistics table as a data frame. Includes biomarker, treatment, visit,
#' n, meand, median, sd, min, max, %missing values, % LOQ values.
#'
#' @param data name of data frame to summarize.
#' @param trt_group treatment group variable name e.g. ARM.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param visit_var name of variable containing visit values e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LOQFL.
#' @param ... additional options
#'
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#' 
#' t_summarytable(data = ALB,
#'                trt_group = 'ARM',
#'                param_var = 'PARAMCD',
#'                param = param,
#'                xaxis_var = 'BASE',
#'                visit_var = 'AVISITCD',
#'                loq_flag_var = 'LOQFL')
#'
#'}
#'

t_summarytable <- function(data,
                           trt_group,
                           param_var,
                           param,
                           xaxis_var,
                           visit_var = 'AVISITCD',
                           loq_flag_var = 'LOQFL', ...){
  
  table_data <- data %>%
    filter(eval(parse(text = param_var)) == param)
  
  # by ARM table
  sum_data_by_arm <- table_data %>%
    filter(eval(parse(text = param_var)) == param) %>%
    group_by_(.dots = c(param_var, trt_group, "TRTORD", visit_var)) %>%
    summarise(n = sum(!is.na(eval(parse(text = xaxis_var)))),
              Mean = round(mean(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Median = round(median(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              StdDev = round(sd(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Min = round(min(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Max = round(max(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              PctMiss = round(100 * sum(is.na(eval(parse(text = xaxis_var))))/length(eval(parse(text = xaxis_var))), digits = 2),
              PctLOQ =  round(100 * sum(eval(parse(text = loq_flag_var)) == 'Y', na.rm = TRUE)/length(eval(parse(text = loq_flag_var))), digits = 2)
              ) %>%
    select(param_var, trt_group, visit_var, n:PctLOQ, TRTORD) %>%
    ungroup()
  
  # by combined ARM table
  sum_data_combined_arm <- table_data %>%
    filter(eval(parse(text = param_var)) == param) %>%
    group_by_(.dots = c(param_var, visit_var)) %>%
    summarise(n = sum(!is.na(eval(parse(text = xaxis_var)))),
              Mean = round(mean(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Median = round(median(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              StdDev = round(sd(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Min = round(min(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              Max = round(max(eval(parse(text = xaxis_var)), na.rm = TRUE), digits = 2),
              PctMiss = round(100 * sum(is.na(eval(parse(text = xaxis_var))))/length(eval(parse(text = xaxis_var))), digits = 2),
              PctLOQ =  round(100 * sum(eval(parse(text = loq_flag_var)) == 'Y', na.rm = TRUE)/length(eval(parse(text = loq_flag_var))), digits = 2),
              MAXTRTORDVIS = max(TRTORD) # identifies the maximum treatment order within visits
    ) %>% # additional use of max function identifies maximum treatment order across all visits.
    mutate(ARM = "Comb.", TRTORD = max(MAXTRTORDVIS) + 1) %>% # select only those columns needed to prop
    select(param_var, trt_group, visit_var, n:PctLOQ, TRTORD) %>%
    ungroup()
  
  # combine the two data sets and apply some formatting. Note that R coerces ARM into character since it is a factor and character
  sum_data <- rbind(sum_data_by_arm, sum_data_combined_arm) %>% # concatenate
    select(Biomarker = param_var, Treatment = trt_group, Visit = visit_var, n:PctLOQ, TRTORD) %>% # reorder variables
    arrange(Biomarker, Visit, TRTORD) %>% # drop variable
    select(-TRTORD)
}
