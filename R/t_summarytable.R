#' Function to generate a table of descriptive summary statistics accompany
#' plots.
#' 
#' Output render by teal.goshawk module \code{t_summarytable} returns
#' descriptive summary statistics table
#'
#' @param data frame name
#' @param biomarker PARAM value
#' @param trt_group treatment group variable name e.g. ARM
#' @param time visit variable name e.g. VISIT
#' @param loq loq variable name e.g. loq_flag (not in current ALB)
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
#' biomarker <- c('IGG') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' 
#' # call function
#' t_summarytable(data = ALB,
#'                trt_group = 'ARM',
#'                biomarker_var = 'PARAMCD',
#'                biomarker = biomarker, # the PARAMCD value
#'                value_var = 'AVAL',
#'                visit = 'AVISIT',
#'                loq = NULL)
#'               
#'
t_summarytable <- function(data,
                           trt_group,
                           biomarker_var,
                           biomarker,
                           value_var,
                           visit,
                           loq, ...){
  
  # Helper
  sum_data <- data %>%
    filter(eval(parse(text = biomarker_var)) == biomarker) %>%
    group_by(eval(parse(text = visit)),
             eval(parse(text = trt_group))) %>%
    summarise(n = sum(!is.na(eval(parse(text = value_var)))),
              avg = round(mean(eval(parse(text = value_var)),
                               na.rm = TRUE),1),
              med = round(median(eval(parse(text = value_var)),
                                 na.rm = TRUE),1),
              sd = round(sd(eval(parse(text = value_var)),
                            na.rm = TRUE),1)
              #pctLOQ = round(100 * sum(eval(parse(text = loq))=='YES')/
              #                 length(eval(parse(text = loq))=='YES'),2)
              )

  tbl <- tableGrob(t(sum_data))
  grid.arrange(arrangeGrob(tbl, ncol=1, nrow=1))

  return(tbl)

}
