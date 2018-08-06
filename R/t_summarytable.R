#' Function to generate a table of descriptive summary statistics accompany
#' plots.
#' 
#' Output render by teal.goshawk module \code{t_summarytable} returns
#' descriptive summary statistics table
#'
#' @param data data frame name
#' @param biomarker biomarker PARAM value
#' @param trt_group treatment group variable name e.g. arm
#' @param time visit variable name e.g. visit
#' @param loq loq variable name e.g. loq_flag
#'
#' @author Balazs Toth
#' @author first last
#'
#' @details provide additional information as needed. perhaps link to
#'   specification file
#'
#' @return \code{returned object name} object
#'
#' @export
#'
#' @examples
#'
#' # test data
#' biomarker <- runif(100,3,100)
#' biomarker.bl <- NULL
#' for (i in 0:24) {
#'  biomarker.bl <- c ( biomarker.bl, rep(biomarker[i * 4 + 1], 4))
#'  }
#'  arm <- c(rep('trt',50),rep('pbo',50))
#'  visit <- c(rep(c(0,4,8,12),25))
#'  loq_flag <- c(rep(c(rep('YES',10),rep('NO',10)),5))
#'  example <- data.frame(biomarker = biomarker, biomarker.bl = biomarker.bl, arm = arm, loq_flag = loq_flag, visit = visit)
#'  colnames(example) <- c('IGG', 'IGG.bl', 'arm','loq_flag','visit')
#'  unit <- 'g/L'
#'  timepoint <- 'screening'
#'  color_manual <- c('pbo' = "#1F78B4", 'trt' = "#33A02C")
#'  shape_manual <- c('NO' = 1, 'YES' = 2, 'NA' = 0)
#'
#' # call function
#' t_summarytable(data = example,
#'               biomarker = 'IGG',
#'               trt_group = 'arm',
#'               time = 'visit',
#'               loq = 'loq_flag')
#'
t_summarytable <- function(data, biomarker, trt_group, time, loq, ...){
  
  # Helper
  sum_data <- data %>%
    group_by(eval(parse(text = time)),
             eval(parse(text = trt_group))) %>%
    summarise(n = sum(!is.na(eval(parse(text = biomarker)))),
              avg = round(mean(eval(parse(text = biomarker)),
                               na.rm = TRUE),1),
              med = round(median(eval(parse(text = biomarker)),
                                 na.rm = TRUE),1),
              sd = round(sd(eval(parse(text = biomarker)),
                            na.rm = TRUE),1),
              pctLOQ = round(100 * sum(eval(parse(text = loq))=='YES')/
                               length(eval(parse(text = loq))=='YES'),2))

  tbl <- tableGrob(t(sum_data))
  grid.arrange(arrangeGrob(tbl, ncol=1, nrow=1))

  return(tbl)

}
