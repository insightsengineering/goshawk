#' Function to generate a line plot
#' output rendered by teal.goshawk module
#'
#' \code null
#'
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomaker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param trt_group name of variable representing treatment group.
#' @param time name of vairable containing visit names.
#' @param color_manual vector of colors.
#' @param median boolean whether to display median results.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param roate_xlab boolean whether to rotate x-axis labels.
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @author Balazs Toth
#' @author Wenyi Liu (luiw2) wenyi.liu@roche.com
#'
#' @details provide additional information as needed. perhaps link to specification file.\url{http://rstudio.com}
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#'
#'


g_lineplot <- function(label = 'Line Plot',
                       data,
                       biomarker_var = 'PARAMCD',
                       biomarker,
                       value_var = 'AVAL',
                       trt_group,
                       time,
                       color_manual,
                       median = FALSE,
                       hline = NULL,
                       rotate_xlab = FALSE) {
  
  # Summary statistics
  sum_data <- data %>%
    filter(eval(parse(text = biomarker_var)) == biomarker) %>%
    group_by(eval(parse(text = time)),
             eval(parse(text = trt_group))) %>%
    summarise(count = sum(!is.na(eval(parse(text = value_var)))),
              mean = mean(eval(parse(text = value_var)),na.rm = TRUE),
              CIup = mean(eval(parse(text = value_var)),na.rm = TRUE) + 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
              CIdown = mean(eval(parse(text = value_var)),na.rm = TRUE) - 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
              median = median(eval(parse(text = value_var)),na.rm = TRUE),
              quant25 = quantile(eval(parse(text = value_var)), 0.25, na.rm = TRUE),
              quant75 = quantile(eval(parse(text = value_var)), 0.75, na.rm = TRUE))
  colnames(sum_data)[1:2] <- c(time,trt_group)

  # Base plot
  pd <- position_dodge(0.8)

  if (median) {
    line <- 'median'
    up_limit <- 'quant75'
    down_limit <- 'quant25'
  } else {
    line <- 'mean'
    up_limit <- 'CIup'
    down_limit <- 'CIdown'
  }

  title <- ''
  if (grepl('CHG',value_var)) {
    title <- paste0(' change from baseline')
  }


  plot1 <-  ggplot(data = sum_data,
                   aes_string(x = time,
                              y = line,
                              color = trt_group,
                              group = trt_group)) +
    geom_point(position = pd) +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit,
                             ymax = up_limit),
                  width=1,
                  position = pd) +
    theme_bw() +

    theme(legend.position = "bottom") +
    ggtitle(paste0(biomarker, ' ', line, ' over time')) +
    xlab('time') + ylab(paste0(biomarker, ' ', line, title))

  # Format x-label
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
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


## example
# ALB <- read_bce("/opt/BIOSTAT/prod/cdpt7692/s28625i/libraries/alb.sas7bdat")
# ALB <- read_bce("/opt/bee/home_nas/luiw2/teal.goshawk/alb.sas7bdat")

# g_lineplot(label = 'Line Plot',
#            data = ALB,
#            biomarker_var = 'PARAMCD',
#            biomarker = 'CRP',
#            value_var = 'AVAL',
#            trt_group = 'ARM',
#            time = 'AVISIT',
#            color_manual = NULL,
#            median = FALSE,
#            hline = NULL,
#            rotate_xlab = TRUE)
