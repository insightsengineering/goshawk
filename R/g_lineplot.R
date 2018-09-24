#' Line plot of summary statistics over time
#' 
#' This function is rendered by teal.goshawk module
#'
#'
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomaker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time.
#' @param color_manual vector of colors.
#' @param median boolean whether to display median results.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param roate_xlab boolean whether to rotate x-axis labels.
#' 
#' @import ggplot2
#' @import dplyr
#' @importfrom gridExtra grid_arrange
#' @importfrom stringr str_wrap
#'
#' @author Balazs Toth (toth.balazs@gene.com)
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details Currently, the output plot can display mean and median of input value. For mean, the error bar denotes
#' 95\% confidence interval. For median, the error bar denotes median-SD to median+SD.
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(gridExtra)
#' library(stringr)
#' 
#' ANL <- expand.grid(
#'   USUBJID = paste0("p-",1:100),
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B", "ARM C"),
#'   PARAMCD = c("CRP", "IGG", "IGM")
#' )
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' ANL$PCHG <- ANL$CHG/ANL$AVAL*100
#' ANL$AVALU <- 'mg'
#' 
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' 
#' g_lineplot(label = 'Line Plot',
#'            data = ANL,
#'            biomarker_var = 'PARAMCD',
#'            biomarker = 'CRP',
#'            value_var = 'AVAL',
#'            trt_group = 'ARM',
#'            time = 'VISIT',
#'            color_manual = NULL,
#'            median = FALSE,
#'            hline = NULL,
#'            rotate_xlab = FALSE)
#'            



g_lineplot <- function(label = 'Line Plot',
                       data,
                       biomarker_var = 'PARAMCD',
                       biomarker,
                       value_var = 'AVAL',
                       unit_var = 'AVALU',
                       ymin = NA, ymax = NA,
                       trt_group,
                       trt_group_level = NULL,
                       time,
                       time_level = NULL,
                       color_manual,
                       median = FALSE,
                       hline = NULL,
                       rotate_xlab = FALSE) {
  
  ## Pre-process data
  if(!is.null(trt_group_level)){
    data[[trt_group]] <- factor(data[[trt_group]],
                                levels = trt_group_level)
  } else {
    data[[trt_group]] <- factor(data[[trt_group]])
  }
  
  if(!is.null(time_level)){
    data[[time]] <- factor(data[[time]],
                                levels = time_level)
  } else {
    data[[time]] <- factor(data[[time]])
  }
  
  ## Summary statistics
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

  ## Base plot
  pd <- position_dodge(0.5)

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

  unit <- unique(filter(data, eval(parse(text = biomarker_var)) == biomarker)[[unit_var]])

  plot1 <-  ggplot(data = sum_data,
                   aes_string(x = time,
                              y = line,
                              color = trt_group,
                              group = trt_group)) +
    geom_point(position = pd) +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit,
                             ymax = up_limit),
                  width=0.5,
                  position = pd) +
    theme_bw() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ggtitle(paste0(biomarker, ' (', unit, ') ', line, ' over time')) +
    xlab(time) + 
    ylab(paste0(biomarker, ' ', line, title))+
    theme(legend.position = "bottom",
          plot.title = element_text(size=18, margin = margin()),
          axis.title.y = element_text(margin = margin(r = 20)))
    

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

  
  ## number of obs table
  sum_data[[trt_group]] <- factor(sum_data[[trt_group]],
                                  levels = rev(levels(sum_data[[trt_group]])))
  arm <- as.vector(unique(sum_data[[trt_group]]))
  x <- unique(sum_data[[time]])
  
  tbl <- ggplot(sum_data, aes_string(x = time, y = trt_group, label = 'count')) +
    geom_text(size = 3.5) +
    ggtitle("Number of observations") + 
    theme_minimal() +
    scale_y_discrete(labels = function(x = sum_data[[trt_group]]) str_wrap(x, width = 8)) + 
    scale_x_discrete(limits = x)+
    theme(panel.grid.major = element_blank(), legend.position = "none",
          panel.border = element_blank(), axis.text.x =  element_blank(),
          axis.ticks =  element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=12))
  
  grid.arrange(plot1, tbl, heights = c(10, 2))
 
}
