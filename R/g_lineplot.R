#' Line plot of summary statistics over time
#' 
#' This function is rendered by teal.goshawk module
#'
#'
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param biomaker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param unit_var name of variable containing biomarker result unit.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time.
#' @param color_manual vector of colors.
#' @param median boolean whether to display median results.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param roate_xlab boolean whether to rotate x-axis labels.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control position dodge
#' 
#' @import ggplot2
#' @import dplyr
#' @import grid
#' @importFrom stringr str_wrap
#' @importFrom stringr str_to_title
#' @importFrom gridExtra grid.arrange
#' @importFrom grid unit.pmax
#'
#' @author Balazs Toth (toth.balazs@gene.com)
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details Currently, the output plot can display mean and median of input value. For mean, the error bar denotes
#' 95\% confidence interval. For median, the error bar denotes median-25% quartile to median+75% quartile.
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#' 
#' # EXAMPLE:
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(gridExtra)
#' library(grid)
#' library(stringr)
#'
#' ANL <- expand.grid(
#'   USUBJID = paste0("p-",1:100),
#'   VISIT = paste0("visit ", 1:10),
#'   ARM = c("ARM A", "ARM B", "ARM C"),
#'   PARAMCD = c("CRP", "IGG", "IGM"),
#'   PARAM = c("C-reactive protein", "Immunoglobulin G", "Immunoglobulin M")
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



g_lineplot <- function(label = 'Line Plot',
                       data,
                       biomarker_var = 'PARAMCD',
                       biomarker_var_label = 'PARAM',
                       biomarker,
                       value_var = 'AVAL',
                       unit_var = 'AVALU',
                       ymin = NA, ymax = NA,
                       trt_group,
                       trt_group_level = NULL,
                       time,
                       time_level = NULL,
                       color_manual = NULL,
                       median = FALSE,
                       hline = NULL,
                       rotate_xlab = FALSE,
                       font_size = 12,
                       dodge = 0.4) {
  
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
  pd <- position_dodge(dodge)

  if (median) {
    line <- 'median'
    up_limit <- 'quant75'
    down_limit <- 'quant25'
  } else {
    line <- 'mean'
    up_limit <- 'CIup'
    down_limit <- 'CIdown'
  }

  unit <- unique(filter(data, eval(parse(text = biomarker_var)) == biomarker)[[unit_var]])
  unit1 <- ifelse(is.na(unit) | unit == "", " ", paste0(' (', unit, ') '))
  
  biomarker1 <- unique(filter(data, eval(parse(text = biomarker_var)) == biomarker)[[biomarker_var_label]]) 
  gtitle <- paste0(biomarker1, unit1, str_to_title(line), ' by Treatment @ Visits')
  gylab <- paste0(biomarker1, ' ', str_to_title(line), ' of ', value_var, ' Values')
  
  plot1 <-  ggplot(data = sum_data,
                   aes_string(x = time,
                              y = line,
                              color = trt_group,
                              group = trt_group)) +
    geom_point(position = pd) +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit,
                             ymax = up_limit),
                  width=0.4,
                  position = pd) +
    theme_bw() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ggtitle(gtitle) +
    labs(caption = paste("The output plot can display mean and median of input value.
                         For mean, the error bar denotes 95% confidence interval.
                         For median, the bar denotes the first to third quartile.")) +
    xlab(time) + 
    ylab(gylab)+
    theme(legend.position = "bottom",
          plot.title = element_text(size=font_size, margin = margin(), hjust = 0.5),
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
  
  # Format font size
  if (!is.null(font_size)){
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size))
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
    scale_y_discrete(labels = function(x = sum_data[[trt_group]]) str_wrap(x, width = 12)) + 
    scale_x_discrete(limits = x)+
    theme(panel.grid.major = element_blank(), legend.position = "none",
          panel.border = element_blank(), axis.text.x =  element_blank(),
          axis.ticks =  element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=font_size),
          plot.title = element_text(face = "bold", size=font_size))
  
  glist <- lapply(list(plot=plot1, text=tbl), ggplotGrob)
  leftmar <- do.call(unit.pmax, lapply(glist, "[[", "widths"))
  glist.aligned <- lapply(glist, function(x) {
    x$widths <- leftmar
    x
  })
  
  #Plot the two grobs using grid.arrange
  grid.newpage()
  do.call(grid.arrange, c(glist.aligned, 
                          list(ncol=1), 
                          list(heights=c(18,length(unique(sum_data[[trt_group]]))))))
 
}


