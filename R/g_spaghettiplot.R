#' Spaghetti plot
#' 
#' This function is rendered by teal.goshawk module
#'
#'
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param subj_id unique subject id variable name.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param biomaker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time.
#' @param color_manual vector of colors.
#' @param ymin y-axis lower limit.
#' @param ymax y-axis upper limit.
#' @param facet_ncol number of facets per row.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param roate_xlab boolean whether to rotate x-axis labels.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' 
#' @import ggplot2
#'
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details 
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
#' g_spaghettiplot(data = ANL,
#'                 subj_id = 'USUBJID',
#'                 biomarker_var = 'PARAMCD',
#'                 biomarker = 'CRP',
#'                 value_var = 'AVAL',
#'                 trt_group = 'ARM',
#'                 time = 'VISIT',
#'                 color_manual = c("ARM A" = "#000000", "ARM B" = "#3498DB", "ARM C" = "#E74C3C"),
#'                 hline = NULL,
#'                 rotate_xlab = FALSE,
#'                 group_mean = TRUE)


g_spaghettiplot <- function(data,
                            subj_id = 'USUBJID',
                            biomarker_var = 'PARAMCD',
                            biomarker_var_label = 'PARAM',
                            biomarker,
                            value_var = 'AVAL',
                            unit_var = 'AVALU',
                            trt_group,
                            trt_group_level = NULL,
                            time,
                            time_level = NULL,
                            color_manual = NULL,
                            ymin = NA,
                            ymax = NA,
                            facet_ncol = 2,
                            hline = NULL,
                            rotate_xlab = FALSE,
                            font_size = 12,
                            group_mean = FALSE){
  
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
  

  # Plot
  for.plot <- data[data[[biomarker_var]] %in% biomarker,]
  
  unit <- unique(filter(data, eval(parse(text = biomarker_var)) == biomarker)[[unit_var]])
  unit1 <- ifelse(is.na(unit) | unit == "", " ", paste0(' (', unit, ') '))
  
  biomarker1 <- unique(filter(data, eval(parse(text = biomarker_var)) == biomarker)[[biomarker_var_label]]) 
  
  gtitle <- paste0(biomarker1, unit1, value_var, ' Values by Treatment @ Visits')
  gylab <- paste0(biomarker1, ' ', value_var, ' Values')
  
  title <- list("AVAL" = "Analysis Value", "CHG" = "Change from Baseline")
  
  plot <- ggplot(data = for.plot, 
                 aes_string(x = time, y = value_var, color = trt_group, group = subj_id)) +
    geom_point(size=0.8) +
    geom_line(size=0.4) +
    facet_wrap(trt_group, ncol = facet_ncol) + 
    theme_bw() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ggtitle(gtitle) +
    xlab(time) + 
    ylab(gylab) + 
    theme(plot.title = element_text(size=font_size, margin = margin(), hjust = 0.5))
  
  # Add group mean
  if (group_mean){
    plot <- plot +
      stat_summary(fun.y=mean, geom="line", lwd=1, aes(group = 1, linetype = "Group Mean"), color = "green")+
      scale_linetype_manual(name = "", label = 'Group Mean', values = c(1))
  }
  
  # Format x-label
  if (rotate_xlab){
    plot <- plot +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  # Add manual color
  if (!is.null(color_manual)){
    plot <- plot +
      scale_color_manual(values = color_manual, name = 'Dose')
  }
  
  #Add horizontal line
  if (!is.null(hline)){
    plot <- plot +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  # Format font size
  if (!is.null(font_size)){
    plot <- plot +
      theme(plot.title = element_text(size=font_size, margin = margin()),
            axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }
  
  plot
  
}


