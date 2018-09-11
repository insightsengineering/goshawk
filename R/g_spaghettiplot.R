#' Spaghetti plot
#' 
#' This function is rendered by teal.goshawk module
#'
#'
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param subj_id unique subject id variable name.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomaker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param trt_group name of variable representing treatment group.
#' @param time name of vairable containing visit names.
#' @param ymin y-axis lower limit.
#' @param ymax y-axis upper limit.
#' @param facet_ncol number of facets per row.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param roate_xlab boolean whether to rotate x-axis labels.
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
#' library(dplyr)
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
#'                 hline = NULL,
#'                 rotate_xlab = FALSE)

g_spaghettiplot <- function(data,
                            subj_id = 'USUBJID',
                            biomarker_var = 'PARAMCD',
                            biomarker,
                            value_var = 'AVAL',
                            trt_group,
                            time,
                            ymin = NA,
                            ymax = NA,
                            facet_ncol = 2,
                            hline = NULL,
                            rotate_xlab = FALSE){

  for.plot <- data[data[[biomarker_var]] %in% biomarker,]
  
  title <- list("AVAL" = "Analysis Value", "CHG" = "Change from Baseline")
  
  plot <- ggplot(data = for.plot, 
                 aes_string(x = time, y = value_var, color = trt_group, group = subj_id)) +
    geom_point(size=0.8) +
    geom_line(size=0.4) +
    facet_wrap(trt_group, ncol = facet_ncol) + 
    theme_bw() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    ggtitle(paste0(biomarker, " - ", title[[value_var]], " over time")) +
    xlab(time) + 
    ylab(paste0(biomarker))
  
  # Format x-label
  if (rotate_xlab){
    plot <- plot +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  #Add horizontal line
  if (!is.null(hline)){
    plot <- plot +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  plot
  
}


