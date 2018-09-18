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
#' @importFrom stringr str_wrap
#' @importFrom gridExtra grid.arrange
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
#' # EXAMPLE 1:
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(gridExtra)
#' library(stringr)
#' library(DescTools)
#' 
#' # for development team testing
#' ASL_path <- "~/btk/lupus/dataadam/asl.sas7bdat"
#' ALB_path <- "~/btk/lupus/dataadam/alb3arm.sas7bdat"
#' 
#' # list of biomarkers of interest. see ALB2 assignment below
#' param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")
#' 
#' ASL0 <- read_bce(ASL_path)
#' ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')
#' 
#' ALB0 <- read_bce(ALB_path)
#' 
#' # post process the data to subset records per specification
#' ALB_SUBSET <- subset(ALB0,
#'                      subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'),
#'                      select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
#'                                 'LBSTRESC', 'LBSTRESN'))
#' 
#' # calculate the minimum AVAL for each PARAMCD
#' PARAM_MINS <- ALB_SUBSET %>%
#'   select(USUBJID, PARAMCD, AVAL) %>%
#'   filter(PARAMCD %in% param_choices) %>%
#'   group_by(PARAMCD) %>%
#'   summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))
#' 
#' # post process the data to create several new variables and adjust existing record specific valules per specification
#' # - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
#' # - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
#' ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1),
#'                                                       substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'   mutate(AVISITCD = ifelse(AVISITCD == "BB", "BL", AVISITCD)) %>%
#'   mutate(AVISITCDN =  ifelse(AVISITCD == "BL", 0, substr(AVISITCD,start=2, stop=4))) %>%
#'   mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
#'   mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
#'   mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG))
#' # may need to add similar code for BASE2 related variables
#' 
#' 
#' # merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
#' ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
#'   mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
#'   mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
#' #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))
#' 
#' # for proper chronological ordering of visits in visualizations
#' ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
#' ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#' 
#' # to test loq_flag_var
#' ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
#' 
#' param <- c('CRP') # FOR TESTING: woud come from teal.goshawk.tm_g_moduleName.R
#' shape_manual <- c('N' = 1, 'Y' = 2, 'NA' = 0)
#' 
#' plot1 <- g_lineplot(label = 'Line Plot',
#'                     data = ALB,
#'                     biomarker_var = 'PARAMCD',
#'                     biomarker = 'CRP',
#'                     value_var = 'AVAL',
#'                     trt_group = 'ARM',
#'                     time = 'AVISITCD',
#'                     color_manual = NULL,
#'                     median = FALSE,
#'                     hline = NULL,
#'                     rotate_xlab = FALSE)
#' plot1
#' 
#' # EXAMPLE 2:
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
    ggtitle(paste0(biomarker, ' ', line, ' over time')) +
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


