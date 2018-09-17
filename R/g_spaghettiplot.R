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
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time.
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
#' plot1 <- g_spaghettiplot(data = ALB,
#'                          subj_id = 'USUBJID',
#'                          biomarker_var = 'PARAMCD',
#'                          biomarker = 'CRP',
#'                          value_var = 'AVAL',
#'                          trt_group = 'ARM',
#'                          time = 'AVISITCD',
#'                          hline = NULL,
#'                          rotate_xlab = FALSE)
#' plot1
#'
#' # EXAMPLE 2:
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
                            trt_group_level = NULL,
                            time,
                            time_level = NULL,
                            ymin = NA,
                            ymax = NA,
                            facet_ncol = 2,
                            hline = NULL,
                            rotate_xlab = FALSE){
  
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


