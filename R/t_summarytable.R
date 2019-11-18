#' Function to create a table of descriptive summary statistics to accompany plots.
#'
#' Output descriptive summary statistics table as a data frame. Includes biomarker, treatment,
#' visit,
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
#' @details provide additional information as needed. link to specification file
#' \url{http://rstudio.com}
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#' # Example using ADaM structure analysis dataset.
#'
#' library(dplyr)
#' library(goshawk)
#' library(random.cdisc.data)
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo",
#' "C: Combination" = "Combination")
#'
#' ASL <- radsl(N = 20, seed = 1)
#' ALB <- radlb(ASL, visit_format = "WEEK", n_assessments = 7L, seed = 2)
#' ALB <- ALB %>%
#' mutate(AVISITCD = case_when(
#' AVISIT == "SCREENING" ~ "SCR",
#' AVISIT == "BASELINE" ~ "BL", grepl("WEEK", AVISIT) ~ paste("W", trimws(substr(AVISIT, start = 6,
#' stop = str_locate(AVISIT, "DAY")-1))),
#' TRUE ~ as.character(NA))) %>%
#' mutate(AVISITCDN = case_when(AVISITCD == "SCR" ~ -2,
#' AVISITCD == "BL" ~ 0, grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#' TRUE ~ as.numeric(NA))) %>%
#' # use ARMCD values to order treatment in visualization legend
#' mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#' ifelse(grepl("B", ARMCD), 2,
#' ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#' mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#' mutate(ARM = factor(ARM) %>% reorder(TRTORD))
#'
#' tbl <- t_summarytable(data = ALB,
#'                trt_group = "ARM",
#'                param_var = "PARAMCD",
#'                param = c("CRP"),
#'                xaxis_var = "AVAL",
#'                visit_var = "AVISITCD",
#'                loq_flag_var = "LOQFL")
#' tbl
#'
#'}
#'
t_summarytable <- function(data,
                           trt_group,
                           param_var,
                           param,
                           xaxis_var,
                           visit_var = "AVISITCD",
                           loq_flag_var = "LOQFL", ...){
  table_data <- data %>%
    filter(!!sym(param_var) == param)
  # by treatment group table
  sum_data_by_arm <- table_data %>%
    filter(!!sym(param_var) == param) %>%
    group_by_(.dots = c(param_var, trt_group, "TRTORD", visit_var)) %>%
    summarise(n = sum(!is.na(!!sym(xaxis_var))),
              Mean = round(mean(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Median = round(median(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              StdDev = round(sd(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Min = round(min(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Max = round(max(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              PctMiss = round(100 * sum(is.na(!!sym(xaxis_var))) /
                                length(!!sym(xaxis_var)),
                              digits = 2),
              PctLOQ = round(100 * sum(!!sym(loq_flag_var) == "Y", na.rm = TRUE) /
                               length(!!sym(loq_flag_var)),
                             digits = 2)
    ) %>%
    select(param_var, trt_group, visit_var, .data$n:.data$PctLOQ, .data$TRTORD) %>%
    ungroup()
  # by combined treatment group table
  sum_data_combined_arm <- table_data %>%
    filter(!!sym(param_var) == param) %>%
    group_by_(.dots = c(param_var, visit_var)) %>%
    summarise(n = sum(!is.na(!!sym(xaxis_var))),
              Mean = round(mean(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Median = round(median(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              StdDev = round(sd(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Min = round(min(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              Max = round(max(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
              PctMiss = round(100 * sum(is.na(!!sym(xaxis_var))) /
                                length(!!sym(xaxis_var)),
                              digits = 2),
              PctLOQ = round(100 * sum(!!sym(loq_flag_var) == "Y", na.rm = TRUE) /
                               length(!!sym(loq_flag_var)),
                             digits = 2),
              MAXTRTORDVIS = max(.data$TRTORD) # identifies the maximum treatment order within visits
    ) %>% # additional use of max function identifies maximum treatment order across all visits.
    mutate(!!trt_group := "Comb.", TRTORD = max(.data$MAXTRTORDVIS) + 1) %>% # select only those columns needed to prop
    select(param_var, trt_group, visit_var, .data$n:.data$PctLOQ, .data$TRTORD) %>%
    ungroup()
  # combine the two data sets and apply some formatting. Note that R coerces treatment group into character since it is
  # a factor and character
  sum_data <- rbind(sum_data_by_arm, sum_data_combined_arm) %>% # concatenate
    select(Biomarker = param_var, Treatment = trt_group, Visit = visit_var,
           .data$n:.data$PctLOQ, .data$TRTORD) %>% # reorder variables
    arrange(.data$Biomarker, .data$Visit, .data$TRTORD) %>% # drop variable
    select(-.data$TRTORD)
}
