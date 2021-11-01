#' Function to create a table of descriptive summary statistics to accompany plots.
#'
#' Output descriptive summary statistics table as a data frame. Includes biomarker, treatment,
#' visit,
#' n, mean, median, sd, min, max, %missing values, % LOQ values.
#'
#' @param data name of data frame to summarize.
#' @param trt_group treatment group variable name e.g. ARM.
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param param biomarker to visualize e.g. IGG.
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. AVAL.
#' @param facet_var name of variable facetted on typically containing visit values e.g. AVISITCD.
#'   If NULL then ignored.
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
#' # Example using ADaM structure analysis dataset.
#'
#' library(scda)
#' library(stringr)
#'
#' # original ARM value = dose value
#' arm_mapping <- list("A: Drug X" = "150mg QD", "B: Placebo" = "Placebo",
#' "C: Combination" = "Combination")
#'
#' ASL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' ADLB <- ADLB %>%
#'   mutate(AVISITCD = case_when(
#'     AVISIT == "SCREENING" ~ "SCR",
#'     AVISIT == "BASELINE" ~ "BL",
#'     grepl("WEEK", AVISIT) ~
#'       paste(
#'         "W",
#'         trimws(
#'           substr(
#'             AVISIT,
#'             start = 6,
#'             stop = str_locate(AVISIT, "DAY") - 1
#'           )
#'         )
#'       ),
#'     TRUE ~ NA_character_)) %>%
#'   mutate(AVISITCDN = case_when(
#'     AVISITCD == "SCR" ~ -2,
#'     AVISITCD == "BL" ~ 0,
#'     grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
#'     TRUE ~ NA_real_)) %>%
#'   # use ARMCD values to order treatment in visualization legend
#'   mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
#'     ifelse(grepl("B", ARMCD), 2,
#'       ifelse(grepl("A", ARMCD), 3, NA)))) %>%
#'   mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
#'   mutate(ARM = factor(ARM) %>%
#'   reorder(TRTORD))
#'
#' tbl <- t_summarytable(data = ADLB,
#'                trt_group = "ARM",
#'                param_var = "PARAMCD",
#'                param = c("CRP"),
#'                xaxis_var = "AVAL",
#'                facet_var = "AVISITCD",
#'                loq_flag_var = "LOQFL")
#' tbl
t_summarytable <- function(data,
                           trt_group,
                           param_var,
                           param,
                           xaxis_var,
                           facet_var = "AVISITCD",
                           loq_flag_var = "LOQFL", ...) {

  if (!is.null(facet_var) && trt_group == facet_var) {
    data[paste0(facet_var, "_")] <- data[facet_var]
    facet_var <- paste0(facet_var, "_")
  }

  table_data <- data %>%
    filter(!!sym(param_var) == param)

  # get unique study id or unique study ids if multiple study data
  study_id <- as.data.frame(table(table_data$STUDYID)) %>%
    mutate(STUDYID = paste(.data$Var1, collapse = "/")) %>%
    select(StudyID = .data$STUDYID) %>%
    slice(1)

  # get analysis variable name
  anl_var <- as.data.frame(xaxis_var) %>%
    rename("AnlVar" = xaxis_var)

  min_max_ignore_na <- function(x, type = c("min", "max")) {
    type <- match.arg(type)
    if (all(is.na(x))) {
      return(NA)
    }
    return(
      switch(type,
        min = min,
        max = max
      )(x, na.rm = TRUE)
    )
  }

  # by treatment group table

  sum_data_by_arm <- table_data %>% filter(!!sym(param_var) == param)
  if (!is.null(facet_var)) {
    sum_data_by_arm <- sum_data_by_arm %>%
      group_by(!!sym(param_var), !!sym(trt_group), .data$TRTORD, !!sym(facet_var))
  } else {
    sum_data_by_arm <- sum_data_by_arm %>%
      group_by(!!sym(param_var), !!sym(trt_group), .data$TRTORD)
  }

  sum_data_by_arm <- sum_data_by_arm %>%
    summarise(
      n = sum(!is.na(!!sym(xaxis_var))),
      Mean = round(mean(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      Median = round(median(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      StdDev = round(sd(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      Min = round(min_max_ignore_na(!!sym(xaxis_var), type = "min"), digits = 2),
      Max = round(min_max_ignore_na(!!sym(xaxis_var), type = "max"), digits = 2),
      PctMiss = round(100 * sum(is.na(!!sym(xaxis_var))) / length(!!sym(xaxis_var)), digits = 2),
      PctLOQ = round(100 * sum(!!sym(loq_flag_var) == "Y", na.rm = TRUE) / length(!!sym(loq_flag_var)), digits = 2)
    )

  if (!is.null(facet_var)) {
    sum_data_by_arm <- sum_data_by_arm %>%
      select(param_var, trt_group, facet_var, .data$n:.data$PctLOQ, .data$TRTORD) %>%
      ungroup()
  } else{
    sum_data_by_arm <- sum_data_by_arm %>%
      select(param_var, trt_group, .data$n:.data$PctLOQ, .data$TRTORD) %>%
      ungroup()
  }

  # by combined treatment group table
  sum_data_combined_arm <- table_data %>%
    filter(!!sym(param_var) == param)

  if (!is.null(facet_var)) {
    sum_data_combined_arm <- sum_data_combined_arm %>%
      group_by(!!sym(param_var), !!sym(facet_var))
  } else {
    sum_data_combined_arm <- sum_data_combined_arm %>%
      group_by(!!sym(param_var))
  }

  sum_data_combined_arm <- sum_data_combined_arm %>%
    summarise(
      n = sum(!is.na(!!sym(xaxis_var))),
      Mean = round(mean(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      Median = round(median(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      StdDev = round(sd(!!sym(xaxis_var), na.rm = TRUE), digits = 2),
      Min = round(min_max_ignore_na(!!sym(xaxis_var), type = "min"), digits = 2),
      Max = round(min_max_ignore_na(!!sym(xaxis_var), type = "max"), digits = 2),
      PctMiss = round(100 * sum(is.na(!!sym(xaxis_var))) / length(!!sym(xaxis_var)), digits = 2),
      PctLOQ = round(100 * sum(!!sym(loq_flag_var) == "Y", na.rm = TRUE) / length(!!sym(loq_flag_var)), digits = 2),
      MAXTRTORDVIS = max(.data$TRTORD) # identifies the maximum treatment order within visits
    ) %>% # additional use of max function identifies maximum treatment order across all visits.
    mutate(!!trt_group := "Comb.", TRTORD = max(.data$MAXTRTORDVIS) + 1)

  # select only those columns needed to prop
  if (!is.null(facet_var)) {
    sum_data_combined_arm <- sum_data_combined_arm %>%
    select(param_var, trt_group, facet_var, .data$n:.data$PctLOQ, .data$TRTORD) %>%
    ungroup()

    # combine the two data sets and apply some formatting. Note that R coerces treatment group into
    # character since it is a factor and character
    sum_data <- rbind(sum_data_by_arm, sum_data_combined_arm) %>% # concatenate
      # reorder variables
      select(Biomarker = param_var, Treatment = trt_group, Facet = facet_var, .data$n:.data$PctLOQ, .data$TRTORD) %>%
      arrange(.data$Biomarker, .data$Facet, .data$TRTORD) %>% # drop variable
      select(-.data$TRTORD)

  } else {
    sum_data_combined_arm <- sum_data_combined_arm %>%
    select(param_var, trt_group, .data$n:.data$PctLOQ, .data$TRTORD) %>%
    ungroup()

    # combine the two data sets and apply some formatting. Note that R coerces treatment group into
    # character since it is a factor and character
    sum_data <- rbind(sum_data_by_arm, sum_data_combined_arm) %>% # concatenate
      # reorder variables
      select(Biomarker = param_var, Treatment = trt_group, .data$n:.data$PctLOQ, .data$TRTORD) %>%
      arrange(.data$Biomarker, .data$TRTORD) %>% # drop variable
      select(-.data$TRTORD)
  }

  # add analysis variable as first column
  sum_data <- cbind(study_id, anl_var, sum_data)

}