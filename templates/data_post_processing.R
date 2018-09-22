#'# for development team testing
#'ASL_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/asl.sas7bdat"
#'ALB_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/alb3arm.sas7bdat"
#'
#'# list of biomarkers of interest. see ALB2 assignment below
#'param_choices <- c("CRP","ADIGG","IG","IGA","IGE","IGG","IGM","TEST")
#'
#'ASL0 <- read_bce(ASL_path)
#'ASL <- subset(ASL0, subset = ITTFL == 'Y' & IAFL == 'Y')
#'
#'ALB0 <- read_bce(ALB_path)
#'
#'# post process the data to subset records per specification
#'ALB_SUBSET <- subset(ALB0,
#'               subset = PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('BASE%','%WEEK%'), 
#'               select = c('STUDYID', 'USUBJID', 'ITTFL', 'ANLFL', 'ARM', 'ARMCD', 'AVISIT', 'AVISITN', 'PARAMCD', 'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG',
#'                'LBSTRESC', 'LBSTRESN'))
#'
#' # calculate the minimum AVAL for each PARAMCD
#' PARAM_MINS <- ALB_SUBSET %>%
#' select(USUBJID, PARAMCD, AVAL) %>%
#'   filter(PARAMCD %in% param_choices) %>%
#'   group_by(PARAMCD) %>%
#'   summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))
#'   
#'# post process the data to create several new variables and adjust existing record specific valules per specification
#'# - create a visit code variable - baseline record code is "BB" and week records coded to "W NN"
#'# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
#'ALB_SUPED1 <- ALB_SUBSET %>% mutate(AVISITCD = paste0(substr(AVISIT,start=1, stop=1), 
#'                                         substr(AVISIT, start=regexpr(" ", AVISIT), stop=regexpr(" ", AVISIT)+2))) %>%
#'                mutate(AVISITCD = ifelse(AVISITCD == "BB", "BL", AVISITCD)) %>%
#'                mutate(AVISITCDN =  ifelse(AVISITCD == "BL", 0, substr(AVISITCD,start=2, stop=4))) %>%
#'                mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
#'                mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
#'                mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG)) %>%
#'                mutate(ARMCD = ifelse(grepl("1", ARM), "C", ifelse(grepl("2", ARM), "B", ifelse(grepl("3", ARM), "A", NA)))) %>%
#'                mutate(TRTORD = ifelse(grepl("C", ARMCD), 1, ifelse(grepl("B", ARMCD), 2, ifelse(grepl("A", ARMCD), 3, NA))))
#'                # may need to add similar code for BASE2 related variables
#'
#'   
#' # merge minimum AVAL value onto the ALB data to calculate the log2 variables and preserve the variable order
#' ALB_SUPED2 <- merge(ALB_SUPED1, PARAM_MINS, by="PARAMCD")[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
#'        mutate(AVALL2 = ifelse(AVAL == 0, log2(AVAL_MIN/2), log2(AVAL))) %>%
#'        mutate(BASEL2 = ifelse(BASE == 0, log2(AVAL_MIN/2), log2(BASE))) #%>% need SPA to finish adding BASE2 to ALB
#'        #mutate(BASE2L2 = ifelse(BASE2 == 0, log2(AVAL_MIN/2), log2(AVAL)))
#'
#'# for proper chronological ordering of visits in visualizations
#'ALB_SUPED2$AVISITCDN <- as.numeric(ALB_SUPED2$AVISITCDN) # coerce character into numeric
#'ALB <- ALB_SUPED2 %>% mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN))
#'
#'# for representing treatment string that includes dose
#' ALB <- ALB %>%
#'       mutate(ARMORVAL = ARM) %>%
#'       mutate(ARM = case_when(
#'         ARMCD == "C" ~ "Placebo",
#'         ARMCD == "B" ~ "150mg QD",
#'         ARMCD == "A" ~ "200mg BID",
#'         TRUE ~ as.character(NA)))
#' 
#' ALB <- ALB %>% mutate(ARM = factor(ARM) %>% reorder(TRTORD))
#'
#'
#'# to test loq_flag_var
#'ALB <- ALB %>% mutate(LOQFL = ifelse(PARAMCD == "CRP" & AVAL < .5, "Y", "N"))
