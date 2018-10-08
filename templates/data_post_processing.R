################################################################################
# BEGIN: SPA Input Required
################################################################################
# study root path under qa branch - used for determining access
access_path <- "/opt/BIOSTAT/qa/cdt7738a"

# absolute path and data det name
ASL_path <- "/opt/BIOSTAT/qa/cdt7738a/s30044m/libraries/asl.sas7bdat"
ALB_path <- "/opt/BIOSTAT/qa/cdt7738a/s30044m/libraries/alb.sas7bdat"

# for development team testing
# ASL_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/asl.sas7bdat"
# ALB_path <- "/opt/bee/home_nas/npaszty/btk/lupus/dataadam/alb3arm.sas7bdat"

# study information
MOLECULE <- "BTK"
INDICATION <- "Lupus"
STUDY <- "GA30044"

# analysis type e.g. Interim Analysis, Final Analysis etc.
ATYPE <- "Interim Analysis"

# assign treatment colors to match those used in SPA study output
color_manual = c('Placebo' = "#000000", '150mg QD' = "#3498DB", '200mg BID' = "#E74C3C")

# assign LOQ flag symbols: circles and triangles respectively
shape_manual = c('N' = 1, 'Y' = 2, 'NA' = 0)

# list of biomarkers of interest. see ALB1 assignment below
param_choices <- c("ACIGG", "ACIGM", "ADIGG", "ANAPC", "ANAPDS", "ANAPH", "ANAPM", "ANAPND", "ANAPP", "ANAPR", "ANAPS", "ANAPSP", "ASSBABC", "AVCT_JMT",
                   "B2G1GAAB", "B2GLYIGG", "B2GLYIGM", "BASOSF", "BTKP", "BTKT",
                   "C3", "C4S", "CCL20", "CCL3", "CCL4", "CD19A", "CD19P", "CD3A", "CD3P", "CD4A", "CD4P", "CD8A", "CD8P", "CRP",
                   "DLCT_JCH", "DLCT_MZB", "DLCT_TXN",
                   "IGG", "IGM",
                   "NLBC_JCH", "NLBC_MZB", "NLBC_TXN",
                   "RNPAABC",  "RWCT_JCH", "RWCT_MZB", "RWCT_TME", "RWCT_TXN",
                   "SSAAABC")

# biomarkers of interest to exclude from performing log2 calculation: 
# NLBC are already log2 transformed: assigned AVAL to AVALL2
exclude_l2 <- c("NLBC_JCH", "NLBC_MZB", "NLBC_TXN")
# DLCT are CHG values, AVCT is average CHG: assigned NA
exclude_chg <- c("DLCT_JCH", "DLCT_MZB", "DLCT_TXN", "AVCT_JMT")

################################################################################
# END: SPA Input Required
################################################################################

# post process the ASL and ALB data to subset records per specification
ASL <- read_bce(ASL_path) %>%
  filter(ITTFL == 'Y' & IAFL == 'Y')

ALB_SUBSET <- read_bce(ALB_path) %>%
  filter(PARAMCD %in% c(param_choices) & ITTFL == 'Y' & IAFL == 'Y' & ANLFL == 'Y' & AVISIT %like any% c('SCREEN%', 'BASE%','%WEEK%')) %>%
  select(c('STUDYID', 'USUBJID',
           'ITTFL', 'ANLFL', 'ABLFL2', 
           'ARM', 'ARMCD', 
           'AVISIT', 'AVISITN', 
           'PARAM','PARAMCD', 
           'AVAL', 'AVALU', 'BASE', 'CHG', 'PCHG', 'BASE2', 'CHG2', 'PCHG2',
           'LBSTRESC', 'LBSTRESN',
           'LOQFL'))

# identify the minimum non-zero value for AVAL for each PARAMCD.
# non-zero minimum value used for log2 transformed analysis values
PARAM_MINS <- ALB_SUBSET %>%
  select(USUBJID, PARAMCD, AVAL) %>%
  filter(PARAMCD %in% param_choices & AVAL > 0) %>%
  group_by(PARAMCD) %>%
  summarise(AVAL_MIN=min(AVAL, na.rm=TRUE))

# post process the data to create several new variables and adjust existing record specific valules per specification
# - create a visit code variable - week records coded to "W NN"
# - adjust existing BASELINE record values where values are missing: According to SPA this is a STREAM artifact
ALB_SUPED1 <- ALB_SUBSET %>% 
  mutate(AVISITCD = case_when(
    AVISIT == 'SCREENING' ~ 'SCR',
    AVISIT == 'BASELINE' ~ 'BL',
    grepl("WEEK", AVISIT) ~ paste("W",trimws(substr(trimws(AVISIT), start=6, stop=7))),
    TRUE ~ as.character(NA)
  )) %>%
  mutate(AVISITCDN =  ifelse(trimws(AVISITCD) == "BL", 0, substr(AVISITCD, start=2, stop=4))) %>%
  mutate(AVISITCDN =  as.numeric(ifelse(trimws(AVISITCD) == "SCR", -1, AVISITCDN))) %>%
  
  mutate(BASE2 = ifelse(AVISIT == "SCREENING" & is.na(BASE2), AVAL, BASE2)) %>%
  mutate(CHG2 = ifelse(AVISIT == "SCREENING" & is.na(CHG2), 0, CHG2)) %>%
  mutate(PCHG2 = ifelse(AVISIT == "SCREENING" & is.na(PCHG2), 0, PCHG2)) %>%

  mutate(BASE = ifelse(AVISIT == "BASELINE" & is.na(BASE), AVAL, BASE)) %>%
  mutate(CHG = ifelse(AVISIT == "BASELINE" & is.na(CHG), 0, CHG)) %>%
  mutate(PCHG = ifelse(AVISIT == "BASELINE" & is.na(PCHG), 0, PCHG)) %>%
  
  mutate(TRTORD = ifelse(grepl("C", ARMCD), 1, ifelse(grepl("B", ARMCD), 2, ifelse(grepl("A", ARMCD), 3, NA))))

# merge minimum AVAL value onto the ALB data to calculate the log2 variables. preserve the variable order
ALB_SUPED2 <- merge(PARAM_MINS, ALB_SUPED1, by="PARAMCD", all=TRUE)[, union(names(ALB_SUPED1), names(PARAM_MINS))] %>%
  # visit values
  mutate(AVALL2 = ifelse(PARAMCD %in% exclude_l2, AVAL, # excludes biomarkers where log2 is not appropriate: for example assay value already log2
                         ifelse(PARAMCD %in% exclude_chg, NA, # excludes biomarkers where log2 is not appropriate: for example CHG type assay
                                ifelse(AVAL == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                       ifelse(AVAL == 0 & AVAL_MIN <= 0, NA, # would be taking log2 of 0 or negative value so set to NA
                                              ifelse(AVAL > 0, log2(AVAL), NA)))))) %>%
  # baseline values
  mutate(BASEL2 = ifelse(PARAMCD %in% exclude_l2, BASE,
                         ifelse(PARAMCD %in% exclude_chg, NA,     
                                ifelse(BASE == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                       ifelse(BASE == 0 & AVAL_MIN <= 0, NA,
                                              ifelse(BASE > 0, log2(BASE), NA)))))) %>%
  # screening
  mutate(BASE2L2 = ifelse(PARAMCD %in% exclude_l2, BASE2,
                          ifelse(PARAMCD %in% exclude_chg, NA,      
                                 ifelse(BASE2 == 0 & AVAL_MIN > 0, log2(AVAL_MIN/2),
                                        ifelse(BASE2 == 0 & AVAL_MIN <= 0, NA,
                                               ifelse(BASE2 > 0, log2(BASE2), NA))))))

# create final data set used by goshawk
ALB <- ALB_SUPED2 %>% 
  mutate(AVISITCD = factor(AVISITCD) %>% reorder(AVISITCDN)) %>%
  mutate(ARMORVAL = ARM) %>%
  mutate(ARM = case_when(
    ARMCD == "C" ~ "Placebo",
    ARMCD == "B" ~ "150mg QD",
    ARMCD == "A" ~ "200mg BID",
    TRUE ~ as.character(NA))) %>% # need explicit 'N' value for LOQFL
  mutate(ARM = factor(ARM) %>% reorder(TRTORD)) %>%
  mutate(LOQFL = ifelse(LOQFL == 'Y', LOQFL, 'N'))
