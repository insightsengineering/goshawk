library("dplyr")

# Deterministic, self-contained lab data (ADaM-like) used by all tests.
# 12 subjects (4 per arm), 3 visits, 2 parameters (CRP, ALT).
make_adlb <- function() {
  arms <- c("A: Drug X", "B: Placebo", "C: Combination")
  subj <- data.frame(
    USUBJID = sprintf("S-%02d", 1:12),
    ARM = rep(arms, each = 4),
    SEX = rep(c("F", "M"), 6),
    stringsAsFactors = FALSE
  )
  visits <- data.frame(
    AVISITCD = c("BL", "W 1", "W 5"),
    AVISITCDN = c(0, 1, 5),
    stringsAsFactors = FALSE
  )
  params <- data.frame(
    PARAMCD = c("CRP", "ALT"),
    PARAM = c("C-reactive protein", "Alanine transaminase"),
    AVALU = c("mg/L", "U/L"),
    ANRLO = c(0.5, 20),
    ANRHI = c(1.5, 60),
    stringsAsFactors = FALSE
  )
  dat <- merge(merge(subj, visits, by = NULL), params, by = NULL)
  dat <- arrange(dat, PARAMCD, USUBJID, AVISITCDN)
  rownames(dat) <- NULL

  idx <- seq_len(nrow(dat))
  scale_p <- ifelse(dat$PARAMCD == "CRP", 1, 40)
  dat$AVAL <- round(scale_p * (0.6 + 0.05 * (idx %% 7) + 0.1 * dat$AVISITCDN + 0.02 * sin(idx)), 3)
  dat$BASE <- round(scale_p * (0.7 + 0.04 * (idx %% 5) + 0.03 * cos(idx)), 3)
  dat$STUDYID <- "STUDY-1"
  dat$ARMCD <- c("A", "B", "C")[match(dat$ARM, arms)]
  dat$TRTORD <- match(dat$ARM, arms)
  dat$LBSTRESC <- as.character(dat$AVAL)
  dat$LOQFL <- "N"

  # one lower and one upper limit of quantification per parameter
  lloq <- dat$USUBJID == "S-01" & dat$AVISITCD == "BL"
  uloq <- dat$USUBJID == "S-02" & dat$AVISITCD == "W 1"
  dat$LBSTRESC[lloq & dat$PARAMCD == "CRP"] <- "< 0.5"
  dat$LBSTRESC[uloq & dat$PARAMCD == "CRP"] <- "> 1.5"
  dat$LBSTRESC[lloq & dat$PARAMCD == "ALT"] <- "< 20"
  dat$LBSTRESC[uloq & dat$PARAMCD == "ALT"] <- "> 60"
  dat$LOQFL[lloq | uloq] <- "Y"

  # one missing CRP result: S-03 at W 5
  dat$AVAL[dat$USUBJID == "S-03" & dat$AVISITCD == "W 5" & dat$PARAMCD == "CRP"] <- NA_real_

  dat$ARM <- factor(dat$ARM, levels = arms)
  attr(dat$ARM, "label") <- "Description of Planned Arm"
  dat$SEX <- factor(dat$SEX)
  attr(dat$ANRLO, "label") <- "Analysis Normal Range Lower Limit"
  attr(dat$ANRHI, "label") <- "Analysis Normal Range Upper Limit"

  loqs <- dat %>% filter(LOQFL == "Y") %>% select(PARAM, LBSTRESC) %>%
    mutate(LBSTRESC_n = as.numeric(gsub("[^[:digit:].]", "", LBSTRESC))) %>%
    summarise(.by = PARAM,
              LLOQC = LBSTRESC[which.min(LBSTRESC_n)],
              LLOQN = min(LBSTRESC_n),
              ULOQC = LBSTRESC[which.max(LBSTRESC_n)],
              ULOQN = max(LBSTRESC_n)
              )
  # Equivalent to loqs <- goshawk:::h_identify_loq_values(dat, "LOQFL")
  #                  PARAM LLOQC LLOQN ULOQC ULOQN
  # 1 Alanine transaminase  < 20  20.0  > 60  60.0
  # 2   C-reactive protein < 0.5   0.5 > 1.5   1.5
  dat <- merge(dat, loqs, by = "PARAM", sort = FALSE)
  dat <- dat[order(dat$PARAMCD, dat$USUBJID, dat$AVISITCDN), ]
  rownames(dat) <- NULL
  dat
}

arm_colors <- c(
  "A: Drug X" = "#000000",
  "B: Placebo" = "#3498DB",
  "C: Combination" = "#E74C3C"
)

# Wide-format data for g_correlationplot: one row per subject and visit.
make_corr_data <- function() {
  adlb <- make_adlb()
  crp <- adlb[adlb$PARAMCD == "CRP", ]
  alt <- adlb[adlb$PARAMCD == "ALT", ]
  key <- c("USUBJID", "AVISITCD")
  dat <- merge(
    crp[, c(key, "ARM", "AVAL", "LBSTRESC", "PARAM", "LOQFL", "ANRHI", "ANRLO", "ULOQN", "LLOQN")],
    alt[, c(key, "BASE", "LBSTRESC", "PARAM", "LOQFL", "ANRHI", "ANRLO", "ULOQN", "LLOQN")],
    by = key,
    suffixes = c(".CRP", ".ALT")
  )
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  dat$LOQFL_COMB <- ifelse(dat$LOQFL_CRP == "Y" | dat$LOQFL_ALT == "Y", "Y", "N")
  dat <- dat[!is.na(dat$AVAL) & !is.na(dat$BASE), ]
  dat$ARM <- factor(dat$ARM, levels = levels(adlb$ARM))
  attr(dat$ARM, "label") <- "Description of Planned Arm"
  rownames(dat) <- NULL
  dat
}
