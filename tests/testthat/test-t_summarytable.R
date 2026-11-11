adlb <- make_adlb()
stat_cols <- c("n", "Mean", "Median", "StdDev", "Min", "Max", "PctMiss", "PctLOQ")

test_that("t_summarytable summarises by treatment and visit", {
  tbl <- t_summarytable(
    data = adlb,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL",
    facet_var = "AVISITCD",
    loq_flag_var = "LOQFL"
  )
  expect_s3_class(tbl, "data.frame")
  expect_equal(
    names(tbl),
    c("StudyID", "AnlVar", "Biomarker", "Treatment", "Facet", stat_cols)
  )
  # arms + combined x visits
  expect_equal(nrow(tbl), (length(unique(adlb$ARM)) + 1) * length(unique(adlb$AVISITCD)))
  expect_true("Comb." %in% tbl$Treatment)
  expect_equal(unique(as.character(tbl$StudyID)), "STUDY-1")
  expect_equal(unique(as.character(tbl$AnlVar)), "AVAL")
  expect_equal(unique(as.character(tbl$Biomarker)), "CRP")
})

test_that("t_summarytable counts non-missing values and LOQ percentages", {
  tbl <- t_summarytable(
    data = adlb,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL"
  )
  comb <- tbl$Treatment == "Comb."
  expect_equal(tbl$n[comb & tbl$Facet == "BL"], 12)
  expect_equal(tbl$n[comb & tbl$Facet == "W 5"], 11)
  expect_equal(tbl$PctMiss[comb & tbl$Facet == "W 5"], round(100 / 12, 2))
  expect_equal(tbl$PctLOQ[comb & tbl$Facet == "BL"], round(100 / 12, 2))
  expect_equal(tbl$PctLOQ[comb & tbl$Facet == "W 5"], 0)
})

test_that("t_summarytable works without a facet variable", {
  tbl <- t_summarytable(
    data = adlb,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "ALT",
    xaxis_var = "AVAL",
    facet_var = NULL
  )
  expect_equal(names(tbl), c("StudyID", "AnlVar", "Biomarker", "Treatment", stat_cols))
  expect_equal(nrow(tbl), 4)
  expect_equal(tbl$n[tbl$Treatment == "Comb."], 36)
})

test_that("t_summarytable handles the treatment group being the facet variable", {
  tbl <- t_summarytable(
    data = adlb,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL",
    facet_var = "ARM"
  )
  expect_true("Facet" %in% names(tbl))
  expect_equal(nrow(tbl), length(unique(adlb$PARAM)) * length(unique(adlb$ARM)))
})

test_that("t_summarytable returns NA for min and max when all values are missing", {
  adlb_na <- adlb
  adlb_na$AVAL[adlb_na$PARAMCD == "CRP" & adlb_na$AVISITCD == "W 5" & adlb_na$ARM == "A: Drug X"] <- NA_real_
  tbl <- t_summarytable(
    data = adlb_na,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL"
  )
  row <- tbl$Treatment == "A: Drug X" & tbl$Facet == "W 5"
  expect_equal(tbl$n[row], 0)
  expect_true(is.na(tbl$Min[row]))
  expect_true(is.na(tbl$Max[row]))
  expect_equal(tbl$PctMiss[row], 100)
})

test_that("t_summarytable combines several studies in the study id", {
  adlb_2 <- adlb
  adlb_2$STUDYID[adlb_2$USUBJID %in% sprintf("S-%02d", 7:12)] <- "STUDY-2"
  tbl <- t_summarytable(
    data = adlb_2,
    trt_group = "ARM",
    param_var = "PARAMCD",
    param = "CRP",
    xaxis_var = "AVAL"
  )
  expect_equal(unique(as.character(tbl$StudyID)), "STUDY-1/STUDY-2")
})
