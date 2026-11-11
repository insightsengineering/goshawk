adlb <- make_adlb()

test_that("h_identify_loq_values finds the first LLOQ and ULOQ per parameter", {
  res <- goshawk:::h_identify_loq_values(adlb, "LOQFL")
  expect_s3_class(res, "data.frame")
  expect_setequal(res$PARAM, c("C-reactive protein", "Alanine transaminase"))
  expect_equal(res$LLOQN[res$PARAM == "C-reactive protein"], 0.5)
  expect_equal(res$ULOQN[res$PARAM == "C-reactive protein"], 1.5)
  expect_equal(res$LLOQN[res$PARAM == "Alanine transaminase"], 20)
  expect_equal(res$ULOQN[res$PARAM == "Alanine transaminase"], 60)
  expect_equal(attr(res$LLOQN, "label"), "Lower Limit of Quantitation")
  expect_equal(attr(res$ULOQC, "label"), "Upper Limit of Quantitation (C)")
})

test_that("h_identify_loq_values returns NA limits without any LOQ", {
  no_loq <- adlb
  no_loq$LOQFL <- "N"
  res <- goshawk:::h_identify_loq_values(no_loq, "LOQFL")
  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$LLOQN)))
  expect_true(all(is.na(res$ULOQN)))
})

test_that("h_caption_loqs_label describes the limits of quantification", {
  crp <- adlb[adlb$PARAMCD == "CRP", ]
  caption <- goshawk:::h_caption_loqs_label(crp, "LOQFL")
  expect_length(caption, nrow(crp))
  expect_equal(
    unique(caption),
    "Limits of quantification read from study data for C-reactive protein: LLOQ is < 0.5, ULOQ is > 1.5"
  )

  no_loq <- crp
  no_loq$LOQFL <- "N"
  expect_match(
    unique(goshawk:::h_caption_loqs_label(no_loq, "LOQFL")),
    "LLOQ is NA, ULOQ is NA"
  )
})

test_that("check_color accepts valid colors and rejects invalid ones", {
  expect_true(goshawk:::check_color("red"))
  expect_true(goshawk:::check_color(c("red", "#00FF00")))
  expect_match(goshawk:::check_color("notacolor"), "valid color")
  expect_false(isTRUE(goshawk:::check_color(1)))
  expect_false(isTRUE(goshawk:::check_color(c("red", "blue"), len = 1)))
})

test_that("assert_color errors for invalid colors only", {
  expect_silent(goshawk:::assert_color("red"))
  expect_error(goshawk:::assert_color("notacolor"))
})
