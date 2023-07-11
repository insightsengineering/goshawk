library(scda)
rADLB <- synthetic_cdisc_data("latest")$adlb #nolint
usethis::use_data(rADLB)

rADSL <- synthetic_cdisc_data("latest")$adsl #nolint
usethis::use_data(rADSL)
