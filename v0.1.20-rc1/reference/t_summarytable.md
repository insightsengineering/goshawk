# Function to create a table of descriptive summary statistics to accompany plots.

Output descriptive summary statistics table as a data frame. Includes
biomarker, treatment, visit, n, mean, median, SD, min, max, %missing
values, % `LOQ` values.

## Usage

``` r
t_summarytable(
  data,
  trt_group,
  param_var,
  param,
  xaxis_var,
  facet_var = "AVISITCD",
  loq_flag_var = "LOQFL",
  ...
)
```

## Arguments

- data:

  name of data frame to summarize.

- trt_group:

  treatment group variable name e.g. `ARM`.

- param_var:

  name of variable containing biomarker codes e.g. `PARAMCD`.

- param:

  biomarker to visualize e.g. `IGG`.

- xaxis_var:

  name of variable containing biomarker results displayed on X-axis e.g.
  `AVAL`.

- facet_var:

  name of variable facetted on typically containing visit values e.g.
  `AVISITCD`. If NULL then ignored. It defaults to `"AVISITCD"` when not
  provided.

- loq_flag_var:

  name of variable containing `LOQ` flag e.g. `LOQFL`. Defaults to
  `"LOQFL"`.

- ...:

  additional options

## Details

provide additional information as needed. link to specification file
<https://posit.co/>

## Author

Nick Paszty (npaszty) paszty.nicholas@gene.com

Balazs Toth (tothb2) toth.balazs@gene.com

## Examples

``` r
# Example using ADaM structure analysis dataset.

library(stringr)

# original ARM value = dose value
arm_mapping <- list(
  "A: Drug X" = "150mg QD", "B: Placebo" = "Placebo", "C: Combination" = "Combination"
)

ADLB <- rADLB
ADLB <- ADLB %>%
  mutate(AVISITCD = case_when(
    AVISIT == "SCREENING" ~ "SCR",
    AVISIT == "BASELINE" ~ "BL",
    grepl("WEEK", AVISIT) ~
      paste(
        "W",
        trimws(
          substr(
            AVISIT,
            start = 6,
            stop = str_locate(AVISIT, "DAY") - 1
          )
        )
      ),
    TRUE ~ NA_character_
  )) %>%
  mutate(AVISITCDN = case_when(
    AVISITCD == "SCR" ~ -2,
    AVISITCD == "BL" ~ 0,
    grepl("W", AVISITCD) ~ as.numeric(gsub("\\D+", "", AVISITCD)),
    TRUE ~ NA_real_
  )) %>%
  # use ARMCD values to order treatment in visualization legend
  mutate(TRTORD = ifelse(grepl("C", ARMCD), 1,
    ifelse(grepl("B", ARMCD), 2,
      ifelse(grepl("A", ARMCD), 3, NA)
    )
  )) %>%
  mutate(ARM = as.character(arm_mapping[match(ARM, names(arm_mapping))])) %>%
  mutate(ARM = factor(ARM) %>%
    reorder(TRTORD))

tbl <- t_summarytable(
  data = ADLB,
  trt_group = "ARM",
  param_var = "PARAMCD",
  param = c("CRP"),
  xaxis_var = "AVAL",
  facet_var = "AVISITCD",
  loq_flag_var = "LOQFL"
)
tbl
#>    StudyID AnlVar Biomarker   Treatment Facet   n Mean Median StdDev  Min   Max
#> 1  AB12345   AVAL       CRP Combination    BL 132 8.98   8.96   0.89 6.24 11.18
#> 2  AB12345   AVAL       CRP     Placebo    BL 134 8.99   8.92   0.98 6.23 11.63
#> 3  AB12345   AVAL       CRP    150mg QD    BL 134 9.06   9.07   0.93 6.21 11.87
#> 4  AB12345   AVAL       CRP       Comb.    BL 400 9.01   8.96   0.93 6.21 11.87
#> 5  AB12345   AVAL       CRP Combination   SCR 132 9.22   9.20   1.01 6.51 11.91
#> 6  AB12345   AVAL       CRP     Placebo   SCR 134 8.94   8.96   0.87 6.79 11.90
#> 7  AB12345   AVAL       CRP    150mg QD   SCR 134 9.03   9.08   0.94 7.14 11.22
#> 8  AB12345   AVAL       CRP       Comb.   SCR 400 9.06   9.08   0.95 6.51 11.91
#> 9  AB12345   AVAL       CRP Combination   W 1 132 8.96   8.91   1.03 7.15 12.15
#> 10 AB12345   AVAL       CRP     Placebo   W 1 134 8.98   8.96   1.00 6.84 11.53
#> 11 AB12345   AVAL       CRP    150mg QD   W 1 134 9.01   8.93   1.03 6.37 11.33
#> 12 AB12345   AVAL       CRP       Comb.   W 1 400 8.98   8.93   1.02 6.37 12.15
#> 13 AB12345   AVAL       CRP Combination   W 2 132 9.12   9.17   0.96 6.68 11.54
#> 14 AB12345   AVAL       CRP     Placebo   W 2 134 8.99   8.99   1.05 6.55 12.73
#> 15 AB12345   AVAL       CRP    150mg QD   W 2 134 8.87   8.86   1.04 6.78 12.55
#> 16 AB12345   AVAL       CRP       Comb.   W 2 400 9.00   9.03   1.02 6.55 12.73
#> 17 AB12345   AVAL       CRP Combination   W 3 132 8.99   8.97   0.93 6.30 11.33
#> 18 AB12345   AVAL       CRP     Placebo   W 3 134 8.97   8.97   1.07 5.81 11.37
#> 19 AB12345   AVAL       CRP    150mg QD   W 3 134 9.08   9.09   0.96 6.46 11.60
#> 20 AB12345   AVAL       CRP       Comb.   W 3 400 9.02   9.01   0.99 5.81 11.60
#> 21 AB12345   AVAL       CRP Combination   W 4 132 8.97   8.88   0.95 7.16 11.96
#> 22 AB12345   AVAL       CRP     Placebo   W 4 134 9.04   9.04   0.90 6.42 11.21
#> 23 AB12345   AVAL       CRP    150mg QD   W 4 134 8.80   8.71   1.06 5.87 12.22
#> 24 AB12345   AVAL       CRP       Comb.   W 4 400 8.93   8.87   0.97 5.87 12.22
#> 25 AB12345   AVAL       CRP Combination   W 5 132 9.01   8.96   0.91 6.64 11.93
#> 26 AB12345   AVAL       CRP     Placebo   W 5 134 9.06   9.10   1.01 7.08 12.50
#> 27 AB12345   AVAL       CRP    150mg QD   W 5 134 9.04   8.98   1.04 6.92 12.12
#> 28 AB12345   AVAL       CRP       Comb.   W 5 400 9.03   9.00   0.99 6.64 12.50
#>    PctMiss PctLOQ
#> 1        0  15.15
#> 2        0  15.67
#> 3        0  11.19
#> 4        0  14.00
#> 5        0  11.36
#> 6        0  12.69
#> 7        0  14.93
#> 8        0  13.00
#> 9        0  23.48
#> 10       0  17.91
#> 11       0  13.43
#> 12       0  18.25
#> 13       0  12.12
#> 14       0  17.16
#> 15       0  20.90
#> 16       0  16.75
#> 17       0  13.64
#> 18       0  17.91
#> 19       0  11.19
#> 20       0  14.25
#> 21       0  15.91
#> 22       0  11.19
#> 23       0  24.63
#> 24       0  17.25
#> 25       0  15.91
#> 26       0  16.42
#> 27       0  14.18
#> 28       0  15.50
```
