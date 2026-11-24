# Helper for identifying any `LLOQ` and `ULOQ` values in `LBSTRESC`. Outcome drives horizontal line functionality display and legend labeling along with display of values in footnote.

Helper for identifying any `LLOQ` and `ULOQ` values in `LBSTRESC`.
Outcome drives horizontal line functionality display and legend labeling
along with display of values in footnote.

## Usage

``` r
h_identify_loq_values(loqs_data, flag_var)
```

## Arguments

- loqs_data:

  (`data frame`)\
  `loqs_data` data set containing assay data with potential `LOQ` values

## Details

Biomarker Sciences would like to have `LLOQ` and `ULOQ` values available
for reference in the visualizations. This also aids in setting the data
constraint ranges when `goshawk` functions are run from `teal.goshawk`
`UI`.

## Examples

``` r
goshawk:::h_identify_loq_values(loqs_data = goshawk::rADLB, flag_var = "LOQFL")
#>                                  PARAM LLOQC LLOQN ULOQC ULOQN
#> 1 Alanine Aminotransferase Measurement    <7     7  <NA>    NA
#> 2       C-Reactive Protein Measurement    <8     8  <NA>    NA
#> 3         Immunoglobulin A Measurement  <NA>    NA    >3     3
```
