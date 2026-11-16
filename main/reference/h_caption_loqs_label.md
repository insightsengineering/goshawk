# Add footnote to identify `LLOQ` and `ULOQ` values identified from data

Add footnote to identify `LLOQ` and `ULOQ` values identified from data

## Usage

``` r
h_caption_loqs_label(loqs_data, flag_var)
```

## Arguments

- loqs_data:

  (`data frame`)\
  `loqs_data` data set containing assay data with potential `LOQ` values

## Examples

``` r
caption_label <- goshawk:::h_caption_loqs_label(loqs_data = goshawk::rADLB, flag_var = "LOQFL")
```
