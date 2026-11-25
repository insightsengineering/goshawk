# Check that argument is a valid color

Checks if the argument can be converted to valid RGB values space. See
[`grDevices::col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html).

## Usage

``` r
check_color(
  color,
  min.len = NULL,
  max.len = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  null.ok = FALSE
)

assert_color(
  color,
  min.len = NULL,
  max.len = NULL,
  any.missing = TRUE,
  all.missing = TRUE,
  len = NULL,
  null.ok = FALSE,
  .var.name = checkmate::vname(color),
  add = NULL
)
```

## Arguments

- color:

  (`character`)\
  Valid color convertible to RGB scale by
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html)

- min.len:

  \[`integer(1)`\]\
  Minimal length of `x`.

- max.len:

  \[`integer(1)`\]\
  Maximal length of `x`.

- any.missing:

  \[`logical(1)`\]\
  Are vectors with missing values allowed? Default is `TRUE`.

- all.missing:

  \[`logical(1)`\]\
  Are vectors with no non-missing values allowed? Default is `TRUE`.
  Note that empty vectors do not have non-missing values.

- len:

  \[`integer(1)`\]\
  Exact expected length of `x`.

- null.ok:

  \[`logical(1)`\]\
  If set to `TRUE`, `x` may also be `NULL`. In this case only a type
  check of `x` is performed, all additional checks are disabled.

- .var.name:

  \[`character(1)`\]\
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]\
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Depending on the function prefix: If the check is successful, the
functions `assertCharacter`/`assert_character` return `x` invisibly,
whereas `checkCharacter`/`check_character` and
`testCharacter`/`test_character` return `TRUE`. If the check is not
successful, `assertCharacter`/`assert_character` throws an error
message, `testCharacter`/`test_character` returns `FALSE`, and
`checkCharacter`/`check_character` return a string with the error
message. The function `expect_character` always returns an
[`expectation`](https://testthat.r-lib.org/reference/expectation.html).
