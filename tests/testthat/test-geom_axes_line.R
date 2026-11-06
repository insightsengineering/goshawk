# Helper functions adding horizontal and vertical lines to plots.
line_data <- data.frame(
  x = seq_len(10),
  y = seq_len(10),
  lower = 2,
  upper = 7,
  varying = seq_len(10),
  chr = "a",
  stringsAsFactors = FALSE
)

test_that("validate_line_args returns the values, colors and labels", {
  res <- goshawk:::validate_line_args(line_data, vars = c("lower", "upper"), color = c("red", "blue"))
  expect_s3_class(res, "data.frame")
  expect_equal(res$values, c(2, 7))
  expect_equal(res$colors, c("red", "blue"))
  expect_equal(res$labels, c("lower", "upper"))
})

test_that("validate_line_args uses variable labels and a single color", {
  labelled <- line_data
  attr(labelled$lower, "label") <- "Lower limit"
  res <- goshawk:::validate_line_args(labelled, vars = c("lower", "upper"), color = "green")
  expect_equal(res$labels, c("Lower limit", "upper"))
  expect_equal(res$colors, c("green", "green"))
})

test_that("validate_line_args returns an empty data frame without variables", {
  res <- goshawk:::validate_line_args(line_data)
  expect_equal(nrow(res), 0)
  expect_named(res, c("values", "colors", "labels"))
})

test_that("validate_line_args warns when a variable has several values", {
  expect_warning(
    res <- goshawk:::validate_line_args(line_data, vars = "varying"),
    "First value is taken from variable 'varying'",
    fixed = TRUE
  )
  expect_equal(res$values, 1)
})

test_that("validate_line_args validates its input", {
  expect_error(goshawk:::validate_line_args(line_data, vars = "missing"))
  expect_error(goshawk:::validate_line_args(line_data, vars = "chr"))
  expect_error(goshawk:::validate_line_args(line_data, vars = "lower", color = "notacolor"))
  expect_error(goshawk:::validate_line_args(line_data, vars = c("lower", "upper"), color = c("red", "blue", "green")))
  expect_error(goshawk:::validate_line_args(line_data, vars = "lower", label = c("a", "b")))
})

test_that("geom_axes_lines returns no layers by default", {
  expect_length(goshawk:::geom_axes_lines(line_data), 0)
})

test_that("geom_axes_lines builds the requested layers", {
  res <- goshawk:::geom_axes_lines(
    line_data,
    hline_arb = c(1, 2),
    hline_arb_color = c("red", "blue"),
    hline_arb_label = c("a", "b"),
    hline_vars = c("lower", "upper"),
    vline_arb = 3,
    vline_vars = "upper"
  )
  expect_length(res, 4)
  expect_true(all(vapply(res, is.list, logical(1))))

  p <- ggplot2::ggplot(line_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    res
  expect_s3_class(p, "ggplot")
})

test_that("geom_arb_hline and geom_arb_vline return a new scale, a line and a color scale", {
  h <- goshawk:::geom_arb_hline(yintercept = c(2, 5), color = "blue", label = c("h1", "h2"), linetype = 2)
  expect_length(h, 3)
  expect_s3_class(h[[2]]$geom, "GeomHline")
  v <- goshawk:::geom_arb_vline(xintercept = c(2, 5), color = c("blue", "red"), label = "v", linetype = 2)
  expect_length(v, 3)
  expect_s3_class(v[[2]]$geom, "GeomVline")
})

test_that("geom_arb_hline and geom_arb_vline validate their input", {
  expect_error(goshawk:::geom_arb_hline(yintercept = "a"))
  expect_error(goshawk:::geom_arb_hline(yintercept = 1, color = "notacolor"))
  expect_error(goshawk:::geom_arb_hline(yintercept = 1, label = c("a", "b")))
  expect_error(goshawk:::geom_arb_vline(xintercept = "a"))
  expect_error(goshawk:::geom_arb_vline(xintercept = 1, color = "notacolor"))
  expect_error(goshawk:::geom_arb_vline(xintercept = 1, label = c("a", "b")))
})

test_that("geom_range_hline and geom_range_vline use variables from the data", {
  h <- goshawk:::geom_range_hline(vars = c("lower", "upper"), data = line_data, color = "blue", linetype = 2)
  expect_length(h, 3)
  v <- goshawk:::geom_range_vline(vars = c("lower", "upper"), data = line_data, color = "blue", linetype = 2)
  expect_length(v, 3)
})
