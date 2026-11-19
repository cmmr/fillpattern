
# Exported pattern names, palette, and lwd = 0 (gridpattern#141).


test_that("fill_pattern_names lists the base pattern names", {
  expect_type(fill_pattern_names, "character")
  expect_length(fill_pattern_names, 14)
  expect_false(anyDuplicated(fill_pattern_names) > 0)

  for (p in fill_pattern_names) {
    res <- fillpattern:::parse_pattern(p)
    expect_length(res$problems, 0)
    expect_equal(res$name, p)
  }

  # Every base name draws a pattern, not the error fallback.
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  for (p in setdiff(fill_pattern_names, "empty")) {
    g <- expect_no_warning(grid::makeContent(fillPatternGrob(p, bg = "white")))
    expect_length(g$children, 2)
  }
})


test_that("fill_pattern_order is what numeric patterns map to", {
  expect_type(fill_pattern_order, "character")
  expect_length(fill_pattern_order, 35)

  pp <- fillpattern:::parse_pattern
  for (p in fill_pattern_order)
    expect_length(pp(p)$problems, 0)

  for (i in c(1:36, 70, 71)) {
    by_num  <- pp(i)
    by_name <- pp(fill_pattern_order[[(i - 1) %% 35 + 1]])
    expect_equal(by_num[c("name", "angle")], by_name[c("name", "angle")])
  }

  # The scale's default `patterns = seq_len` uses them in order.
  pats <- scale_fill_pattern()$palette(3)
  args <- lapply(pats, attr, "fillpattern_args")
  expect_equal(vapply(args, function (a) pp(a$patterns)$name, ""), c("brick", "fish", "stripe"))
})


test_that("pal_fill_pattern() returns the scale's palette", {
  pal <- pal_fill_pattern(c("brick", "fish"), fg = "black", lty = c("dashed", "44"))
  expect_true(is.function(pal))

  fills <- pal(3)
  expect_length(fills, 3)
  for (f in fills) expect_s3_class(f, c("GridFillPattern", "GridPattern"))

  args <- lapply(fills, attr, "fillpattern_args")
  expect_equal(vapply(args, `[[`, "", "patterns"), c("brick", "fish", "brick"))
  expect_equal(vapply(args, `[[`, "", "lty"),      c("dashed", "44", "dashed"))

  # Same patterns as the scale built from the same arguments.
  sc <- scale_fill_pattern(c("brick", "fish"), fg = "black", lty = c("dashed", "44"))
  expect_equal(
    lapply(sc$palette(3), attr, "fillpattern_args"),
    args )

  # Arguments can still be functions of n, and names are kept.
  pal <- pal_fill_pattern(patterns = c(A = "brick", B = "grid"), lwd = function (n) seq_len(n))
  fills <- pal(2)
  expect_named(fills, c("A", "B"))
  expect_equal(attr(fills[[2]], "fillpattern_args")$lwd, 2)

  # Invalid values warn when the palette is called.
  expect_warning(pal_fill_pattern("stripes45")(1), "stripes45")
})


test_that("pal_fill_pattern() works as a theme palette", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  library(ggplot2)
  seen <- character(0)
  hook <- function (env, row) { seen <<- c(seen, env$pattern); NULL }
  p <- ggplot(mpg, aes(drv, fill = drv)) +
    geom_bar() +
    theme(palette.fill.discrete = pal_fill_pattern(
      c("brick", "fish", "grid"), fg = "black", fun = hook ))
  expect_no_warning(print(p))
  expect_setequal(unique(seen), c("brick", "fish", "grid"))
})


test_that("lwd = 0 draws no pattern lines", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  g <- expect_silent(fillPatternGrob(lwd = 0, bg = "white"))
  expect_equal(g$lwd, 0)
  g <- grid::makeContent(g)
  expect_length(g$children, 2)
  expect_equal(g$children[[2]]$gp$lty, "blank")

  # The "_0lwd" modifier does the same.
  g <- expect_silent(fillPatternGrob("grid_0lwd", bg = "white"))
  g <- grid::makeContent(g)
  expect_equal(g$children[[2]]$gp$lty, "blank")
  expect_equal(g$children[[2]]$gp$lwd, 0)

  # Other line widths keep their line type.
  g <- grid::makeContent(fillPatternGrob("grid_0.5dashed", bg = "white"))
  expect_equal(g$children[[2]]$gp$lty, "dashed")

  # Negative widths are still invalid.
  expect_warning(g <- fillPatternGrob(lwd = -1), class = "fillpattern_invalid_arg")
  expect_equal(g$lwd, 1)
})
