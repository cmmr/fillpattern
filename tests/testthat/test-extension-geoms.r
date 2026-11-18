# Issues #7 and #8: patterns must work in geoms that don't call
# ggplot2::fill_alpha(), and scale_fill_pattern() must accept
# `aesthetics` and `...`.


# Render to a real file: pdf(NULL) doesn't resolve pattern fills.
# Usage: on.exit(local_pdf(), add = TRUE)
local_pdf <- function () {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tf)
  function () { grDevices::dev.off(); unlink(tf) }
}

# A `fun` hook that records the foreground colour of every pattern drawn.
fg_recorder <- function () {
  rec <- new.env()
  rec$fg <- character(0)
  rec$hook <- function (env, row) { rec$fg <- c(rec$fg, env$fg); NULL }
  rec
}


test_that("palette returns real grid patterns", {
  pal <- scale_fill_pattern(patterns = c("brick", "fish"))$palette
  res <- pal(2)

  for (p in res) {
    expect_true(inherits(p, "GridPattern"))
    expect_s3_class(p, c("GridFillPattern", "GridTilingPattern", "GridPattern"), exact = TRUE)
    expect_false(p$group)
    expect_true(is.function(p$f))
  }

  expect_equal(attr(res[[1]], "fillpattern_args")$patterns, "brick")
  expect_equal(attr(res[[2]], "fillpattern_args")$patterns, "fish")
})


test_that("legacy plain-list GridFillPattern objects are still accepted", {
  args <- structure(
    class = c('GridFillPattern', 'GridPattern'),
    .Data = list(patterns = "fish", fg = "red", bg = "white") )
  res <- ggplot2::pattern_alpha(args, 1)
  expect_s3_class(res, "GridTilingPattern")
  expect_equal(environment(res$f)$grob$fg, "red")
})


test_that("geom that skips fill_alpha() draws and maps colour from outline", {
  library(ggplot2)
  done <- local_pdf(); on.exit(done(), add = TRUE)

  # Minimal extension geom: passes `fill` straight to the grob.
  GeomRaw <- ggproto("GeomRaw", Geom,
    required_aes = c("x", "y"),
    default_aes  = aes(colour = "black", fill = "grey"),
    draw_group   = function (data, panel_params, coord) {
      d <- coord$transform(data, panel_params)
      grid::polygonGrob(d$x, d$y, gp = grid::gpar(col = d$colour[1], fill = d$fill[1]))
    })

  df <- data.frame(
    x = c(1, 2, 2, 3, 4, 4), y = c(1, 1, 2, 1, 1, 2),
    g = rep(c("a", "b"), each = 3) )

  rec <- fg_recorder()
  p <- ggplot(df, aes(x, y, group = g, fill = g, colour = g)) +
    layer(geom = GeomRaw, stat = "identity", position = "identity") +
    scale_colour_manual(values = c(a = "#FF0000", b = "#0000FF")) +
    scale_fill_pattern(fun = rec$hook)

  expect_silent(print(p))
  expect_setequal(unique(rec$fg), c("#FF0000", "#0000FF"))

  # Fixed colours work too.
  p <- ggplot(df, aes(x, y, group = g, fill = g)) +
    layer(geom = GeomRaw, stat = "identity", position = "identity") +
    scale_fill_pattern(fg = "black", bg = "white")
  expect_silent(print(p))
})


test_that("fill_alpha() geoms still map colour per row", {
  library(ggplot2)
  done <- local_pdf(); on.exit(done(), add = TRUE)

  rec <- fg_recorder()
  df  <- data.frame(x = c("a", "b"), y = 1:2)
  p   <- ggplot(df, aes(x, y, fill = x, colour = x)) +
    geom_col() +
    scale_colour_manual(values = c(a = "#FF0000", b = "#0000FF")) +
    scale_fill_pattern(fun = rec$hook)

  print(p)

  # geom_col draws one vectorised rectGrob, so these colours can only have
  # come from the per-row lookup in pattern_alpha(), not from the outline.
  expect_setequal(unique(rec$fg), c("#FF0000", "#0000FF"))
})


test_that("scale_fill_pattern() passes `...` to discrete_scale() (#8)", {
  library(ggplot2)
  done <- local_pdf(); on.exit(done(), add = TRUE)

  sc <- scale_fill_pattern(name = "Drive", labels = c("4WD", "Front", "Rear"))
  expect_equal(sc$name, "Drive")
  expect_equal(sc$labels, c("4WD", "Front", "Rear"))
  expect_length(sc$palette(3), 3)

  p <- ggplot(mpg, aes(drv, fill = drv)) + geom_bar() + sc
  expect_silent(print(p))
})


test_that("scale_fill_pattern() supports other aesthetics (#8)", {
  sc <- scale_fill_pattern(aesthetics = "pattern_fill")
  expect_equal(sc$aesthetics, "pattern_fill")
})


test_that("fill_pattern(fg = NA) inherits the shape's col", {
  done <- local_pdf(); on.exit(done(), add = TRUE)

  rec  <- fg_recorder()
  fill <- fill_pattern("stripe", fg = NA, fun = rec$hook)

  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(col = "red", fill = fill))
  expect_equal(rec$fg, "red")

  # Several shapes in one grob, all one colour: still unambiguous.
  rec$fg <- character(0)
  grid::grid.rect(x = c(1, 3) / 4, width = 0.2, gp = grid::gpar(col = c("red", "red"), fill = fill))
  expect_equal(unique(rec$fg), "red")

  # Several colours in one grob: ambiguous, fall back to black.
  rec$fg <- character(0)
  grid::grid.rect(x = c(1, 3) / 4, width = 0.2, gp = grid::gpar(col = c("red", "blue"), fill = fill))
  expect_equal(unique(rec$fg), "black")

  # No outline colour: fall back to black.
  rec$fg <- character(0)
  grid::grid.rect(gp = grid::gpar(col = NA, fill = fill))
  expect_equal(unique(rec$fg), "black")
})


test_that("fill_pattern(bg = NA) inherits the shape's col", {
  done <- local_pdf(); on.exit(done(), add = TRUE)

  bg  <- NULL
  hook <- function (env, row) { bg <<- env$bg$gp$fill; NULL }
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(col = "red", fill = fill_pattern(bg = NA, fun = hook)))
  expect_equal(bg, "red")
})


test_that("lighten() treats transparent colours as missing", {
  li <- fillpattern:::lighten
  expect_equal(li("transparent", "black", 1, 1), "black")
  expect_equal(li("#FF000000",   "white", 1, 1), "white")
  expect_equal(li(NULL,          "black", 1, 1), "black")
  expect_equal(li("red",         "black", 1, 1), "red")
})


test_that("aes_row() returns an empty list outside of fill_alpha()", {
  expect_identical(fillpattern:::aes_row(1L), list())
})


test_that("fill_alpha() called without layer data leaves NA colours for grid", {
  fills  <- scale_fill_pattern()$palette(1)
  caller <- function () ggplot2::fill_alpha(fills, 1)
  res    <- caller()[[1]]
  expect_s3_class(res, "GridTilingPattern")
  expect_true(is.na(environment(res$f)$grob$fg))
})
