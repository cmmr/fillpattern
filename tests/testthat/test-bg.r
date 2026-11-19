
# `bg` can be a grid pattern (gradient, tiling pattern, another fill_pattern())
# or a grob. These are lists, but must be treated as one value (#10).


# A `fun` hook that records what each drawn pattern used for its background.
bg_recorder <- function () {
  rec <- new.env()
  rec$bg <- list()
  rec$fun <- function (env, row) {
    rec$bg[[length(rec$bg) + 1]] <- env$bg
    NULL
  }
  rec
}

# Fill of a background rect, or the background grob itself.
bg_fill <- function (bg) if (inherits(bg, "rect")) bg$gp$fill else bg


test_that("fillPatternGrob() accepts a grid pattern or grob as bg", {
  grad <- grid::linearGradient(c("red", "yellow"))
  rect <- grid::rectGrob(gp = grid::gpar(fill = "pink"))

  expect_identical(fillPatternGrob(bg = grad)$bg, grad)
  expect_identical(fillPatternGrob(bg = rect)$bg, rect)

  # Other arguments still need length 1.
  expect_error(fillPatternGrob(bg = c("red", "blue")), "should be one value")
  expect_error(fillPatternGrob(fg = list("red", "blue")), "should be one value")
})


test_that("makeContent() draws a grid pattern or grob bg", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  grad <- grid::radialGradient(c("red", "yellow"))
  g <- grid::makeContent(fillPatternGrob(pattern = "empty", bg = grad))
  expect_length(g$children, 1)
  expect_s3_class(g$children[[1]], "rect")
  expect_identical(g$children[[1]]$gp$fill, grad)

  # A grob is drawn as-is, not wrapped in rectGrob(gp = gpar(fill = <grob>)),
  # which would error and fall back to a solid fg rect.
  circ <- grid::circleGrob(gp = grid::gpar(fill = "pink"))
  g <- grid::makeContent(fillPatternGrob(pattern = "grid", fg = "red", bg = circ))
  expect_length(g$children, 2)
  expect_identical(g$children[[1]], circ)
})


test_that("fill_pattern() recycles grid patterns and grobs as single values", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  grad <- grid::linearGradient(c("red", "yellow"))
  tile <- grid::pattern(
    grid::circleGrob(r = 0.25, gp = grid::gpar(fill = "blue")),
    width = 0.1, height = 0.1, extend = "repeat" )
  pat  <- fill_pattern("grid", fg = "red")[[1]]
  circ <- grid::circleGrob(gp = grid::gpar(fill = "pink"))

  draw <- function (...) {
    rec   <- bg_recorder()
    fills <- fill_pattern(..., fun = rec$fun)
    grid::grid.newpage()
    for (f in fills) grid::grid.rect(gp = grid::gpar(fill = f))
    lapply(rec$bg, bg_fill)
  }

  pats <- c("brick", "fish", "grid")

  # Passed directly: every pattern gets the whole object.
  expect_identical(draw(pats, bg = grad), list(grad, grad, grad))
  expect_identical(draw(pats, bg = tile), list(tile, tile, tile))
  expect_identical(draw(pats, bg = pat),  list(pat,  pat,  pat))
  expect_identical(draw(pats, bg = circ), list(circ, circ, circ))

  # In a list: recycled per pattern.
  expect_identical(
    draw(pats, bg = list(grad, "cyan")),
    list(grad, "cyan", grad) )
  expect_identical(
    draw(pats, bg = list(circ, pat, "cyan")),
    list(circ, pat, "cyan") )

  # The old list(list(...)) workaround keeps working.
  res <- draw(pats[1:2], bg = list(list(grad), "cyan"))
  expect_identical(unclass(res[[1]]), list(grad))
  expect_identical(res[[2]], "cyan")
})


test_that("scale_fill_pattern() recycles grid patterns as single values", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  library(ggplot2)
  grad <- grid::linearGradient(c("red", "yellow"))
  p    <- ggplot(mpg, aes(drv, fill = drv)) + geom_bar()

  draw <- function (...) {
    rec <- bg_recorder()
    print(p + scale_fill_pattern(fg = "black", ..., fun = rec$fun))
    # Drop the legend keys: same backgrounds again.
    unique(lapply(rec$bg, bg_fill))
  }

  expect_identical(draw(bg = grad),                list(grad))
  expect_identical(draw(bg = list(grad, "cyan")),  list(grad, "cyan"))
})
