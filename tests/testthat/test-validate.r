
# Invalid arguments are replaced with safe defaults, with a warning, when the
# pattern is created; not silently drawn as a solid block (#11).


test_that("parse_pattern() accepts every documented pattern spec", {
  pp <- fillpattern:::parse_pattern

  valid <- c(
    "brick", "her", "herring", "herringbone", "  Brick ", "brick45", "saw20",
    "fish180", "grid_4", "hex_5mm:0.1npc", "saw_sm", "brick_*2:/2",
    "shingle_0.5dashed", "wave_2solid", "oct_longdash", "grid_2lwd",
    "hex_lg:xl_2dotted", "grid45_dashed_1.4lwd_:6mm_sm:", "empty_xl",
    "brick20", "chevron_2dashed", "fish_dotted", "grid_longdash",
    "herringbone45", "hexagon_sm", "octagon_:sm", "rain90_xs",
    "saw_16mm:8mm", "shingle_3lwd", "rshingle_dotted", "stripe35_lg",
    "wave45_sm", "brick_.5lwd", "brick__lg" )

  for (p in valid)
    expect_length(pp(p)$problems, 0)

  for (i in c(1, 7, 35, 36, 100))
    expect_length(pp(i)$problems, 0)

  res <- pp("grid45_dashed_1.4lwd_:6mm_sm:")
  expect_equal(res$name,  "grid")
  expect_equal(res$angle, 45)
  expect_equal(res$lty,   "dashed")
  expect_equal(res$lwd,   1.4)
  expect_equal(res$sizes, list(c("", "6mm"), c("sm", "")))

  expect_equal(pp(3)$name,  "stripe")
  expect_equal(pp(3)$angle, 45)
})


test_that("parse_pattern() keeps the valid parts of an invalid spec", {
  pp <- fillpattern:::parse_pattern

  expect_equal(pp("stripes45")$clean,       "brick45")
  expect_equal(pp("s")$clean,               "brick") # ambiguous
  expect_equal(pp("fish_huge_dashed")$clean, "fish_dashed")
  expect_equal(pp("grid_0lwd_lg")$clean,    "grid_lg")
  expect_equal(pp("grid_1:2:3")$clean,      "grid")
  expect_equal(pp("grid_*0")$clean,         "grid")
  expect_equal(pp("grid_/-2")$clean,        "grid")
  expect_equal(pp("grid_0mm")$clean,        "grid")
  expect_equal(pp("grid_5furlongs")$clean,  "grid")
  expect_equal(pp(1.5)$clean,               "brick")
  expect_equal(pp(NA)$clean,                "brick")
  expect_equal(pp(TRUE)$clean,              "brick")

  for (p in list("stripes45", "s", "fish_huge", 1.5, NA, ""))
    expect_gt(length(pp(p)$problems), 0)
})


test_that("check_lty() accepts every ?par line type", {
  cl <- fillpattern:::check_lty

  for (lty in c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank"))
    expect_equal(cl(lty), lty)

  expect_equal(cl("dash"),   "dashed")
  expect_equal(cl("DOTTED"), "dotted")
  expect_equal(cl(0:6 + 0), NULL) # not one value
  for (i in 0:6) {
    expect_identical(cl(i),               i)
    expect_identical(cl(as.numeric(i)),   i)
    expect_identical(cl(as.character(i)), i)
  }
  for (hex in c("44", "13", "1342", "F2F2", "123456", "12345678", "aB"))
    expect_equal(cl(hex), hex)

  for (bad in list("bogus", "7", "444", "1234567890", "04", 7, -1, 1.5, NA, NULL, TRUE))
    expect_null(cl(bad))
})


test_that("fillPatternGrob() replaces invalid values, with a warning", {

  fix <- function (..., value) {
    expect_warning(g <- fillPatternGrob(...), class = "fillpattern_invalid_arg")
    expect_identical(g[[...names()]], value)
  }

  fix(pattern  = "stripes45",   value = "brick45")
  fix(pattern  = 2.5,           value = "brick")
  fix(fg       = "notacolour",  value = "black")
  fix(fg       = grid::linearGradient(), value = "black")
  fix(bg       = "notacolour",  value = "transparent")
  fix(bg       = list(),        value = "transparent")
  fix(angle    = NA,            value = 0)
  fix(angle    = "45",          value = 0)
  fix(width    = 0,             value = 5)
  fix(width    = -1,            value = 5)
  fix(width    = unit(-1, "mm"), value = 5)
  fix(height   = "tall",        value = NA)
  fix(lwd      = 0,             value = 1)
  fix(lwd      = NA,            value = 1)
  fix(lty      = "bogus",       value = "solid")
  fix(lty      = 9,             value = "solid")
  fix(fun      = "f",           value = NULL)

  expect_warning(g <- fillPatternGrob(min_size = -1), class = "fillpattern_invalid_arg")
  expect_equal(g$min_size, unit(2, "mm"))

  expect_warning(
    g <- fillPatternGrob(width = NA, height = NA),
    "can't both be `NA`" )
  expect_equal(g$width, 5)
})


test_that("fillPatternGrob() keeps valid values as they are", {
  args <- list(
    pattern = "hex_lg:xl_2dotted", fg = "#FF000080", bg = NA, angle = -30,
    width = unit(1, "npc") - unit(2, "mm"), height = NA, lwd = 0.25,
    lty = "1342", fun = function (env, row) NULL, min_size = 0 )
  g <- expect_silent(do.call(fillPatternGrob, args))
  for (i in setdiff(names(args), "min_size"))
    expect_identical(g[[i]], args[[i]])

  expect_identical(expect_silent(fillPatternGrob(lty = "dash"))$lty, "dashed")
  expect_identical(expect_silent(fillPatternGrob(lty = 2))$lty, 2L)
  expect_identical(expect_silent(fillPatternGrob(fg = 2))$fg, 2)
  expect_identical(expect_silent(fillPatternGrob(height = unit(3, "mm")))$height, unit(3, "mm"))
})


test_that("fill_pattern() names the element that was replaced", {
  expect_warning(
    fills <- fill_pattern(c("brick", "stripes45", "grid")),
    "`patterns[2]` = \"stripes45\" is invalid", fixed = TRUE )
  expect_length(fills, 3)

  expect_warning(
    fill_pattern(c("brick", "fish"), lty = c("dashed", "bogus")),
    "`lty[2]`", fixed = TRUE )

  # A single bad value warns once, not once per pattern.
  n <- 0
  withCallingHandlers(
    fill_pattern(c("brick", "fish", "grid"), lwd = -1),
    fillpattern_invalid_arg = function (w) {
      n <<- n + 1
      invokeRestart("muffleWarning")
    })
  expect_equal(n, 1)

  # Names are preserved even when a pattern is replaced.
  expect_warning(fills <- fill_pattern(c(A = "brick", B = "nope")))
  expect_named(fills, c("A", "B"))
})


test_that("issue #11 examples draw patterns, with one warning per bad value", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  library(ggplot2)
  p <- ggplot(mpg, aes(drv, fill = drv)) + geom_bar()

  draw <- function (...) {
    rec  <- new.env()
    rec$seen <- list()
    hook <- function (env, row) {
      rec$seen[[length(rec$seen) + 1]] <- list(pattern = env$pattern, lty = env$lty)
      NULL
    }
    rec$warnings <- character(0)
    withCallingHandlers(
      print(p + scale_fill_pattern(fg = "black", ..., fun = hook)),
      warning = function (w) {
        rec$warnings <- c(rec$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    rec
  }

  # Typo: drawn as "brick45", with one warning.
  rec <- draw(c("brick", "stripes45", "grid"))
  expect_length(rec$warnings, 1)
  expect_match(rec$warnings, "stripes45")
  expect_setequal(
    unique(vapply(rec$seen, `[[`, "", "pattern")),
    c("brick", "grid") )

  # ?par line types: all drawn, no warnings, no solid blocks.
  rec <- draw(lty = c("dashed", "44", "2"))
  expect_length(rec$warnings, 0)
  expect_setequal(unique(lapply(rec$seen, `[[`, "lty")), list("dashed", "44", 2L))
})


test_that("scale_fill_pattern() replaces invalid fade and alpha", {
  sc <- scale_fill_pattern(fg = "black", fade = 2, alpha = c(1, -1))
  expect_warning(
    expect_warning(res <- sc$palette(2), "`fade`"),
    "`alpha[2]`", fixed = TRUE )
  expect_equal(attr(res[[1]], "fillpattern_args")$fade,  1)
  expect_equal(attr(res[[2]], "fillpattern_args")$alpha, 1)
})


test_that("makeContent() fills small shapes with solid fg, without a warning", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = unit(1, "mm"), height = unit(10, "mm")))

  g <- expect_no_warning(grid::makeContent(fillPatternGrob(fg = "blue")))
  expect_length(g$children, 1)
  expect_equal(g$children[[1]]$gp$fill, "blue")

  # fg = NA: the solid fill uses the shape's outline colour, not NA.
  grid::pushViewport(grid::viewport(gp = grid::gpar(col = "red")))
  g <- expect_no_warning(grid::makeContent(fillPatternGrob(fg = NA)))
  expect_equal(g$children[[1]]$gp$fill, "red")

  # Zero-width shapes, e.g. lines.
  grid::pushViewport(grid::viewport(width = unit(0, "mm")))
  g <- expect_no_warning(grid::makeContent(fillPatternGrob(min_size = 0)))
  expect_length(g$children, 1)
})


test_that("makeContent() warns when an unexpected error forces a solid fill", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  old <- options(fillpattern.fallback_warning = "always")
  on.exit(options(old), add = TRUE)

  # A `fun` hook that fails after validation.
  g <- fillPatternGrob(fg = "green", fun = function (env, row) stop("boom"))

  expect_warning(
    res <- grid::makeContent(g),
    class = "fillpattern_fallback" )
  expect_length(res$children, 1)
  expect_equal(res$children[[1]]$gp$fill, "green")
})


test_that("the draw-time fallback warning is shown once per session", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  the <- fillpattern:::the
  was <- the$fallback_warned
  on.exit(the$fallback_warned <- was, add = TRUE)
  the$fallback_warned <- FALSE

  g <- fillPatternGrob(fun = function (env, row) stop("boom"))
  expect_warning(grid::makeContent(g), "once per session", class = "fillpattern_fallback")
  expect_no_warning(grid::makeContent(g))
  expect_no_warning(grid::makeContent(g))
})
