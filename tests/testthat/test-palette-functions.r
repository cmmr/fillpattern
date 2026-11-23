
# Color palette functions as `fg` / `bg` (cmmr/fillpattern#11 follow-up).


test_that("bg and fade defaults depend on which fg values are NA", {
  args <- function (pal, n) lapply(pal(n), attr, "fillpattern_args")
  pick <- function (a, x) lapply(a, `[[`, x)

  a <- args(pal_fill_pattern(), 2)
  expect_equal(pick(a, "bg"),   list("transparent", "transparent"))
  expect_equal(pick(a, "fade"), list(1, 1))

  a <- args(pal_fill_pattern(fg = "black"), 2)
  expect_equal(pick(a, "bg"),   list(NA, NA))
  expect_equal(pick(a, "fade"), list(0.6, 0.6))

  a <- args(pal_fill_pattern(fg = c("black", NA)), 2)
  expect_equal(pick(a, "bg"),   list(NA_character_, "transparent"))
  expect_equal(pick(a, "fade"), list(0.6, 1))

  # Explicit values still win.
  a <- args(pal_fill_pattern(fg = "black", bg = "white", fade = 1), 1)
  expect_equal(a[[1]][c("bg", "fade")], list(bg = "white", fade = 1))

  # The scale passes its arguments through unchanged.
  sc <- scale_fill_pattern(fg = c("black", NA))
  expect_equal(pick(lapply(sc$palette(2), attr, "fillpattern_args"), "bg"), list(NA_character_, "transparent"))
})


test_that("palette functions as fg or bg don't warn in the scale", {
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  library(ggplot2)
  p <- ggplot(mpg, aes(drv, fill = drv)) + geom_bar()

  pal <- function (n) grDevices::hcl.colors(n, "Dark 3")

  # fg as a function: bg and fade defaults treat it as "fg given".
  expect_no_warning(fills <- pal_fill_pattern(fg = pal)(3))
  args <- lapply(fills, attr, "fillpattern_args")
  expect_equal(vapply(args, `[[`, "", "fg"), pal(3))
  expect_true(all(vapply(args, function (a) is.na(a$bg), logical(1))))
  expect_equal(vapply(args, `[[`, 0, "fade"), rep(0.6, 3))

  expect_no_warning(print(p + scale_fill_pattern(fg = pal)))
  expect_no_warning(print(p + scale_fill_pattern(fg = pal, bg = "white")))
  expect_no_warning(print(p + scale_fill_pattern(fg = "black", bg = grDevices::hcl.colors)))
})


test_that("fill_pattern() calls functions with the number of patterns", {
  pats <- c("brick", "fish", "grid")
  seen <- NULL
  pal  <- function (n) { seen <<- n; grDevices::hcl.colors(n) }

  fills <- expect_silent(fill_pattern(pats, fg = pal, bg = grDevices::gray.colors))
  expect_equal(seen, 3)
  expect_length(fills, 3)

  grobs <- lapply(fills, function (f) environment(f$f)$grob)
  if (is.null(grobs[[1]])) grobs <- lapply(fills, `[[`, "grob")
  expect_equal(vapply(grobs, `[[`, "", "fg"), grDevices::hcl.colors(3))
  expect_equal(vapply(grobs, `[[`, "", "bg"), grDevices::gray.colors(3))

  # Other arguments too.
  fills <- expect_silent(fill_pattern(pats, lwd = function (n) seq_len(n) / 2))
  grobs <- lapply(fills, function (f) environment(f$f)$grob)
  if (is.null(grobs[[1]])) grobs <- lapply(fills, `[[`, "grob")
  expect_equal(vapply(grobs, `[[`, 0, "lwd"), c(0.5, 1, 1.5))

  # `fun` is a drawing hook, never called with n.
  hook  <- function (env, row) NULL
  fills <- expect_silent(fill_pattern(pats, fun = hook))
  grobs <- lapply(fills, function (f) environment(f$f)$grob)
  if (is.null(grobs[[1]])) grobs <- lapply(fills, `[[`, "grob")
  expect_identical(grobs[[1]]$fun, hook)

  # A function returning invalid values still warns.
  expect_warning(
    fill_pattern("brick", fg = function (n) rep("notacolour", n)),
    "`fg` = \"notacolour\" is invalid", fixed = TRUE )
})
