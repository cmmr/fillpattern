test_that("scale_fill_pattern: internal get_i recycling logic", {
  # We access the internal get_i logic via the generated palette function
  sc <- scale_fill_pattern(
    patterns = c("p1", "p2"),
    fg       = c("red", "blue"),
    width    = 10 # Scalar (length 1)
  )
  
  # The palette function is what calls get_i
  pal <- sc$palette
  
  # Request 3 items to force recycling of the length-2 vectors
  # Expected: 
  # 1: p1, red, width=10
  # 2: p2, blue, width=10
  # 3: p1, red, width=10 (Recycled)
  res <- pal(3)
  
  expect_length(res, 3)
  
  # Check item 1
  expect_equal(res[[1]]$patterns, "p1")
  expect_equal(res[[1]]$fg, "red")
  expect_equal(res[[1]]$width, 10)
  
  # Check item 2
  expect_equal(res[[2]]$patterns, "p2")
  expect_equal(res[[2]]$fg, "blue")
  
  # Check item 3 (Recycled)
  expect_equal(res[[3]]$patterns, "p1")
  expect_equal(res[[3]]$fg, "red")
})

test_that("scale_fill_pattern: internal fill_palette name preservation", {
  # Verify that names on the 'patterns' vector are propagated to the output list
  sc <- scale_fill_pattern(patterns = c(A = "brick", B = "fish"))
  pal <- sc$palette
  res <- pal(2)
  
  expect_equal(names(res), c("A", "B"))
})

test_that("scale_fill_pattern: functional parameters evaluation", {
  # Test that fill_palette executes functions passed as parameters (e.g. fg(n))
  # fg is a function of n.
  # CRITICAL: We explicitly provide bg and fade to prevent the default arguments
  # (which use is.na(fg)) from triggering a warning when fg is a function.
  sc <- scale_fill_pattern(
    patterns = "brick",
    fg = function(n) paste0("color", 1:n),
    bg = "transparent", 
    fade = 1
  )
  
  pal <- sc$palette
  res <- pal(2)
  
  expect_equal(res[[1]]$fg, "color1")
  expect_equal(res[[2]]$fg, "color2")
})

test_that("scale_fill_pattern: distinguishes 'fun' hook from param functions", {
  # 1. If 'fun' is a hook (has env, row args), it should be passed as-is
  hook_fun <- function(env, row) { }
  sc <- scale_fill_pattern(fun = hook_fun)
  pal <- sc$palette
  res <- pal(1)
  expect_identical(res[[1]]$fun, hook_fun)
  
  # 2. If a param function (like lwd) does NOT have (env, row), it is evaluated
  gen_fun <- function(n) rep(5, n)
  sc <- scale_fill_pattern(lwd = gen_fun)
  pal <- sc$palette
  res <- pal(2)
  expect_equal(res[[1]]$lwd, 5)
})

test_that("pattern_alpha: extracts aes context and passes to fun", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  # Create a hook to capture the 'row' data passed to the pattern
  captured_rows <- list()
  capture_hook <- function(env, row) {
    captured_rows <<- c(captured_rows, list(row))
    NULL
  }
  
  # Make a simple plot
  df <- data.frame(x = 1, y = 1, fill_group = "A", stringsAsFactors = FALSE)
  
  p <- ggplot(df, aes(x, y, fill = fill_group)) +
    geom_tile() +
    scale_fill_pattern(fun = capture_hook, patterns = "brick")
  
  # We must actually DRAW the plot to trigger makeContent (and thus the hook).
  # We use a temporary PDF device to avoid opening windows.
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  print(p)
  
  # Verify capture occurred
  expect_true(length(captured_rows) > 0)
  
  # Verify the captured row contains expected data
  # geom_tile data usually contains x, y, fill, PANEL, group, etc.
  row1 <- captured_rows[[1]]
  expect_true("fill" %in% names(row1))
  expect_true("x" %in% names(row1))
})

test_that("pattern_alpha: zero-dimension optimization", {
  # pattern_alpha has a check: if width/height is 0, return a simple rectGrob.
  # We assume this works if a zero-width plot renders without error.
  
  df <- data.frame(x = 1, y = 1, g = "A")
  # A bar with width 0
  p <- ggplot(df, aes(x, y, fill = g)) +
    geom_col(width = 0) + 
    scale_fill_pattern()
  
  # Just ensure it builds/prints without error
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  expect_silent(print(p))
})

test_that("pattern_alpha: calculates default bg from aes color when fg is set", {
  library(ggplot2)
  
  # Scenario: 
  # 1. fg is set to "black", so bg defaults to NA in scale_fill_pattern().
  # 2. geom_tile has colour="red".
  # 3. pattern_alpha should see bg=NA and calculate it using lighten("red", "white").
  
  # We use a hook to capture the computed 'bg' actually passed to the grob
  # makeContent converts bg to a rectGrob, so we must extract the fill parameter.
  captured_bg <- NULL
  capture_bg_hook <- function(env, row) {
    if (inherits(env$bg, "grob")) {
      captured_bg <<- env$bg$gp$fill
    } else {
      captured_bg <<- env$bg
    }
    NULL
  }
  
  df <- data.frame(x = 1, y = 1, g = "A")
  
  p <- ggplot(df, aes(x, y, fill = g)) +
    # Use linewidth instead of size to avoid deprecation warning
    geom_tile(colour = "red", linewidth = 2) + 
    scale_fill_pattern(fg = "black", fun = capture_bg_hook)
  
  # Render to trigger pattern_alpha and the hook
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  print(p)
  
  # Verification:
  # 1. bg should be a color string now
  expect_type(captured_bg, "character")
  
  # 2. bg should NOT be NA
  expect_false(is.na(captured_bg))
  
  # 3. bg should NOT be "transparent"
  expect_true(captured_bg != "transparent")
  
  # 4. bg should be a lightened version of "red" (mixed with white)
  # lighten("red", "white", fade=0.6 (default), alpha=1)
  expect_true(captured_bg != "#FF0000") 
  expect_match(captured_bg, "^#") # Is a hex code
})
