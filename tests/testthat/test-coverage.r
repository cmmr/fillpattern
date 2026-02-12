test_that("fill_pattern handle empty and named inputs", {
  expect_equal(fill_pattern(patterns = character(0)), list())
  p <- fill_pattern(patterns = c(A = "brick"))
  expect_equal(names(p), "A")
})

test_that("fillPatternGrob validates arguments", {
  expect_error(fillPatternGrob(lwd = 1:2), "should be one value")
  expect_error(fillPatternGrob(lty = c("solid", "dashed")), "should be one value")
  g <- fillPatternGrob(min_size = 5)
  expect_true(grid::is.unit(g$min_size))
})

test_that("makeContent parsing logic and edge cases", {
  # 1. Numeric pattern mapping
  g <- fillPatternGrob(pattern = 1, bg = "white")
  g_made <- grid::makeContent(g)
  expect_length(g_made$children, 2) 
  
  # 2. Case insensitivity and trimming
  g <- fillPatternGrob(pattern = "  Brick ", bg = "white")
  g_made <- grid::makeContent(g)
  expect_length(g_made$children, 2)
  
  # 3. Angle modifier parsing
  g <- fillPatternGrob(pattern = "brick45", angle = 10, bg = "white")
  g_made <- grid::makeContent(g)
  expect_length(g_made$children, 2)
  
  # 4. String parsing: Line Width and Style
  g <- fillPatternGrob(pattern = "grid_2lwd_dashed", lwd = 1, lty = "solid", bg = "white")
  g_made <- grid::makeContent(g)
  fg_gp <- g_made$children[[2]]$gp
  expect_equal(fg_gp$lwd, 2)
  expect_equal(fg_gp$lty, "dashed")
  
  # 5. Error Handling: Invalid pattern name (Fall back to solid rect)
  g <- fillPatternGrob(pattern = "notapattern")
  g_made <- grid::makeContent(g)
  expect_length(g_made$children, 1)
  expect_s3_class(g_made$children[[1]], "rect")
  
  # 6. Error Handling: Invalid lty (Fall back to solid rect)
  g <- fillPatternGrob(lty = "notalty")
  g_made <- grid::makeContent(g)
  expect_length(g_made$children, 1)
  
  # 7. Error Handling: Insufficient size
  # The function catches the "stop" and returns a fallback rectGrob.
  # We verify that it returns the fallback (length 1 child) instead of throwing an error.
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = grid::unit(10, "mm"), height = grid::unit(10, "mm")))
  on.exit(grid::popViewport())
  
  g <- fillPatternGrob(min_size = 20)
  g_made <- grid::makeContent(g)
  
  # Expect fallback behavior (1 child = the background/solid rect), not a crash
  expect_length(g_made$children, 1) 
  expect_s3_class(g_made$children[[1]], "rect")
  
  # 8. Custom 'fun' execution
  env_monitor <- new.env()
  f <- function(env, row) { assign("called", TRUE, envir = env_monitor) }
  g <- fillPatternGrob(fun = f)
  grid::makeContent(g)
  expect_true(exists("called", envir = env_monitor))
})

test_that("modify_size internal logic", {
  ms <- fillpattern:::modify_size
  expect_equal(ms(10, 'x', "xs"), 2.5)
  expect_equal(ms(10, 'x', "sm"), 5)
  expect_equal(ms(10, 'x', "md"), 10)
  expect_equal(ms(10, 'x', "lg"), 20)
  expect_equal(ms(10, 'x', "xl"), 40)
  expect_equal(ms(10, 'x', "*3"), 30)
  expect_equal(ms(10, 'x', "/2"), 5)
  expect_equal(ms(0, 'x', "1in"), 25.4, tolerance = 0.01)
  
  expect_error(ms(10, 'x', "badunit"), "Expected a positive number")
  expect_error(ms(10, 'x', "10badunit"), "Invalid unit")
})

test_that("lighten internal logic", {
  li <- fillpattern:::lighten
  expect_equal(li("black", "black", 1, 1), "black")
  expect_equal(li("black", "black", 0.5, 1), "#7F7F7F")
  expect_equal(li(NA, "red", 1, 1), "red")
  expect_error(li("black", "black", 2, 1), "must be within")
  expect_error(li("black", "black", 1, -1), "must be within")
  expect_error(li("black", "black", "text", 1), "must be one number")
})

test_that("pattern_alpha logic", {
  # Test white/white correction (force fg to black)
  args <- list(fg = "white", bg = "white", fade=1, alpha=1)
  class(args) <- c("GridFillPattern", "GridPattern")
  
  # Invoke method
  res <- fillpattern:::pattern_alpha.GridFillPattern(args, 1)
  
  # In recent grid versions, the grob is stored inside the function closure 'f'.
  # We inspect the environment of 'f' to find the 'grob' object.
  expect_true(is.function(res$f))
  
  env <- environment(res$f)
  expect_true(exists("grob", envir = env), info = "Could not find 'grob' in pattern closure")
  
  internal_grob <- get("grob", envir = env)
  expect_equal(internal_grob$fg, "black")
})

test_that("scale_fill_pattern accepts functions", {
  sc <- scale_fill_pattern(fg = function(n) rep("red", n))
  expect_s3_class(sc, "ScaleDiscrete")
})
