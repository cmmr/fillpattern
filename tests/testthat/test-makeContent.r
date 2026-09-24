test_that("makeContent", {
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  gt <- expect_silent(fillPatternGrob())
  gt <- expect_silent(grid::makeContent(gt))
  
  expect_s3_class(gt, c("fill_pattern", "gTree", "grob", "gDesc"))
  expect_contains(names(gt), methods::formalArgs(fillPatternGrob))
  expect_contains(names(gt), c("name", "gp", "vp", "children", "childrenOrder"))
  expect_gte(length(gt$children), 1)
  expect_s3_class(gt$children, c("gList"))

})

test_that("makeContent: 'empty' pattern draws only the background", {

  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  # With a background: a single rect filled with bg (not the fg error fallback)
  g <- grid::makeContent(fillPatternGrob(pattern = "empty", fg = "red", bg = "white"))
  expect_s3_class(g, "fill_pattern")
  expect_length(g$children, 1)
  expect_s3_class(g$children[[1]], "rect")
  expect_equal(g$children[[1]]$gp$fill, "white")

  # Transparent background: nothing drawn
  g <- grid::makeContent(fillPatternGrob(pattern = "empty"))
  expect_s3_class(g, "fill_pattern")
  expect_length(g$children, 0)

  # Abbreviations and modifiers are accepted without falling back
  for (p in c("emp", "empty45", "empty_lg_2dashed")) {
    g <- grid::makeContent(fillPatternGrob(pattern = p, bg = "white"))
    expect_s3_class(g, "fill_pattern")
    expect_length(g$children, 1)
  }

})
