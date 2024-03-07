test_that("fillPatternGrob", {
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  gt <- expect_silent(fillPatternGrob())
  expect_s3_class(gt, c("fill_pattern", "gTree", "grob", "gDesc"))
  expect_contains(names(gt), methods::formalArgs(fillPatternGrob))
  expect_contains(names(gt), c("name", "gp", "vp", "children", "childrenOrder"))
  
})
