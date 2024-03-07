test_that("pattern_alpha", {
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  args <- structure(
    class = c('GridFillPattern', 'GridPattern'),
    .Data = list(
      patterns = "brick", 
      fg       = "black", 
      bg       = "white" ))
  
  fill <- expect_silent(ggplot2::pattern_alpha(x = args, alpha = 1))
  
  expect_s3_class(fill, c("GridTilingPattern", "GridPattern"))
  
})
