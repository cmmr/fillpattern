test_that("fade and alpha", {
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  expect_equal(fillpattern:::lighten("black", "black", 1, 1), "black")
  expect_equal(fillpattern:::lighten(NA,      "black", 1, 0), "#00000000")
  expect_equal(fillpattern:::lighten("black", "black", 0, 0), "#FFFFFF00")
})
