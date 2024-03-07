test_that("modify_size", {
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  expect_equal(fillpattern:::modify_size(30, 'x', "20mm"), 20)
  expect_equal(fillpattern:::modify_size(20, 'y', "*2"),   40)
  
})
