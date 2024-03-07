
# vdiffr doesn't support the grid::pattern object, 
# so we'll just look for errors/warnings across a range of inputs.

test_that("grid graphics", {
  
  library(grid)
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  
  linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  patterns  <- c("brick", "chevron", "fish", "grid", "herringbone", "hexagon", 
                 "octagon", "rain", "saw", "shingle", "rshingle", "stripe", "wave" )
  
  n     <- 20
  fills <- expect_silent(fill_pattern(
    pattern = rep(patterns,                                    length.out = n), 
    fg      = rep(c("black", "red", "green", "blue"),          length.out = n), 
    bg      = rep(c("white", "transparent"),                   length.out = n), 
    angle   = rep(c(0, 20, 45, 90),                            length.out = n), 
    width   = rep(list(unit(5, 'mm'), NA, unit(1/10, 'npc')),  length.out = n), 
    height  = rep(list(NA, unit(1/10, 'npc'), unit(5, 'mm')),  length.out = n), 
    lwd     = rep(c(0.5, 1, 1.5, 2, 3),                        length.out = n), 
    lty     = rep(linetypes,                                   length.out = n) ))
  
  gp <- expect_silent(Map(gpar, fill = fills))
  
  
  grid.newpage()
  
  i <- 0
  for (x in 1:5 / 5 - 1/10)
    for (y in 1:4 / 4 - 1/8)
      expect_silent({
        i  <- i + 1
        grid.rect(x = x, y = y, width = 1/5, height = 1/4, gp = gp[[i]])
        NULL
      })
  
})
