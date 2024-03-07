test_that("ggplot2 graphics", {

  library(ggplot2)
  
  grDevices::pdf(file = tf <- tempfile(fileext = ".pdf"))
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  
  p1 <- expect_silent({
    ggplot(mpg, aes(x = class, y = hwy, color = class, fill = class)) +
      geom_boxplot() +
      scale_fill_pattern()
  })
  
  p2 <- expect_silent({
    ggplot(mpg, aes(x = drv, y = hwy, color = drv, fill = drv)) +
      geom_violin() +
      scale_colour_brewer(palette = "Set1") + 
      scale_fill_pattern(c("brick", "stripe45", "grid45_lg"), fg = "black")
  })
  
  p3 <- expect_silent({
    ggplot(mpg, aes(x = drv, color = drv, fill = drv)) +
      geom_bar() +
      scale_fill_pattern(
        patterns = c("hex_sm", "brick90_xl", "fish"),
        lty      = c("solid", "twodash", "dotted"),
        lwd      = c(2, 3, 1) ) +
      theme(legend.key.size = unit(2, 'cm'))
  })
  
})
  
  
# # Verify that colors are getting copied from aes() 'color' to 'fill'
# test_that("ggplot2 color mapping", {
#   
#   pdf(NULL)
#   on.exit(dev.off(), add = TRUE)
#   
#   p <- expect_silent({
#     ggplot(mpg, aes(x = drv, color = drv, fill = drv)) +
#       scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF")) + 
#       geom_bar() +
#       scale_fill_pattern(pattern = "stripe", fg = "black", fade = 1) + 
#       theme_void()
#   })
#   
#   gtable <- expect_silent(ggplot_gtable(ggplot_build(p)))
#   pIndex <- expect_silent(sapply(gtable$grobs, function (g) startsWith(g$name, "panel-1")))
#   expect_equal(sum(pIndex), 1)
#   panel  <- gtable$grobs[[which(pIndex)]]
#   rIndex <- expect_silent(sapply(names(panel$children), startsWith, "geom_rect"))
#   expect_equal(sum(rIndex), 1)
#   fills  <- panel$children[[which(rIndex)]]$gp$fill
#   
#   
#   skip_if_not(capabilities("cairo"), "No SVG file support.")
#   skip_on_os("mac")
#   
#   tf <- tempfile(fileext = ".svg")
#   on.exit(unlink(tf), add = TRUE)
#   
#   svg(filename = tf); fills[[1]]$f(); dev.off()
#   expect_true(any(grepl('fill:rgb(100%,0%,0%)', readLines(tf), fixed = TRUE)))
#   
#   svg(filename = tf); fills[[2]]$f(); dev.off()
#   expect_true(any(grepl('fill:rgb(0%,100%,0%)', readLines(tf), fixed = TRUE)))
#   
#   svg(filename = tf); fills[[3]]$f(); dev.off()
#   expect_true(any(grepl('fill:rgb(0%,0%,100%)', readLines(tf), fixed = TRUE)))
#   
# })
