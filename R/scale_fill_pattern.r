#' Patterned Fills for ggplot.
#' 
#' @inherit fill_pattern
#'
#' @param patterns   A vector of pattern names that will be subset or recycled 
#'        as needed to match the levels of the `aes()` fill variable. If 
#'        integers are provided, they are mapped to predefined patterns. 
#'        See "Details" and "Pattern Names" sections below. Default: `seq_len`
#' 
#' @param fg   Foreground color for the pattern's lines, or `NA` to use the 
#'        color scale for the `aes()` color variable. Default: `NA`
#' 
#' @param bg   Background color (or grob), or `NA` to use the color scale for 
#'        the `aes()` color variable.
#'        Default: `ifelse(is.na(fg), "transparent", NA)`
#'        
#' @param fade,alpha   Modify the color from the `aes()` color scale. Fade will 
#'        make it more white, and alpha will make it more transparent. Both 
#'        values must be between 0 and 1, inclusive, where 1 means unchanged.
#'        Default: `fade = ifelse(is.na(fg), 1, 0.6), alpha = 1`
#' 
#' @param angle   How much the rotate the pattern, given in degrees clockwise.
#'        Default: `0`
#' 
#' @param width   The width of the pattern tile. Assumed to be millimeters 
#'        unless set otherwise with [unit()]. Default: `unit(1/10, 'npc')`
#' 
#' 
#' @return A [ggplot2::discrete_scale()] object.
#' 
#' 
#' @details All of the parameters can accept a vector of values or a function 
#'          that takes `n` as an argument and returns the value(s) to use. The 
#'          values are subset or recycled as needed to obtain the same number 
#'          as `length(levels(fill))`, where fill is the variable defined by 
#'          `aes(fill = )`.
#' 
#' 
#' @seealso [fill_pattern()] for base `grid` graphics integration.
#' 
#' @export
#' @examples
#'  \donttest{
#'     library(ggplot2)
#'     library(fillpattern)
#'     
#'     ggplot(mpg, aes(x = class, y = hwy, color = class, fill = class)) +
#'       geom_boxplot() +
#'       scale_fill_pattern()
#'
#'     ggplot(mpg, aes(x = drv, y = hwy, color = drv, fill = drv)) +
#'       geom_violin() +
#'       scale_colour_brewer(palette = "Set1") + 
#'       scale_fill_pattern(c("brick", "stripe45", "grid45_lg"), fg = "black")
#'
#'     ggplot(mpg, aes(x = drv, color = drv, fill = drv)) +
#'       geom_bar() +
#'       scale_fill_pattern(
#'         patterns = c("hex_sm", "brick90_xl", "fish"),
#'         lty      = c("solid", "twodash", "dotted"),
#'         lwd      = c(2, 3, 1) ) +
#'       theme(legend.key.size = unit(2, 'cm'))
#'  }

scale_fill_pattern <- function (
    patterns = seq_len, 
    fg       = NA, 
    bg       = ifelse(is.na(fg), "transparent", NA), 
    fade     = ifelse(is.na(fg), 1, 0.6), 
    alpha    = 1, 
    angle    = 0, 
    width    = unit(1/10, 'npc'), 
    height   = NA, 
    lwd      = 1, 
    lty      = "solid",
    fun      = NULL,
    min_size = 2 ) {
  
  
  #________________________________________________________
  # Fetch the i-th value from val, subsetting/recycling.
  #________________________________________________________
  get_i <- function (val, i) {
    if (length(val) <= 1) return (val)
    return (val[[(i - 1) %% length(val) + 1]])
  }
  
  
  fill_palette <- function (n) {
    
    #________________________________________________________
    # Call any functions passed to parameters
    #________________________________________________________
    for (i in formalArgs(scale_fill_pattern))
      if (is.function(v <- get(i)))
        if (!identical(formalArgs(v), c('env', 'row')))
          assign(i, v(n))
    
    
    #________________________________________________________
    # Sets of fill_pattern() arguments for pattern_alpha.
    #________________________________________________________
    fills <- lapply(seq_len(n), function (i) {
      structure(
        class = c('GridFillPattern', 'GridPattern'),
        .Data = list(
          patterns = get_i(patterns, i), 
          fg       = get_i(fg,       i), 
          bg       = get_i(bg,       i), 
          fade     = get_i(fade,     i), 
          alpha    = get_i(alpha,    i), 
          angle    = get_i(angle,    i), 
          width    = get_i(width,    i), 
          height   = get_i(height,   i), 
          lwd      = get_i(lwd,      i), 
          lty      = get_i(lty,      i), 
          fun      = get_i(fun,      i),
          min_size = get_i(min_size, i) ))
    })
    
    
    #________________________________________________________
    # Preserve any names set by the user on `patterns`.
    #________________________________________________________
    if (!is.null(names(patterns)))
      names(fills) <- names(patterns)
    
    return (fills)
  }
  
  
  ggplot2::discrete_scale(aesthetics = "fill",  palette = fill_palette)
}




#' Apply aes() color mapping to patterns.
#'
#' @noRd
#' @keywords internal
#' 
#' @param x       A list of arguments for `fill_pattern()`.
#' @param alpha   The alpha mask that `ggbuild` wants applied, not used.
#' 
#' @return A list of one [grid::pattern()] object.
#' 
#' @export
#' @examples
#'     library(grid)
#'     library(fillpattern)
#'     
#'     args <- structure(
#'       class = c('GridFillPattern', 'GridPattern'),
#'       .Data = list(
#'         patterns = "brick", 
#'         fg       = "black", 
#'         bg       = "white" ))
#'     
#'     fill <- ggplot2::pattern_alpha(x = args, alpha = 1)
#'     grid.newpage()
#'     grid.rect(width = 0.5, height = 0.5, gp = gpar(fill = fill))
#' 

pattern_alpha.GridFillPattern <- function (x, alpha) {
  
  row <- list(colour = NA)
  
  # Skip if we're somehow not in a ggplot_gtable() call stack.
  if (length(sys.frames()) >= 5) {
    
    # Find all the aes() mappings for all grobs.
    p   <- parent.frame(n = 5)
    dat <- if (exists('coords',     p)) { get('coords',     p)
    } else if (exists('first_rows', p)) { get('first_rows', p)
    } else if (exists('data',       p)) { get('data',       p) }
    
    if (is.data.frame(dat)) {
      
      # Find our Map()/mapply() index.
      i <- try(match.call()$x[[3]], silent = TRUE)
      if (!is.integer(i)) i <- 1L
      
      # This grob's aes() mappings.
      if (nrow(dat) >= i) row <- as.list(dat[i,])
      
    }
  }
  
  
  # Skip zero-width or zero-height grobs.
  if (all(c('xmin', 'xmax', 'ymin', 'ymax') %in% names(row)))
    if (row$xmin == row$xmax || row$ymin == row$ymax)
      return (grid::pattern(grid::rectGrob()))
  
  
  # Apply lightened color mapping to fg/bg if they are NA.
  if (is.na(x$fg)) x$fg <- lighten(row$colour, "black", x$fade, x$alpha)
  if (is.na(x$bg)) x$bg <- lighten(row$colour, "white", x$fade, x$alpha)
  
  
  # Make key glyphs visible for scale_fill_pattern(fg = "white").
  if (all(c(x$fg, x$bg) %in% c("white", "#FFFFFF", "#FFFFFFFF")))
    x$fg <- "black"
  
  
  # Enable user-provided function to access the aes mappings too.
  if (is.function(x$fun)) attr(x$fun, 'row') <- row
  
  
  # Now that we have the colors, build the grob.
  do.call(fill_pattern, x[intersect(names(x), formalArgs(fill_pattern))])[[1]]
}



#' Make a color more white and/or transparent.
#'
#' @noRd
#' @keywords internal
#' 
#' @param color   A color in a format parse-able by `grDevices::col2rgb()`.
#' @param default   Color to use instead if `color` is `NA`.
#' @param fade   How much to fade the color towards white. Range [0,1], where 1 
#'        is no change.
#' @param alpha   How much transparency to add. Range [0,1], where 1 is no 
#'        change.
#' 
#' @return A color.
#' 
#' @examples
#' 
#' fillpattern:::lighten("black", "black", 1, 1)
#' fillpattern:::lighten("black", "black", 1, 0)
#' fillpattern:::lighten("black", "black", 0, 0)
#' 

lighten <- function (color, default, fade, alpha) {
  
  if (is.na(color)) color <- default
  
  for (i in c('fade', 'alpha')) {
    v <- get(i)
    if (!is.numeric(v) || length(v) != 1) stop(i, " must be one number.")
    if (is.na(v) || v < 0 || v > 1)       stop(i, " must be within [0,1].")
  }
  
  if (fade < 1) {
    color <- grDevices::col2rgb(color)
    color <- color + (255 - color) * (1 - fade)
    color <- grDevices::rgb(t(color), maxColorValue=255)
  }
  
  if (alpha < 1)
    color <- ggplot2::alpha(color, alpha)
  
  return (color)
}

