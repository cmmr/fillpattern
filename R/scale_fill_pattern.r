#' Patterned Fills for ggplot.
#' 
#' Two important caveats - \cr\cr
#' 1. `color` and `fill` must be mapped to the same variable in the `aes()` 
#' specification, for example `aes(color = var, fill = var)`.\cr\cr
#' 2. This function sets both the color and fill scales, so you cannot use 
#' other `scale_color_*` or `scale_fill_*` functions on the same plot.\cr\cr
#' 
#' 
#' @param size   The size of the individual pattern elements. Assumed to be 
#'        millimeters unless set otherwise with [unit()]. Use a vector of 
#'        length two in a list to set width and height separately. The default 
#'        will scale the "grid" pattern to repeat five times across the x axis. 
#'        See details section for additional information. 
#'        Default: `list(unit(c(1/5, NA), 'npc'))`
#' 
#' @param pattern,color   A vector of pattern/color names which will be subset 
#'        or recycled as needed. Or, a function that accepts `n` as an argument 
#'        and returns exactly `n` pattern/color names. 
#'        Default: `pattern = seq_len, color = scales::hue_pal()`
#' 
#' @param ...   Arguments passed on to [fill_pattern_list()].
#' 
#' @return A list of [ggplot2::discrete_scale()] objects.
#' 
#' 
#' 
#' @export
#' @examples
#'     library(ggplot2)
#'     library(fillpattern)
#'
#'     ggplot(mpg, aes(x = class, y = hwy, color = class, fill = class)) +
#'       geom_boxplot() +
#'       scale_fill_pattern()
#'
#'     ggplot(mpg, aes(x = drv, y = hwy, color = drv, fill = drv)) +
#'       geom_violin() +
#'       scale_fill_pattern(invert = TRUE)
#'
#'     ggplot(mpg, aes(x = drv, color = drv, fill = drv)) +
#'       geom_bar() +
#'       scale_fill_pattern(
#'         pattern = c("grid45_xs", "shingle", "brick90_sm"),
#'         color   = scales::pal_brewer("qual"),
#'         lty   = c("dotted", "twodash", "dotdash"),
#'         lwd   = c(1, 3, 2) )

scale_fill_pattern <- function (
    pattern = seq_len, 
    fg      = NA, 
    bg      = ifelse(is.na(fg), "transparent", NA), 
    angle   = 0, 
    width   = unit(1/10, 'npc'), 
    height  = NA, 
    lwd     = 1, 
    lty     = "solid",
    fun     = NA ) {
  
  stopifnot(length(fun) == 1 && (is.function(fun) || is.na(fun)))
  
  
  fill_palette <- function (n) {
    
    for (i in c('pattern', 'fg', 'bg', 'angle', 'width', 'height', 'lwd', 'lty')) {
      v <- get(i)
      if (is.function(v)) v <- v(n)
      v <- rep(v, length.out = n)
      assign(i, v)
    }
    
    lapply(seq_len(n), function (i) {
      
      structure(
        class = c('GridFillPattern', 'GridPattern'),
        .Data = list(
          pattern = pattern[[i]],
          fg      = fg[[i]],
          bg      = bg[[i]],
          angle   = angle[[i]],
          width   = width[[i]],
          height  = height[[i]],
          lwd     = lwd[[i]],
          lty     = lty[[i]],
          fun     = fun ))
      
    })}
  
  ggplot2::discrete_scale(aesthetics = "fill",  palette = fill_palette)
}




pattern_alpha.GridFillPattern <- function (x, alpha) {
  
  p <- parent.frame(5)
  i <- if (exists('fill_pattern_i', p)) get('fill_pattern_i', p) else 1
  assign('fill_pattern_i', i + 1, p)
  
  row <- if (exists('coords', p))     { as.list(get('coords',     p)[i,])
  } else if (exists('first_rows', p)) { as.list(get('first_rows', p)[i,])
  } else if (exists('data', p))       { as.list(get('data',       p)[i,])
  } else                              { list(colour = "black") }
  
  color <- row$colour
  if (is.na(x$fg)) x$fg <- ifelse(is.na(color), "black", color)
  if (is.na(x$bg)) x$bg <- ifelse(is.na(color), "white", color)
  
  if (is.function(x$fun)) attr(x$fun, 'row') <- row
  
  do.call(fill_pattern, x)
}

