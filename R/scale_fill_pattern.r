#' Patterned Fills for ggplot.
#' 
#' @inherit fill_pattern
#'
#' @param patterns   A vector of pattern names that will be subset or recycled 
#'        as needed to match the levels of the `aes()` fill variable. If 
#'        integers are provided, they are mapped to the patterns in 
#'        [fill_pattern_order]. See "Details" and "Pattern Names" sections 
#'        below. Default: `seq_len`
#' 
#' @param fg   Foreground color for the pattern's lines, or `NA` to use the 
#'        color scale for the `aes()` color variable. Default: `NA`
#' 
#' @param bg   Background color, grid pattern (such as 
#'        [grid::linearGradient()]), or grob, or `NA` to use the color scale 
#'        for the `aes()` color variable.
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
#' @param aesthetics   The names of the aesthetics that this scale works with,
#'        for instance `"pattern_fill"` for geoms from the \pkg{ggpattern} 
#'        package. Default: `"fill"`
#' 
#' @param ...   Other arguments passed on to [ggplot2::discrete_scale()], such
#'        as `name`, `breaks`, `labels`, `limits`, `na.value`, `drop`, or 
#'        `guide`.
#' 
#' 
#' @return `scale_fill_pattern()` returns a [ggplot2::discrete_scale()] 
#'         object. `pal_fill_pattern()` returns a palette: a function that 
#'         takes a number of levels, `n`, and returns a list of `n` 
#'         [grid::pattern()] objects. Use it as 
#'         `theme(palette.fill.discrete = pal_fill_pattern())` (ggplot2 4.0 or 
#'         later), or as the palette of another package's scale.
#' 
#' 
#' @details All of the parameters can accept a vector of values or a function 
#'          that takes `n` as an argument and returns the value(s) to use. The 
#'          values are subset or recycled as needed to obtain the same number 
#'          as `nlevels(fill)`, where fill is the variable defined by 
#'          `aes(fill = )`.
#' 
#' Invalid values give a warning when the plot is built, and are replaced 
#' with the default for that parameter, as described in [fill_pattern()].
#' 
#' @section Color Mapping:
#' 
#' When `fg` or `bg` is `NA`, the pattern takes its color from the `colour` 
#' aesthetic of the same data row. Geoms that apply `alpha` to their fill with
#' [ggplot2::fill_alpha()] (all of ggplot2's own geoms, and many extensions) 
#' let `scale_fill_pattern()` look up that row directly. For other geoms, the 
#' pattern uses the outline color (`col`) that the geom gives the filled 
#' shape, if there is exactly one; otherwise it uses black lines on a white 
#' background. For example, `ggalluvial::geom_alluvium()` only gives its 
#' flows an outline color with `outline.type = "full"`.
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
#'     
#'     # Patterns as the default discrete fill palette (ggplot2 >= 4.0).
#'     if (packageVersion("ggplot2") >= "4.0.0")
#'       ggplot(mpg, aes(x = drv, fill = drv)) +
#'         geom_bar() +
#'         theme(palette.fill.discrete = pal_fill_pattern(fg = "black"))
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
    min_size = 2,
    aesthetics = "fill",
    ... ) {
  
  # Arguments stay lazy until the palette is called, as before.
  palette <- pal_fill_pattern(
    patterns = patterns, fg = fg, bg = bg, fade = fade, alpha = alpha, 
    angle = angle, width = width, height = height, lwd = lwd, lty = lty, 
    fun = fun, min_size = min_size )
  
  ggplot2::discrete_scale(aesthetics = aesthetics, palette = palette, ...)
}



#' @rdname scale_fill_pattern
#' @export

pal_fill_pattern <- function (
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
  
  
  function (n) {
    
    #________________________________________________________
    # Call any functions passed to parameters
    #________________________________________________________
    for (i in formalArgs(pal_fill_pattern))
      if (is.function(v <- get(i)))
        if (!identical(formalArgs(v), c('env', 'row')))
          assign(i, v(n))
    
    
    #________________________________________________________
    # Replace invalid values with defaults, with a warning.
    #________________________________________________________
    args <- check_args(single = FALSE, args = list(
      patterns = patterns, fg = fg, bg = bg, fade = fade, alpha = alpha, 
      angle = angle, width = width, height = height, lwd = lwd, lty = lty, 
      fun = fun, min_size = min_size ))
    
    
    #________________________________________________________
    # Real grid patterns that remember their fill_pattern() 
    # arguments, so pattern_alpha() can apply the colour 
    # mapping later.
    #________________________________________________________
    fills <- lapply(seq_len(n), function (i) {
      new_fill_pattern(list(
          patterns = get_i(args$patterns, i), 
          fg       = get_i(args$fg,       i), 
          bg       = get_i(args$bg,       i), 
          fade     = get_i(args$fade,     i), 
          alpha    = get_i(args$alpha,    i), 
          angle    = get_i(args$angle,    i), 
          width    = get_i(args$width,    i), 
          height   = get_i(args$height,   i), 
          lwd      = get_i(args$lwd,      i), 
          lty      = get_i(args$lty,      i), 
          fun      = get_i(args$fun,      i),
          min_size = get_i(args$min_size, i) ))
    })
    
    
    #________________________________________________________
    # Preserve any names set by the user on `patterns`.
    #________________________________________________________
    if (!is.null(names(patterns)))
      names(fills) <- names(patterns)
    
    return (fills)
  }
}




#' A grid pattern that remembers its `scale_fill_pattern()` arguments.
#'
#' @noRd
#' @keywords internal
#' 
#' @param args   A list of arguments for `fill_pattern()`, plus `fade` and 
#'        `alpha`. `fg` and/or `bg` may be `NA`.
#' 
#' @return A [grid::pattern()] object with class `"GridFillPattern"` 
#'         prepended and `args` stored in the `"fillpattern_args"` attribute.
#'         Drawn as-is, any `NA` colours are taken from the filled shape's 
#'         `col` (see `makeContent.fill_pattern()`).

new_fill_pattern <- function (args) {
  pat <- build_pattern(args)
  attr(pat, 'fillpattern_args') <- args
  class(pat) <- c('GridFillPattern', class(pat))
  return (pat)
}



#' Apply aes() color mapping to patterns.
#'
#' @noRd
#' @keywords internal
#' 
#' @param x       A pattern from `new_fill_pattern()`. A plain list of 
#'                `fill_pattern()` arguments with class `"GridFillPattern"` is 
#'                also accepted.
#' @param alpha   The alpha mask that `ggbuild` wants applied, not used.
#' 
#' @return A [grid::pattern()] object.
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
  
  args <- attr(x, 'fillpattern_args', exact = TRUE)
  if (is.null(args)) args <- unclass(x)
  
  # ggplot2 calls Map(pattern_alpha, x = fill, alpha = alpha), so our call 
  # looks like `FUN(x = dots[[1L]][[i]], ...)`. Recover `i`.
  i <- tryCatch(match.call()$x[[3]], error = function (e) NULL)
  if (!is.numeric(i) || length(i) != 1) i <- 1L
  
  row <- aes_row(i)
  
  # Skip zero-width or zero-height grobs.
  if (all(c('xmin', 'xmax', 'ymin', 'ymax') %in% names(row)))
    if (isTRUE(row$xmin == row$xmax || row$ymin == row$ymax))
      return (grid::pattern(grid::rectGrob()))
  
  # Apply lightened color mapping to fg/bg if they are NA.
  if (!is.null(row$colour)) {
    if (is_na(args$fg)) args$fg <- lighten(row$colour, "black", args$fade, args$alpha)
    if (is_na(args$bg)) args$bg <- lighten(row$colour, "white", args$fade, args$alpha)
  }
  
  # Enable user-provided function to access the aes mappings too.
  if (is.function(args$fun)) attr(args$fun, 'row') <- row
  
  build_pattern(args)
}



#' Find the layer data for the i-th grob in the current fill_alpha() call.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param i   Row index.
#' 
#' @return A list with the aesthetics for row `i`. If the data can't be 
#'         found, `list()`.
#' 
#' @details Geoms call `ggplot2::fill_alpha()` from their `draw_panel()`, 
#'          `draw_group()`, or `draw_key()` method, where the layer data is 
#'          named `coords`, `first_rows`, or `data`. Search the call stack 
#'          for the innermost `fill_alpha()` call, then look in the frame it 
#'          was called from.

aes_row <- function (i) {
  
  for (k in rev(seq_len(sys.nframe() - 1L))) {
    
    if (!identical(sys.function(k), ggplot2::fill_alpha)) next
    
    env <- sys.frame(sys.parents()[[k]])
    for (nm in c('coords', 'first_rows', 'data')) {
      dat <- get0(nm, envir = env, inherits = FALSE)
      if (is.data.frame(dat))
        return (if (nrow(dat) >= i) as.list(dat[i,]) else list())
    }
    
    break
  }
  
  return (list())
}



#' Convert one set of scale arguments into a grid pattern.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param args   A list of arguments for `fill_pattern()`, plus `fade` and 
#'        `alpha`.
#' 
#' @return A [grid::pattern()] object.

build_pattern <- function (args) {
  
  # Make key glyphs visible for scale_fill_pattern(fg = "white").
  is_white <- function (x) is.character(x) && all(x %in% c("white", "#FFFFFF", "#FFFFFFFF"))
  if (is_white(args$fg) && is_white(args$bg))
    args$fg <- "black"
  
  names(args)[names(args) == 'patterns'] <- 'pattern'
  
  grob <- do.call(fillPatternGrob, args[intersect(names(args), formalArgs(fillPatternGrob))])
  grob$fade  <- args$fade  %||% 1
  grob$alpha <- args$alpha %||% 1
  
  grid::pattern(group = FALSE, grob = grob)
}



#' Make a color more white and/or transparent.
#'
#' @noRd
#' @keywords internal
#' 
#' @param color   A color in a format parse-able by `grDevices::col2rgb()`.
#' @param default   Color to use instead if `color` is `NA`, `NULL`, or fully 
#'        transparent (e.g. ggalluvial's default of `colour = "transparent"`).
#' @param fade   How much to fade the color towards white. Range `[0, 1]`,
#'        where 1 is no change.
#' @param alpha   How much transparency to add. Range `[0, 1]`, where 1 is no
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
  
  if (is_na(color) || isTRUE(grDevices::col2rgb(color, TRUE)[4,1] == 0))
    color <- default
  
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




#' Fetch the i-th value from `val`, subsetting/recycling.
#' 
#' A grid pattern or grob is one value, even though it is a list.
#' @noRd
get_i <- function (val, i) {
  if (is_single(val) || length(val) <= 1) return (val)
  return (val[[(i - 1) %% length(val) + 1]])
}


#' Is `x` a grid pattern (e.g. `grid::linearGradient()`) or a grob?
#' 
#' These are lists, but each one is a single `fg` or `bg` value.
#' Neither grid nor ggplot2 exports an `is_pattern()` helper.
#' @noRd
is_single <- function (x) {
  inherits(x, "GridPattern") || grid::is.grob(x)
}


#' Is `x` a single NA (and not, e.g., a grob)?
#' @noRd
is_na <- function (x) {
  is.null(x) || (is.atomic(x) && length(x) == 1 && is.na(x))
}


#' Null-default operator (base R only has this since 4.4.0).
#' @noRd
`%||%` <- function (x, y) if (is.null(x)) y else x
