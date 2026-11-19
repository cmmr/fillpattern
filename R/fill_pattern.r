
#' Patterned Fills for Grobs
#' 
#' 
#' @name fill_pattern
#'
#' @param patterns,pattern   The pattern specification. Options are `"brick"`, 
#'        `"chevron"`, `"fish"`, `"grid"`, `"herringbone"`, `"hexagon"`, 
#'        `"octagon"`, `"rain"`, `"saw"`, `"shingle"`, `"rshingle"`, 
#'        `"stripe"`, `"wave"`, and `"empty"`, optionally abbreviated and/or
#'        suffixed with modifiers. See "Pattern Names" section below.
#'        Default: `"brick"`
#'
#' @param fg   Foreground color, for the pattern's lines, or `NA` to use the
#'        outline color (`col`) of the shape being filled. Default: `"black"`
#'
#' @param bg   Background color, grid pattern (such as 
#'        [grid::linearGradient()] or another `fill_pattern()`), or grob, 
#'        or `NA` to use the outline color (`col`) of the shape being 
#'        filled. Default: `"transparent"`
#'
#' @param angle   How much the rotate the pattern, given in degrees clockwise.
#'        Default: `0`
#'
#' @param width   The width of the pattern tile. Assumed to be millimeters 
#'        unless set otherwise with [unit()]. Default: `5`
#' 
#' @param height The height of the pattern tile, or `NA` to match `width`. 
#'        Assumed to be millimeters unless set otherwise with [unit()].
#'        Default: `NA`
#'
#' @param lwd   Line width. A positive number, or `0` for no lines.
#'        See [graphics::par()] for additional details. Default: `1`
#'
#' @param lty   Line type. One of `"solid"`, `"dashed"`, `"dotted"`, 
#'        `"dotdash"`, `"longdash"`, `"twodash"`, or `"blank"`; an integer 
#'        from 0 to 6; or a string of 2, 4, 6, or 8 hex digits, such as 
#'        `"44"` or `"1342"`. See [graphics::par()] for additional details. 
#'        Default: `"solid"`
#'
#' @param fun   A function for modifying graphical parameters immediately 
#'        before rendering. Should accept two parameters: `env`, an 
#'        environment that the function should modify, and `row`, the row of 
#'        transformed data that ggbuild has constructed for this grob 
#'        (including aes mappings). The function should return a gTree or an 
#'        error to force returning from the parent function immediately, or 
#'        `NULL` to continue processing with the updated `env`. Default: `NULL`
#' 
#' @param min_size   Minimum size of the pattern to draw. Applies to both width 
#'        and height. Useful for avoiding CPU and memory overhead on tiny 
#'        graphical elements. Shapes narrower or shorter than `min_size` are 
#'        filled with solid `fg` color instead. Assumed to be millimeters 
#'        unless set otherwise with [unit()]. Default: `2`
#' 
#' 
#' @details `fillPatternGrob()` expects a single value for each parameter, 
#'          and uses the first if given more. `fill_pattern()` can accept a 
#'          vector of values for each parameter which are subset or recycled 
#'          as needed to obtain the same number as `length(patterns)`.
#' 
#' Invalid values give a warning when the pattern is created, and are 
#' replaced with the default for that parameter. For an invalid pattern 
#' name, only the invalid parts are replaced or dropped: `"stripes45_lg"` 
#' becomes `"brick45_lg"`, and `"fish_huge"` becomes `"fish"`. If a pattern 
#' still can't be drawn, it is filled with solid `fg` color instead, with a 
#' warning that is shown once per session.
#' 
#' When `fg` or `bg` is `NA`, the color is looked up when the pattern is 
#' drawn: it is the `col` of the shape the pattern fills, or `"black"` (for 
#' `fg`) / `"white"` (for `bg`) if that shape has no single, visible `col`. A 
#' grob that draws several shapes, such as `rectGrob(x = 1:3/4, ...)`, only 
#' qualifies if all of them have the same `col`.
#' 
#' 
#' @return `fill_pattern()` returns a list of `grid::pattern()` objects; 
#'         `fillPatternGrob()` returns a `grid::gTree()` object.
#' 
#' 
#' @section Pattern Names:
#' 
#' **Base name:**
#' * Pattern names must always begin with one of `"brick"`, `"chevron"`, 
#'   `"fish"`, `"grid"`, `"herringbone"`, `"hexagon"`, `"octagon"`, `"rain"`, 
#'   `"saw"`, `"shingle"`, `"rshingle"`, `"stripe"`, `"wave"`, or `"empty"`. 
#'   These are also available as [fill_pattern_names].
#' * `"empty"` draws no pattern lines, only the `bg` color. This is useful
#'   for a baseline or control group shown next to patterned groups. Modifiers
#'   are accepted but have no visible effect.
#' * These names support partial matching, e.g. `"her"`, `"herring"`, and 
#'   `"herringbone"` are all valid. However, tiling designs may be added in 
#'   the future, so it is recommended to use the full names in finished code.
#' 
#' **Angle modifier:**
#' * A number immediately following the tiling design, such as `"stripe45"`, 
#'   `"fish180"`, or `"saw20"`.
#' * Is added to the `angle` argument; `fill_pattern("brick45", angle=45)` is 
#'   equivalent to `fill_pattern("brick90")`.
#' 
#' **Width and height modifier:**
#' * An underscore followed by a single size to be used for both width and height.
#' * Or, an underscore followed by the new width and height separated by a colon.
#' * Can be absolute sizes (`"grid_4"` or `"hex_5mm:0.1npc"`) or relative to 
#'   the `width` and `height` arguments (`"saw_sm"` or `"brick_*2:/2"`). The 
#'   shorthand values `"xs"`, `"sm"`, `"md"`, `"lg"`, and `"xl"` are equivalent 
#'   to `"/4"`, `"/2"`, `"1"`, `"*2"`, and `"*4"`, respectively.
#' 
#' **Line width and style:**
#' * An underscore, followed by a number, followed by one of `"solid"`, 
#'   `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, or `"twodash"`. For 
#'   example, `"shingle_0.5dashed"` or `"wave_2solid"`.
#' * The number component is optional, so `"oct_longdash"` is also valid, and 
#'   will use `lwd` for the line width.
#' * To specify just the line width, suffix the number with "lwd": 
#'   `"grid_2lwd"` will use `lty` for the line style. `"_0lwd"` draws no 
#'   lines.
#' 
#' **Combinations:**
#' * Modifiers can be combined in any order. For example, `"hex_lg:xl_2dotted"` 
#'   or `"grid45_dashed_1.4lwd_:6mm_sm:"`.
#' 
#' 
#' @section Tile Size:
#' 
#' `width` and `height` set the size of one tile of the pattern. The pattern 
#' is drawn separately for each shape it fills, so `"npc"` units are relative 
#' to that shape's bounding box, not to the whole plot or page.
#' 
#' Tiles that are out of bounds in both directions are resized when drawn. 
#' A tile is out of bounds horizontally if it is narrower than 1.5 mm or wider 
#' than a quarter of the shape, and vertically if it is shorter than 1.5 mm 
#' or taller than a quarter of the shape. Such a tile is scaled, keeping its 
#' aspect ratio, by the smallest amount that brings one of its dimensions back 
#' within bounds. This keeps several tiles visible in small shapes such as 
#' legend keys, and keeps patterns from becoming too fine to see. So the 
#' drawn size may not exactly match `width` and `height`.
#' 
#' Shapes narrower or shorter than `min_size` are filled with solid `fg` 
#' color instead of a pattern.
#' 
#' 
#' @seealso [scale_fill_pattern()] for `ggplot2` integration.
#' 
#' @export
#' @examples
#'     library(grid)
#'     library(fillpattern)
#'     
#'     grid.newpage()
#'     grid.rect(gp = gpar(fill = fill_pattern("brick", bg = "gray", angle = 90)))
#'     
#'     grid.newpage()
#'     gp <- Map(gpar, fill = fill_pattern(
#'       patterns = c("grid_3lwd", "stripe_longdash", "herringbone45", "hexagon_lg"),
#'       fg       = c("black",     "white",           "black",         "blue"),
#'       bg       = c("white",     "black",           "cyan",          "beige") ))
#'     grid.circle( gp = gp[[1]], x = 1/4, y = 3/4, r = 1/5)
#'     grid.polygon(gp = gp[[2]], x = c(9,12,15)/16, y = c(15,9,15)/16)
#'     grid.rect(   gp = gp[[3]], x = 1/4, y = 1/4, width = 2/5, height = 2/5)
#'     grid.rect(   gp = gp[[4]], x = 3/4, y = 1/4, width = 2/5, height = 2/5)
#'     
#'     # fg = NA picks up the outline color of each shape.
#'     grid.newpage()
#'     fill <- fill_pattern("stripe45", fg = NA)
#'     grid.circle(x = 1/4, r = 1/5, gp = gpar(col = "red",  fill = fill))
#'     grid.circle(x = 3/4, r = 1/5, gp = gpar(col = "blue", fill = fill))

fill_pattern <- function (
    patterns = "brick", fg = "black", bg = "transparent",
    angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NULL,
    min_size = 2 ) {
  
  n <- length(patterns)
  if (n == 0) return (list())
  
  
  #________________________________________________________
  # Replace invalid values with defaults, with a warning.
  #________________________________________________________
  args <- check_args(single = FALSE, args = list(
    patterns = patterns, fg = fg, bg = bg, angle = angle, width = width, 
    height = height, lwd = lwd, lty = lty, fun = fun, min_size = min_size ))
  
  
  #________________________________________________________
  # Convert to a list of grid::pattern() objects.
  #________________________________________________________
  fills <- lapply(seq_along(patterns), function (i) {
    
    grid::pattern(
      group = FALSE,
      grob  = fillPatternGrob(
        pattern  = get_i(args$patterns, i),
        fg       = get_i(args$fg,       i),
        bg       = get_i(args$bg,       i),
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



#' @rdname fill_pattern
#' @export
fillPatternGrob <- function (
    pattern = "brick", fg = "black", bg = "transparent",
    angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NULL,
    min_size = 2 ) {
  
  
  #________________________________________________________
  # Replace invalid values with defaults, with a warning.
  #________________________________________________________
  list2env(check_args(as.list(environment())), environment())
  
  if (!is.unit(min_size)) min_size <- unit(min_size, 'mm')
  
  grid::gTree(
    pattern  = pattern,
    fg       = fg,
    bg       = bg,
    angle    = angle,
    width    = width,
    height   = height,
    lwd      = lwd,
    lty      = lty,
    fun      = fun,
    min_size = min_size,
    cl       = "fill_pattern" )
}

