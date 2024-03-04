
#' Patterned Fills for Grobs
#' 
#' 
#' 
#' 
#' @name fill_pattern
#'
#' @param pattern   The pattern specification. One of
#'        `"brick"`, `"fish"`, `"grid"`, `"hexagon"`, `"octagon"`, `"rain"`,
#'        `"saw"`, `"shingle"`, `"rshingle"`, `"stripe"`, or `"wave"`,
#'        optionally abbreviated and/or suffixed with modifers.
#'        See details section below.
#'        Default: `"brick"`
#'
#' @param color   The color of the pattern's lines.
#'        Default: `"black"`
#'
#' @param fill   The color (or grob) to use as the background.
#'        Default: `ifelse(invert, "black", "transparent")`
#'
#' @param angle   How much the rotate the pattern, given in degrees clockwise.
#'        Default: `0`
#'
#' @param size   The (approximate) size of the individual pattern elements.
#'        Assumed to be millimeters unless set otherwise with [unit()].
#'        Use a vector of length two to set width and height separately.
#'        Default: `5`
#'
#' @param lwd   Line width. A positive number.
#'        See [base::par()] for additional details. Default: `1`
#'
#' @param lty   Line type. One of `"solid"`, `"dashed"`, `"dotted"`, 
#'        `"dotdash"`, `"longdash"`, or `"twodash"`.
#'        See [base::par()] for additional details. Default: `"solid"`
#'
#' @param invert   Use `color` for the background, and `fill` for the
#'        foreground. Default: `FALSE`
#' 
#' 
#' @return `fill_pattern()` returns a single [grid::pattern()] object. 
#'         `fill_pattern_list()`, a list of [grid::pattern()] objects. 
#'         `fillPatternGrob()`, a [grid::gTree()] object.
#'  
#' 
#' @export
#' @examples
#'     library(grid)
#'     library(fillpattern)
#'     
#'     grid.newpage()
#'     grid.rect(width  = 0.5, height = 0.5, gp = gpar(fill = fill_pattern("fish")))


fill_pattern <- function (
    pattern = "brick", fg = "black", bg = "transparent", 
    angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NA ) {
  
  grid::pattern(
    group = FALSE,
    grob  = fillPatternGrob(
      pattern = pattern,
      fg      = fg,
      bg      = bg,
      angle   = angle,
      width   = width,
      height  = height,
      lwd     = lwd,
      lty     = lty,
      fun     = fun ))
}


#' @rdname fill_pattern
#' @export
fill_pattern_list <- function (
    pattern = "brick", fg = "black", bg = "transparent",
    angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NA ) {

  n <- length(pattern)
  if (n == 0) return (list())


  #________________________________________________________
  # Ensure lengths are 1 or same as number of patterns.
  #________________________________________________________
  for (i in c('fg', 'bg', 'angle', 'width', 'height', 'lwd', 'lty', 'fun')) {
    
    val <- get(i, inherits = FALSE)

    if (is.function(val)) val <- list(val)
    if (is.atomic(val))   val <- as.list(val)
    if (is.list(val) && length(val) == 1 && n > 1) val <- rep(val, n)

    if (length(val) != n)
      stop("Length of `", i, "` is not equal to 1 or length(pattern).")

    assign(i, val)
  }
  
  
  #________________________________________________________
  # Convert to a list of grid::pattern() objects.
  #________________________________________________________
  lapply(seq_along(pattern), function (i) {
    
    fill_pattern(
      pattern = pattern[[i]],
      fg      = fg[[i]],
      bg      = bg[[i]],
      angle   = angle[[i]],
      width   = width[[i]],
      height  = height[[i]],
      lwd     = lwd[[i]],
      lty     = lty[[i]],
      fun     = fun[[i]] )
  })

}




#' @rdname fill_pattern
#' @export
fillPatternGrob <- function (
    pattern = "brick", fg = "black", bg = "transparent",
    angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NA ) {
  
  for (i in formalArgs(fillPatternGrob))
    if (length(val <- get(i, inherits = FALSE)) != 1)
      stop("`", i, "` should be one value, not ", length(val), ".")
  
  grid::gTree(
    pattern = pattern,
    fg      = fg,
    bg      = bg,
    angle   = angle,
    width   = width,
    height  = height,
    lwd     = lwd,
    lty     = lty,
    fun     = fun,
    cl      = "fill_pattern" )
}


