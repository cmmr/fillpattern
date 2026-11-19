
#' Pattern Names
#'
#' `fill_pattern_names` lists the base pattern names. Any pattern given to
#' [fill_pattern()] or [scale_fill_pattern()] begins with one of these,
#' optionally followed by modifiers (see the "Pattern Names" section of
#' [fill_pattern()]).
#'
#' `fill_pattern_order` lists the patterns that numbers map to:
#' `pattern = i` draws `fill_pattern_order[(i - 1) %% 35 + 1]`. The default
#' `scale_fill_pattern(patterns = seq_len)` uses them in this order.
#'
#' @format Character vectors. `fill_pattern_names` has 14 elements, and
#'         `fill_pattern_order` has 35.
#'
#' @seealso [fill_pattern()], [scale_fill_pattern()]
#'
#' @export
#' @examples
#'     library(grid)
#'     library(fillpattern)
#'
#'     fill_pattern_names
#'     head(fill_pattern_order)
#'
#'     # These are equivalent.
#'     fill <- fill_pattern(3)
#'     fill <- fill_pattern(fill_pattern_order[3])
#'
#'     # One rectangle per base pattern.
#'     grid.newpage()
#'     fills <- fill_pattern(fill_pattern_names, bg = "white")
#'     for (i in seq_along(fills))
#'       grid.rect(
#'         x      = ((i - 1) %% 5 + 0.5) / 5,
#'         y      = 1 - ((i - 1) %/% 5 + 0.5) / 3,
#'         width  = 0.18,
#'         height = 0.28,
#'         gp     = gpar(fill = fills[[i]]) )

fill_pattern_names <- c(
  "brick", "chevron", "fish", "grid", "herringbone", "hexagon", "octagon",
  "rain", "saw", "shingle", "rshingle", "stripe", "wave", "empty" )



#' @rdname fill_pattern_names
#' @export

fill_pattern_order <- c(
  "brick", "fish", "stripe45", "hexagon", "shingle45",
  "wave135", "grid", "octagon", "saw", "grid45",
  "rshingle90", "fish180", "stripe135", "wave45",
  "brick135", "saw45", "shingle", "fish90", "rain135",
  "rshingle135", "rshingle45", "saw90", "stripe",
  "brick45", "rshingle", "fish270", "fish135", "wave",
  "shingle90", "saw135", "wave90", "brick90",
  "fish45", "shingle135", "stripe90" )
