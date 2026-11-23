# Patterned Fills for ggplot.

Patterned Fills for ggplot.

## Usage

``` r
scale_fill_pattern(
  patterns = seq_len,
  fg = NA,
  bg = NULL,
  fade = NULL,
  alpha = 1,
  angle = 0,
  width = unit(1/10, "npc"),
  height = NA,
  lwd = 1,
  lty = "solid",
  fun = NULL,
  min_size = 2,
  aesthetics = "fill",
  ...
)

pal_fill_pattern(
  patterns = seq_len,
  fg = NA,
  bg = NULL,
  fade = NULL,
  alpha = 1,
  angle = 0,
  width = unit(1/10, "npc"),
  height = NA,
  lwd = 1,
  lty = "solid",
  fun = NULL,
  min_size = 2
)
```

## Arguments

- patterns:

  A vector of pattern names that will be subset or recycled as needed to
  match the levels of the
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) fill
  variable. If integers are provided, they are mapped to the patterns in
  [fill_pattern_order](https://cmmr.github.io/fillpattern/reference/fill_pattern_names.md).
  See "Details" and "Pattern Names" sections below. Default: `seq_len`

- fg:

  Foreground color for the pattern's lines, or `NA` to use the color
  scale for the
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) color
  variable. Default: `NA`

- bg:

  Background color, grid pattern (such as
  [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html)), or
  grob, or `NA` to use the color scale for the
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) color
  variable. Default: `NULL`, which means `"transparent"` where `fg` is
  `NA`, and `NA` elsewhere.

- fade, alpha:

  Modify the color from the
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) color
  scale. Fade will make it more white, and alpha will make it more
  transparent. Both values must be between 0 and 1, inclusive, where 1
  means unchanged. Default: `fade = NULL`, which means 1 where `fg` is
  `NA` and 0.6 elsewhere; `alpha = 1`

- angle:

  How much the rotate the pattern, given in degrees clockwise. Default:
  `0`

- width:

  The width of the pattern tile. Assumed to be millimeters unless set
  otherwise with [`unit()`](https://rdrr.io/r/grid/unit.html). Default:
  `unit(1/10, 'npc')`

- height:

  The height of the pattern tile, or `NA` to match `width`. Assumed to
  be millimeters unless set otherwise with
  [`unit()`](https://rdrr.io/r/grid/unit.html). Default: `NA`

- lwd:

  Line width. A positive number, or `0` for no lines. See
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) for
  additional details. Default: `1`

- lty:

  Line type. One of `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`,
  `"longdash"`, `"twodash"`, or `"blank"`; an integer from 0 to 6; or a
  string of 2, 4, 6, or 8 hex digits, such as `"44"` or `"1342"`. See
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) for
  additional details. Default: `"solid"`

- fun:

  A function for modifying graphical parameters immediately before
  rendering. Should accept two parameters: `env`, an environment that
  the function should modify, and `row`, the row of transformed data
  that ggbuild has constructed for this grob (including aes mappings).
  The function should return a gTree or an error to force returning from
  the parent function immediately, or `NULL` to continue processing with
  the updated `env`. Default: `NULL`

- min_size:

  Minimum size of the pattern to draw. Applies to both width and height.
  Useful for avoiding CPU and memory overhead on tiny graphical
  elements. Shapes narrower or shorter than `min_size` are filled with
  solid `fg` color instead. Assumed to be millimeters unless set
  otherwise with [`unit()`](https://rdrr.io/r/grid/unit.html). Default:
  `2`

- aesthetics:

  The names of the aesthetics that this scale works with, for instance
  `"pattern_fill"` for geoms from the ggpattern package. Default:
  `"fill"`

- ...:

  Other arguments passed on to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
  such as `name`, `breaks`, `labels`, `limits`, `na.value`, `drop`, or
  `guide`.

## Value

`scale_fill_pattern()` returns a
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
object. `pal_fill_pattern()` returns a palette: a function that takes a
number of levels, `n`, and returns a list of `n`
[`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) objects. Use
it as `theme(palette.fill.discrete = pal_fill_pattern())` (ggplot2 4.0
or later), or as the palette of another package's scale.

## Details

All of the parameters can accept a vector of values or a function that
takes `n` as an argument and returns the value(s) to use. The values are
subset or recycled as needed to obtain the same number as
`nlevels(fill)`, where fill is the variable defined by `aes(fill = )`.
For example, a color palette function such as `bg = hcl.colors` or
`fg = viridisLite::viridis` gives each level its own color.

Invalid values give a warning when the plot is built, and are replaced
with the default for that parameter, as described in
[`fill_pattern()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md).

## Color Mapping

When `fg` or `bg` is `NA`, the pattern takes its color from the `colour`
aesthetic of the same data row. Geoms that apply `alpha` to their fill
with
[`ggplot2::fill_alpha()`](https://ggplot2.tidyverse.org/reference/fill_alpha.html)
(all of ggplot2's own geoms, and many extensions) let
`scale_fill_pattern()` look up that row directly. For other geoms, the
pattern uses the outline color (`col`) that the geom gives the filled
shape, if there is exactly one; otherwise it uses black lines on a white
background. For example, `ggalluvial::geom_alluvium()` only gives its
flows an outline color with `outline.type = "full"`.

## Pattern Names

**Base name:**

- Pattern names must always begin with one of `"brick"`, `"chevron"`,
  `"fish"`, `"grid"`, `"herringbone"`, `"hexagon"`, `"octagon"`,
  `"rain"`, `"saw"`, `"shingle"`, `"rshingle"`, `"stripe"`, `"wave"`, or
  `"empty"`. These are also available as
  [fill_pattern_names](https://cmmr.github.io/fillpattern/reference/fill_pattern_names.md).

- `"empty"` draws no pattern lines, only the `bg` color. This is useful
  for a baseline or control group shown next to patterned groups.
  Modifiers are accepted but have no visible effect.

- These names support partial matching, e.g. `"her"`, `"herring"`, and
  `"herringbone"` are all valid. However, tiling designs may be added in
  the future, so it is recommended to use the full names in finished
  code.

**Angle modifier:**

- A number immediately following the tiling design, such as
  `"stripe45"`, `"fish180"`, or `"saw20"`.

- Is added to the `angle` argument; `fill_pattern("brick45", angle=45)`
  is equivalent to `fill_pattern("brick90")`.

**Width and height modifier:**

- An underscore followed by a single size to be used for both width and
  height.

- Or, an underscore followed by the new width and height separated by a
  colon.

- Can be absolute sizes (`"grid_4"` or `"hex_5mm:0.1npc"`) or relative
  to the `width` and `height` arguments (`"saw_sm"` or `"brick_*2:/2"`).
  The shorthand values `"xs"`, `"sm"`, `"md"`, `"lg"`, and `"xl"` are
  equivalent to `"/4"`, `"/2"`, `"1"`, `"*2"`, and `"*4"`, respectively.

**Line width and style:**

- An underscore, followed by a number, followed by one of `"solid"`,
  `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, or `"twodash"`. For
  example, `"shingle_0.5dashed"` or `"wave_2solid"`.

- The number component is optional, so `"oct_longdash"` is also valid,
  and will use `lwd` for the line width.

- To specify just the line width, suffix the number with "lwd":
  `"grid_2lwd"` will use `lty` for the line style. `"_0lwd"` draws no
  lines.

**Combinations:**

- Modifiers can be combined in any order. For example,
  `"hex_lg:xl_2dotted"` or `"grid45_dashed_1.4lwd_:6mm_sm:"`.

## Tile Size

`width` and `height` set the size of one tile of the pattern. The
pattern is drawn separately for each shape it fills, so `"npc"` units
are relative to that shape's bounding box, not to the whole plot or
page.

Tiles that are out of bounds in both directions are resized when drawn.
A tile is out of bounds horizontally if it is narrower than 1.5 mm or
wider than a quarter of the shape, and vertically if it is shorter than
1.5 mm or taller than a quarter of the shape. Such a tile is scaled,
keeping its aspect ratio, by the smallest amount that brings one of its
dimensions back within bounds. This keeps several tiles visible in small
shapes such as legend keys, and keeps patterns from becoming too fine to
see. So the drawn size may not exactly match `width` and `height`.

Shapes narrower or shorter than `min_size` are filled with solid `fg`
color instead of a pattern.

## See also

[`fill_pattern()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md)
for base `grid` graphics integration.

## Examples

``` r
 # \donttest{
    library(ggplot2)
    library(fillpattern)
    
    ggplot(mpg, aes(x = class, y = hwy, color = class, fill = class)) +
      geom_boxplot() +
      scale_fill_pattern()


    ggplot(mpg, aes(x = drv, y = hwy, color = drv, fill = drv)) +
      geom_violin() +
      scale_colour_brewer(palette = "Set1") + 
      scale_fill_pattern(c("brick", "stripe45", "grid45_lg"), fg = "black")


    ggplot(mpg, aes(x = drv, color = drv, fill = drv)) +
      geom_bar() +
      scale_fill_pattern(
        patterns = c("hex_sm", "brick90_xl", "fish"),
        lty      = c("solid", "twodash", "dotted"),
        lwd      = c(2, 3, 1) ) +
      theme(legend.key.size = unit(2, 'cm'))

    
    # A color palette function gives each level its own background.
    ggplot(mpg, aes(x = drv, fill = drv)) +
      geom_bar() +
      scale_fill_pattern(fg = "black", bg = function (n) hcl.colors(n, "Pastel 1"))

    
    # Patterns as the default discrete fill palette (ggplot2 >= 4.0).
    if (packageVersion("ggplot2") >= "4.0.0")
      ggplot(mpg, aes(x = drv, fill = drv)) +
        geom_bar() +
        theme(palette.fill.discrete = pal_fill_pattern(fg = "black"))

 # }
```
