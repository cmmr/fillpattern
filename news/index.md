# Changelog

## fillpattern (development version)

### New Features

- **Pattern Name Lists:** `fill_pattern_names` lists the 14 base pattern
  names, and `fill_pattern_order` lists the 35 patterns that numbers
  (and the default `scale_fill_pattern(patterns = seq_len)`) map to
  (trevorld/gridpattern#141).
- **Palette Function:** New
  [`pal_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  returns the palette that
  [`scale_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  uses, for `theme(palette.fill.discrete = )` in ggplot2 4.0 or later,
  or for other packages’ scales (trevorld/gridpattern#141).
- **No Lines:** `lwd = 0`, or the `"_0lwd"` pattern modifier, now draws
  no pattern lines (trevorld/gridpattern#141).
- **More Line Types:** `lty` accepts every line type that
  [`?par`](https://rdrr.io/r/graphics/par.html) does: the six names,
  `"blank"`, the integers 0 to 6, and hex dash patterns such as `"44"`
  or `"1342"` ([\#11](https://github.com/cmmr/fillpattern/issues/11)).

### Bug Fixes

- **Invalid Arguments:** Invalid arguments, such as a misspelled pattern
  name or an unsupported `lty`, no longer silently draw a solid block.
  They now give a warning when the pattern is created and are replaced
  with the default for that argument; an invalid pattern name keeps its
  valid parts, so `"stripes45"` becomes `"brick45"`.
  [`fillPatternGrob()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md)
  uses the first value of a longer argument, with a warning, instead of
  an error. Shapes smaller than `min_size` are still filled with solid
  `fg` color, without a warning, and now use the shape’s outline color
  when `fg = NA`. Any other failure to draw a pattern warns once per
  session ([\#11](https://github.com/cmmr/fillpattern/issues/11)).
- **Pattern and Grob Backgrounds:** `bg` can now be a grid pattern, such
  as [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html) or
  another
  [`fill_pattern()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md),
  passed directly or in a list, e.g. `bg = list(grad, "cyan")`.
  Previously,
  [`fill_pattern()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md)
  and
  [`scale_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  split the pattern into its components and quietly drew the wrong
  colors, and
  [`fillPatternGrob()`](https://cmmr.github.io/fillpattern/reference/fill_pattern.md)
  errored. A grob `bg` is now drawn as-is instead of being replaced by a
  solid `fg`-colored rectangle
  ([\#10](https://github.com/cmmr/fillpattern/issues/10)).

### Documentation

- **Tile Size:** A new “Tile Size” section explains how `width` and
  `height` are interpreted per shape, and when tiles are resized to stay
  between 1.5 mm and a quarter of the shape (trevorld/gridpattern#141).

## fillpattern 1.0.5

CRAN release: 2026-11-18

### New Features

- **Other Aesthetics and Scale Options:**
  [`scale_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  gains `aesthetics` and `...` arguments. Use
  `aesthetics = "pattern_fill"` for ‘ggpattern’ geoms, and pass `name`,
  `labels`, `breaks`, `limits`, `guide`, etc. through to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  ([\#8](https://github.com/cmmr/fillpattern/issues/8)).
- **Inherited Colors:** `fill_pattern(fg = NA)` and
  `fill_pattern(bg = NA)` now take their color from the outline (`col`)
  of the shape being filled.

### Bug Fixes

- **Extension Geoms:**
  [`scale_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  no longer errors in geoms that don’t call
  [`ggplot2::fill_alpha()`](https://ggplot2.tidyverse.org/reference/fill_alpha.html),
  such as `ggalluvial::geom_alluvium()`. The scale’s palette now returns
  real [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html)
  objects. In those geoms, `NA` colors come from the shape’s outline
  color, or fall back to black lines on white
  ([\#7](https://github.com/cmmr/fillpattern/issues/7)).
- **Color Lookup:** The `colour` aesthetic lookup no longer depends on a
  fixed call-stack depth. It now finds the data in the frame that called
  [`ggplot2::fill_alpha()`](https://ggplot2.tidyverse.org/reference/fill_alpha.html).
- **Transparent Colors:** A fully transparent `colour` (e.g. 
  ‘ggalluvial’’s default) is now treated as missing, so patterns stay
  visible.

## fillpattern 1.0.4

CRAN release: 2026-09-25

### New Features

- **Empty Pattern:** Added an `"empty"` pattern that renders only the
  background color, for use as a baseline alongside patterned fills
  ([@ulysg](https://github.com/ulysg),
  [\#5](https://github.com/cmmr/fillpattern/issues/5)).

## fillpattern 1.0.3

CRAN release: 2026-02-13

### New Features

- **Hex Logo:** Added an official hex logo to the package.

### Dependency Changes

- **R Version Requirement:** Bumped the minimum required R version from
  4.1.0 to 4.2.0 to ensure compatibility with recent graphics engine
  features.

### Bug Fixes

- **Size Parsing:** Fixed a bug where size modifier strings ending in a
  colon (e.g., `"brick_8mm:"`) were parsed incorrectly, applying the
  width dimension to the height.
- **Unit Validation:** `modify_size()` now gracefully handles and
  reports invalid unit strings instead of crashing, and includes support
  for standard units like `"in"`, `"inches"`, and `"cm"`.
- **Background Color:** Fixed an issue in
  [`scale_fill_pattern()`](https://cmmr.github.io/fillpattern/reference/scale_fill_pattern.md)
  where the background color was not correctly calculated from the
  aesthetic mapping when a foreground color was manually specified.

### Testing & Development

- **Code Coverage:** Added a comprehensive test suite using `testthat`,
  achieving 100% code coverage.
- **Badges:** Added a code coverage badge to the README.

## fillpattern 1.0.2

CRAN release: 2024-06-24

- Fixed crash on very small fill areas.
- New parameter `min_size` to use solid fills for small areas.

## fillpattern 1.0.1

CRAN release: 2024-03-09

- Allow installation on R 4.1 or later (for conda-forge compatibility).

## fillpattern 1.0.0

CRAN release: 2024-03-08

- Initial CRAN submission.
