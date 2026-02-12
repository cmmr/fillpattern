# Changelog

## fillpattern 1.0.3

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
