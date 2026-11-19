
# Argument validation for fill_pattern(), fillPatternGrob(), and
# scale_fill_pattern().
#
# Invalid values are replaced with a safe default and a warning, when the
# pattern is created. By the time makeContent.fill_pattern() runs, every
# argument should be valid, so a failure there is unexpected (#11).



arg_defaults <- list(
  pattern = "brick", patterns = "brick", fg = "black", bg = "transparent",
  angle = 0, width = 5, height = NA, lwd = 1, lty = "solid", fun = NULL,
  min_size = 2, fade = 1, alpha = 1 )

lty_names <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

size_units <- c(
  "npc", "mm", "points", "picas", "bigpts", "dida",
  "cicero", "scaledpts", "lines", "char", "native", "snpc",
  "in", "inches", "cm" )



#' Parse a pattern specification such as `"brick45_lg_2dashed"`.
#'
#' Only string handling: sizes are checked here, but applied at draw time,
#' since `"npc"` sizes depend on the viewport.
#'
#' @noRd
#' @keywords internal
#'
#' @param pattern   A pattern name, or an index into `fill_pattern_order`.
#'
#' @return A list with `name` (full base name), `angle` (degrees to add),
#'         `lwd` and `lty` (`NULL` unless set by a modifier), `sizes` (a list
#'         of `c(width, height)` strings for `modify_size()`), `problems` (a
#'         character vector, empty if the pattern is valid), and `clean` (the
#'         pattern with any invalid parts replaced or dropped).

parse_pattern <- function (pattern) {

  res <- list(
    name = "brick", angle = 0, lwd = NULL, lty = NULL,
    sizes = list(), problems = character(0), clean = pattern )

  problem <- function (...) res$problems <<- c(res$problems, paste0(...))


  #________________________________________________________
  # Map integers to predefined styles.
  #________________________________________________________
  if (is.numeric(pattern) && length(pattern) == 1 && !is.na(pattern)) {
    if (!is.finite(pattern) || pattern %% 1) {
      problem("A numeric pattern must be a whole number.")
      res$clean <- "brick"
      return (res)
    }
    pattern <- fill_pattern_order[[(pattern - 1) %% length(fill_pattern_order) + 1]]
  }

  if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern)) {
    problem("A pattern must be a name, such as \"brick\", or a number.")
    res$clean <- "brick"
    return (res)
  }


  parts <- strsplit(tolower(trimws(pattern)), '_', fixed = TRUE)[[1]]
  keep  <- character(0)


  #________________________________________________________
  # Base name, optionally followed by an angle.
  #________________________________________________________
  base  <- if (length(parts)) parts[[1]] else ""
  m     <- regmatches(base, regexec('^([a-z]*)([0-9]+\\.?[0-9]*)?$', base))[[1]]
  name  <- if (length(m)) m[[2]] else base
  angle <- if (length(m)) m[[3]] else ""

  if (is.na(pmatch(name, fill_pattern_names))) {
    problem(
      "\"", name, "\" isn't a pattern name, or an unambiguous ",
      "abbreviation of one. Use one of ", 
      paste(collapse = ", ", dQuote(fill_pattern_names, FALSE)), "." )
    name <- "brick"
  }

  res$name <- fill_pattern_names[[pmatch(name, fill_pattern_names)]]
  if (nzchar(angle)) res$angle <- as.numeric(angle)
  keep <- paste0(name, angle)


  #________________________________________________________
  # Line width/style and size modifiers.
  #________________________________________________________
  lwd_re <- paste0(
    '^([0-9]+\\.?[0-9]*|\\.[0-9]+)?(lwd|',
    paste(collapse = '|', lty_names), ')$' )

  for (part in parts[-1]) {

    if (!nzchar(part)) next

    if (grepl(lwd_re, part)) {

      lwd_mod <- as.numeric(sub("[a-z]+$", "", part))
      lty_mod <- sub("^[0-9.]*", "", part)

      if (!is.na(lwd_mod))  res$lwd <- lwd_mod
      if (lty_mod != "lwd") res$lty <- lty_mod

    } else {

      size_mods <- strsplit(part, ':', fixed = TRUE)[[1]]
      if (endsWith(part, ":")) size_mods <- c(size_mods, "")

      if (length(size_mods) > 2 || !all(vapply(size_mods, is_size_mod, logical(1)))) {
        problem(
          "\"", part, "\" isn't a modifier. Use a size such as \"lg\", ",
          "\"*2\", \"8mm\", or \"5mm:0.1npc\", or a line such as \"2lwd\" ",
          "or \"dashed\"." )
        next
      }

      res$sizes <- c(res$sizes, list(c(head(size_mods, 1), tail(size_mods, 1))))
    }

    keep <- c(keep, part)
  }


  if (length(res$problems))
    res$clean <- paste(collapse = "_", keep)

  return (res)
}



#' Is `str` a size that `modify_size()` understands?
#'
#' @noRd
#' @keywords internal
#'
#' @param str   A size string such as `"lg"`, `"*2"`, `"/5"`, `"8mm"`, `"0.1npc"`,
#'        or `""` (no change).
#'
#' @return `TRUE` or `FALSE`.

is_size_mod <- function (str) {

  if (!nzchar(str))                              return (TRUE)
  if (str %in% c('xs', 'sm', 'md', 'lg', 'xl')) return (TRUE)

  if (startsWith(str, "*") || startsWith(str, "/")) {
    val <- suppressWarnings(as.numeric(substr(str, 2, nchar(str))))
    return (isTRUE(is.finite(val) && val > 0))
  }

  val <- sub("[a-z]+$", "", str)
  u   <- substr(str, nchar(val) + 1, nchar(str))
  val <- suppressWarnings(as.numeric(val))

  isTRUE(is.finite(val) && val != 0) &&
    (!nzchar(u) || !is.na(pmatch(u, size_units)))
}



#' Check a line type, accepting everything that `?par` does.
#'
#' @noRd
#' @keywords internal
#'
#' @param x   A line type: an integer from 0 to 6, a name such as `"dashed"`
#'        (or an unambiguous abbreviation, such as `"dash"`), `"blank"`, or a
#'        string of 2, 4, 6, or 8 hex digits, such as `"44"` or `"1342"`.
#'
#' @return The line type for [grid::gpar()], or `NULL` if `x` isn't valid.

check_lty <- function (x) {

  if (length(x) != 1 || is.na(x)) return (NULL)

  if (is.numeric(x))
    return (if (is.finite(x) && x %in% 0:6) as.integer(x))

  if (!is.character(x)) return (NULL)

  x <- trimws(x)
  if (grepl('^[0-6]$', x)) return (as.integer(x))

  i <- pmatch(tolower(x), c(lty_names, "blank"))
  if (!is.na(i)) return (c(lty_names, "blank")[[i]])

  if (grepl('^([1-9a-fA-F]{2}){1,4}$', x)) return (x)

  return (NULL)
}



#' Is `x` a single valid color (or `NA`)?
#' @noRd
is_color <- function (x) {
  if (is_na(x)) return (TRUE)
  if (!(is.character(x) || is.numeric(x)) || length(x) != 1) return (FALSE)
  !inherits(tryCatch(grDevices::col2rgb(x), error = identity), "error")
}



#' Is `x` a single positive size (number or unit), or `NA` if allowed?
#' @noRd
is_size <- function (x, na_ok = FALSE, zero_ok = FALSE) {

  if (length(x) != 1) return (FALSE)

  if (grid::is.unit(x)) {
    # Can't check the sign of e.g. unit(1, 'npc') - unit(2, 'mm').
    if (!inherits(x, "simpleUnit")) return (TRUE)
    x <- as.numeric(x)
  }

  if (is.atomic(x) && is.na(x) && !is.nan(x)) return (na_ok)
  if (!is.numeric(x) || !is.finite(x))        return (FALSE)

  if (zero_ok) x >= 0 else x > 0
}



#' Validate one argument value.
#'
#' @noRd
#' @keywords internal
#'
#' @param arg   The argument's name, e.g. `"lty"` or `"pattern"`.
#' @param val   A single value for it.
#' @param label   How to refer to the value in a warning, e.g. `"lty[2]"`.
#'
#' @return `val`, or a safe default with a warning if `val` is invalid.

check_value <- function (arg, val, label = arg) {

  # A one-element list, e.g. `bg = list(list(grad), "cyan")`, holds one value.
  if (is.list(val) && !is_single(val) && length(val) == 1)
    val <- val[[1]]

  fix <- function (problem, fallback) {
    warn_invalid(label, val, problem, fallback)
    return (fallback)
  }

  switch(
    EXPR = arg,

    patterns = ,
    pattern = {
      res <- parse_pattern(val)
      if (length(res$problems)) return (fix(res$problems, res$clean))
    },

    fg = {
      if (!is_color(val))
        return (fix("Use a color, or `NA` for the shape's outline color.", "black"))
    },

    bg = {
      if (!is_single(val) && !is_color(val))
        return (fix(paste(
          "Use a color, a grid pattern such as `grid::linearGradient()`, a grob,",
          "or `NA` for the shape's outline color." ), "transparent"))
    },

    angle = {
      if (!is.numeric(val) || length(val) != 1 || !is.finite(val))
        return (fix("Use a number of degrees.", 0))
    },

    width = {
      if (!is_size(val, na_ok = TRUE))
        return (fix("Use a positive number of millimeters, or a `unit()`.", 5))
    },

    height = {
      if (!is_size(val, na_ok = TRUE))
        return (fix("Use a positive number of millimeters, a `unit()`, or `NA`.", NA))
    },

    lwd = {
      if (!is.numeric(val) || length(val) != 1 || !isTRUE(is.finite(val) && val >= 0))
        return (fix("Use a positive number, or 0 for no lines.", 1))
    },

    lty = {
      lty <- check_lty(val)
      if (is.null(lty))
        return (fix(paste0(
          "Use one of ", paste(collapse = ", ", dQuote(lty_names, FALSE)),
          ", \"blank\", an integer from 0 to 6, or a string of 2, 4, 6, or 8 ",
          "hex digits such as \"44\". See `?par`." ), "solid"))
      return (lty)
    },

    fun = {
      if (!is.null(val) && !is.function(val))
        return (fix("Use a function or `NULL`.", NULL))
    },

    min_size = {
      if (!is_size(val, zero_ok = TRUE))
        return (fix("Use a number of millimeters (0 or more), or a `unit()`.", 2))
    },

    fade  = ,
    alpha = {
      if (!is.numeric(val) || length(val) != 1 || !isTRUE(val >= 0 && val <= 1))
        return (fix("Use a number from 0 to 1.", 1))
    }
  )

  return (val)
}



#' Validate a list of arguments.
#'
#' @noRd
#' @keywords internal
#'
#' @param args   A named list of arguments.
#' @param single   `TRUE` if each argument should be one value, as for
#'        `fillPatternGrob()`. `FALSE` if each can be a vector or list of
#'        values, as for `fill_pattern()`; each element is then checked.
#'
#' @return `args`, with invalid values replaced by safe defaults.

check_args <- function (args, single = TRUE) {

  for (arg in names(args)) {

    val <- args[[arg]]
    n   <- if (is_single(val) || is.function(val)) 1L else length(val)

    if (n == 1 || (arg == 'fun' && is.null(val))) {
      args[arg] <- list(check_value(arg, val))
      next
    }

    if (n == 0 || single) {
      fallback <- if (n) val[[1]] else arg_defaults[[arg]]
      warn_invalid(arg, val,
        paste0("`", arg, "` should be one value, not ", n, "."), fallback )
      args[arg] <- list(check_value(arg, fallback))
      next
    }

    # Check each element, keeping `val` as-is unless something changes.
    checked <- lapply(seq_len(n), function (i) {
      check_value(arg, val[[i]], paste0(arg, "[", i, "]")) })
    changed <- !mapply(identical, checked, lapply(seq_len(n), function (i) val[[i]]))
    if (any(changed)) args[[arg]] <- checked
  }

  # Can't infer both sizes from each other.
  if (all(c('width', 'height') %in% names(args)))
    if (identical(args$width, NA) && identical(args$height, NA)) {
      warn_invalid("width", NA,
        "`width` and `height` can't both be `NA`.", 5)
      args$width <- 5
    }

  return (args)
}



#' Warn that an argument value was replaced.
#' @noRd
warn_invalid <- function (label, val, problem, fallback) {
  warn_cnd(
    class   = "fillpattern_invalid_arg",
    message = paste0(
      "`", label, "` = ", format_value(val), " is invalid.\n",
      paste0("  ", problem, "\n", collapse = ""),
      "  Using ", format_value(fallback), " instead." ))
}



#' Package state, e.g. whether the draw-time fallback has warned yet.
#' @noRd
the <- new.env(parent = emptyenv())



#' Warn that a pattern couldn't be drawn, once per session.
#' 
#' Patterns are redrawn on every resize, so a warning per shape and redraw 
#' would be a flood. Set `options(fillpattern.fallback_warning = "always")` to 
#' warn every time.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param e   The error that stopped the pattern from being drawn.

warn_fallback <- function (e) {
  
  always <- identical(getOption("fillpattern.fallback_warning"), "always")
  if (!always && isTRUE(the$fallback_warned)) return (invisible())
  the$fallback_warned <- TRUE
  
  warn_cnd(
    class   = "fillpattern_fallback",
    message = paste0(
      "Can't draw fill pattern; drawing a solid `fg` fill instead.\n",
      "  ", conditionMessage(e), 
      if (!always) "\n  This warning is shown once per session." ))
}



#' Signal a warning with a class, without the call.
#' @noRd
warn_cnd <- function (message, class) {
  warning(structure(
    class = c(class, "warning", "condition"),
    list(message = message, call = NULL) ))
}



#' Describe a value in a warning message.
#' @noRd
format_value <- function (x) {
  if (is.null(x))                      return ("`NULL`")
  if (grid::is.unit(x))                return (paste0("`unit(", format(x), ")`"))
  if (is.function(x))                  return ("a function")
  if (is_single(x))                    return (paste0("a <", class(x)[[1]], ">"))
  if (is.atomic(x) && length(x) <= 3) {
    if (length(x) == 1 && is.na(x))    return ("`NA`")
    x <- if (is.character(x)) dQuote(x, FALSE) else as.character(x)
    return (if (length(x) == 1) x else paste0("c(", paste(x, collapse = ", "), ")"))
  }
  paste0("a ", class(x)[[1]], " of length ", length(x))
}
