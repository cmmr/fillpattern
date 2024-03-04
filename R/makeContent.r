


#' Render the pattern, adapting to device size.
#'
#' @noRd
#' @keywords internal
#' @export

makeContent.fill_pattern <- function (gt) {
  
  #________________________________________________________
  # Allow fill_pattern() arguments to be functions.
  #________________________________________________________
  for (i in c('pattern', 'fg', 'bg', 'angle', 'width', 'height', 'lwd', 'lty', 'fun'))
    assign(i, gt[[i]])
  
  
  #________________________________________________________
  # Map integers to predefined styles.
  #________________________________________________________
  if (is.numeric(pattern) && !is.na(pattern) && !pattern %% 1) {
    
    choices <- c(
      "brick", "fish", "stripe45", "hexagon", "shingle45",
      "wave135", "grid", "octagon", "saw", "grid45",
      "rshingle90", "fish180", "stripe135", "wave45",
      "brick135", "saw45", "shingle", "fish90", "rain135",
      "rshingle135", "rshingle45", "saw90", "stripe",
      "brick45", "rshingle", "fish270", "fish135", "wave",
      "shingle90", "saw135", "wave90", "brick90",
      "fish45", "shingle135", "stripe90" )
    pattern <- choices[((pattern - 1) %% length(choices)) + 1]
    remove("choices")
    
  } else {
    pattern <- tolower(trimws(pattern))
  }
  
  
  
  #________________________________________________________
  # Interpret SIZE for current device's width/height.
  #________________________________________________________
  
  if (grid::is.unit(width))  width  <- grid::convertWidth(width,   'mm', TRUE)
  if (grid::is.unit(height)) height <- grid::convertHeight(height, 'mm', TRUE)
  if (is.na(width))          width  <- height
  if (is.na(height))         height <- width
  stopifnot(is.numeric(c(width, height)))
  
  
  
  #________________________________________________________
  # Parse LINE and SIZE component(s) from the pattern.
  #________________________________________________________
  if (grepl(pattern = '_', x = pattern, fixed = TRUE)) {
    
    parts   <- strsplit(pattern, '_')[[1]]
    pattern <- parts[[1]]
    
    for (part in parts[-1]) {
      
      if (grepl("(solid|dashed|dotted|dotdash|longdash|twodash)$", part)) {
        lwd_mod <- sub("[a-z]+$", "", part)
        lty     <- sub(lwd_mod, "", part, fixed = TRUE)
        lwd_mod <- abs(as.numeric(lwd_mod))
        if (!is.na(lwd_mod) && lwd > 0) lwd <- lwd_mod
        
      } else {
        size_mods <- strsplit(part, ':')[[1]]
        width     <- modify_size(width,  'x', head(size_mods, 1))
        height    <- modify_size(height, 'y', tail(size_mods, 1))
      }
      
    }
    
    remove("parts", "part")
  }
  
  
  
  #________________________________________________________
  # Pattern size limits. Also handy for legend key glyphs.
  #________________________________________________________
  w <- grid::convertWidth( unit(1, 'npc'), 'mm', TRUE)
  h <- grid::convertHeight(unit(1, 'npc'), 'mm', TRUE)
  
  # Only change when they are both out of bounds.
  if (width > w/4 || width < 1.5)
    if (height > w/4 || height < 1.5) {
      
      adj_x_size <- min(w/4, max(1.5, width))
      adj_y_size <- min(h/4, max(1.5, height))
      
      # Make smallest change to get one back in-bounds, 
      # then scale the other dimension by same amount.
      if (abs(adj_x_size - width) < abs(adj_y_size - height)) {
        height <- adj_x_size * (height / width)
        width <- adj_x_size
      } else {
        width <- adj_y_size * (width / height)
        height <- adj_y_size
      }
      
      remove("adj_x_size", "adj_y_size")
    }
  
  
  
  
  
  #________________________________________________________
  # Parse ANGLE component from the pattern.
  #________________________________________________________
  if (grepl(pattern = '^[a-z]+[0-9]+\\.{0,1}[0-9]*$', x = pattern)) {
    angle_mod <- sub("^[a-z]+", "", pattern)
    pattern   <- sub(angle_mod, "", pattern, fixed = TRUE)
    angle     <- angle + as.numeric(angle_mod)
  }
  stopifnot(is.numeric(angle) && !is.na(angle))


  #________________________________________________________
  # Validate graphical arguments.
  #________________________________________________________
  pattern <- local({
    choices <- c(
      "brick", "fish", "grid", "hexagon", "octagon", "rain",
      "saw", "shingle", "rshingle", "stripe", "wave" )
    tryCatch(
      expr  = match.arg(arg = pattern, choices = choices),
      error = function (e) {
        stop("pattern '", pattern, "' doesn't match options: ", paste(collapse = ", ", choices))
      })})

  lty <- local({
    choices <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    tryCatch(
      expr  = match.arg(arg = lty, choices = choices),
      error = function (e) {
        stop("lty (line type) '", lty, "' doesn't match options: ", paste(collapse = ", ", choices))
      })})

  stopifnot(is.numeric(lwd) && length(lwd) == 1 && isTRUE(lwd > 0))
  
  
  #________________________________________________________
  # Allow the user to make custom modifications.
  #________________________________________________________
  vp <- if (angle %% 360) grid::viewport(angle = -angle)
  bg <- if (!identical(bg, "transparent")) grid::rectGrob(gp = grid::gpar(fill = bg))
  gp <- grid::gpar(col = fg, lwd = lwd, lty = lty)
  
  if (is.function(fun)) fun(environment(), attr(fun, 'row', TRUE))


  #________________________________________________________
  # Expand to allow rotating without gaps.
  #________________________________________________________
  if (angle %% 180) {
    sq <- max(c(w, h, 2 * width, 2 * height))
    x_min <- y_min <- -0.5 * sq
    x_max <- y_max <-  1.5 * sq
    remove("sq")
  } else {
    x_min <- -2 * width
    x_max <-  2 * width + w
    y_min <- -2 * height
    y_max <-  2 * height + h
  }


  #________________________________________________________
  # Lists for the pattern-building functions to use.
  #________________________________________________________
  X <- list(s = width,  b = c(x_min, x_max))
  Y <- list(s = height, b = c(y_min, y_max))

  for (i in c('X', 'Y'))
    assign(i, within(get(i), {
      p <- c(
        rev(seq(from = 0, to = b[[1]], by = -s)),
        seq(from = s, to = b[[2]], by =  s) )
      n  <- length(p)
      l  <- min(p)
      h  <- max(p)
      i  <- seq_len(n)
      m2 <- i %% 2
      e  <- p[which(m2 == 0)]
      o  <- p[which(m2 == 1)]
      ne <- length(e)
      no <- length(o)
    }))

  mapply(
    from = c('n', 'ne', 'no'),
    to   = c('m', 'me', 'mo'),
    FUN  = function (from, to) {
      X[[to]] <<- Y[[from]]
      Y[[to]] <<- X[[from]] })


  #________________________________________________________
  # Add all combinations, creating a longer vector.
  #________________________________________________________
  cross <- function (...) {
    rowSums(expand.grid(...))
  }


  #________________________________________________________
  # Draw the pattern over the entire area.
  #________________________________________________________
  res <- switch(
    EXPR = pattern,

    brick = list(grid::segmentsGrob, list(
      x0 = with(X, c(rep(l, m), rep(o, m),    rep(e, m))),
      y0 = with(Y, c(p, rep(o, each = m),     rep(e, each = m))),
      x1 = with(X, c(rep(h, m), rep(o, m),    rep(e, m))),
      y1 = with(Y, c(p, rep(o, each = m) + s, rep(e, each = m) + s)) )),

    fish = list(grid::curveGrob, list(
      x1 = with(X, cross(c(0, s), c(rep(e, me),        rep(e, me)))),
      y1 = with(Y, cross(c(0, s), c(rep(e, each = me), rep(e, each = me)))),
      x2 = with(X, cross(c(0, s), c(rep(e, me) + s*2,   rep(e, me) + s*2))),
      y2 = with(Y, cross(c(0, s), c(rep(e, each = me), rep(e, each = me)))),
      ncp = 10,
      square = FALSE )),

    grid = list(grid::segmentsGrob, list(
      x0 = with(X, c(rep(l, m), p)),
      y0 = with(Y, c(p, rep(l, m))),
      x1 = with(X, c(rep(h, m), p)),
      y1 = with(Y, c(p, rep(h, m))) )),

    hexagon = list(grid::segmentsGrob, with(
      data = expand.grid(x = X$e, y = Y$e),
      expr = list(
        x0 = cross(x, X$s * 2 * c(0, 2/6, 3/6, 5/6, 1, 2/6)),
        y0 = cross(y, Y$s * 2 * c(0.5, 0.5, 1, 1, 0.5, 0.5)),
        x1 = cross(x, X$s * 2 * c(2/6, 3/6, 5/6, 1, 5/6, 3/6)),
        y1 = cross(y, Y$s * 2 * c(0.5, 1, 1, 0.5, 0, 0)) ))),

    octagon = list(grid::segmentsGrob, with(
      data = expand.grid(x = X$e, y = Y$e),
      expr = list(
        x0 = cross(x, X$s * 2 * c(0, 0.3, 0.6, 0.3, 0.3, 0.6)),
        y0 = cross(y, Y$s * 2 * c(0.3, 0.6, 0.3, 0, 0.6, 0.3)),
        x1 = cross(x, X$s * 2 * c(0.3, 0.6, 0.3, 0, 0.3, 1)),
        y1 = cross(y, Y$s * 2 * c(0.6, 0.3, 0, 0.3, 1, 0.3)) ))),

    rain = list(grid::segmentsGrob, list(
      x0 = with(X, c(rep(o, m),        rep(e, m))),
      y0 = with(Y, c(rep(o, each = m), rep(e, each = m))),
      x1 = with(X, c(rep(o, m),        rep(e, m))),
      y1 = with(Y, c(rep(o, each = m), rep(e, each = m)) + s) )),

    rshingle = list(grid::segmentsGrob, list(
      x0 = with(X, c(rep(l, m), rep(o, mo),            rep(e, me)) + s/5),
      y0 = with(Y, c(p,         rep(o, each = mo),     rep(e, each = me))),
      x1 = with(X, c(rep(h, m), rep(o, mo),            rep(e, me)) - s/5),
      y1 = with(Y, c(p,         rep(o, each = mo) + s, rep(e, each = me) + s)) )),

    saw = list(grid::polylineGrob, list(
      x  = with(X, rep(p, m)),
      y  = with(Y, rep(p, each = m) + rep(ifelse(X$m2, s/4, -s/4), n)),
      id.length = rep(X$n, each = Y$n) )),

    shingle = list(grid::segmentsGrob, list(
      x0 = with(X, c(rep(l, m), rep(o, mo),            rep(e, me)) - s/5),
      y0 = with(Y, c(p,         rep(o, each = mo),     rep(e, each = me))),
      x1 = with(X, c(rep(h, m), rep(o, mo),            rep(e, me)) + s/5),
      y1 = with(Y, c(p,         rep(o, each = mo) + s, rep(e, each = me) + s)) )),

    stripe = list(grid::segmentsGrob, list(
      x0 = with(X, p),
      y0 = with(Y, rep(l, m)),
      x1 = with(X, p),
      y1 = with(Y, rep(h, m)) )),

    wave = list(grid::curveGrob, list(
      x1 = with(X, c(rep(p, m),        rep(p, m))),
      y1 = with(Y, c(rep(p, each = m), rep(p, each = m))),
      x2 = with(X, c(rep(p, m) + s,    rep(p, m) + s)),
      y2 = with(Y, c(rep(p, each = m), rep(p, each = m))),
      ncp = 10,
      square = FALSE ))
  )


  #________________________________________________________
  # Render the patterned grob as the foreground.
  #________________________________________________________
  
  fun  <- res[[1]]
  args <- c(res[[2]], list(gp = gp, vp = vp, default.units = "mm"))
  fg   <- do.call(fun, args)
  
  
  grid::setChildren(gt, gList(bg, fg))
}




#' @noRd
#' @keywords internal
#' 
modify_size <- function (size, axis, str) {
  
  if (nchar(str) == 0) return (size)
  
  if (str %in% c('xs', 'sm', 'md', 'lg', 'xl')) {
    val <- c('xs' = 1/4, 'sm' = 1/2, 'md' = 1, 'lg' = 2, 'xl' = 4)[[str]]
    if (!is.na(val)) return (size * val)
    
  } else if (startsWith(str, "*")) {
    val <- as.numeric(substr(str, 2, nchar(str)))
    if (!is.na(val)) return (size * val)
    
  } else if (startsWith(str, "/")) {
    val <- as.numeric(substr(str, 2, nchar(str)))
    if (!is.na(val)) return (size / val)
    
  } else {
    
    val <- sub("[a-z]+$", "", str)
    u   <- sub(val, "", str, fixed = TRUE)
    val <- abs(as.numeric(val))
    
    if (nzchar(u) && !is.na(val)) {
      u <- tryCatch(
        error = function (e) stop("Invalid unit: '", u, "'\n", e), 
        expr  = match.arg(u, c(
          "npc", "mm", "points", "picas", "bigpts", "dida", 
          "cicero", "scaledpts", "lines", "char", "native", "snpc" )))
      val <- grid::convertUnit(
        x         = unit(val, units = u), 
        unitTo    = "mm",
        axisFrom  = axis, 
        typeFrom  = "dimension",
        valueOnly = TRUE )
    }
    
    if (!is.na(val)) return (val)
  }
  
  stop(
    "Unable to parse ", axis, " size suffix '", str, "'\n",
    "Expected a positive number or 'xs', 'sm', 'md', 'lg', 'xl'.")
}
