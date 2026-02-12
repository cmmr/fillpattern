library(ggplot2)
library(fillpattern)

draw_hex_logo_ggplot <- function() {
  
  # 1. Define Hexagon Coordinates
  # Angles for pointy top: 90, 30, -30, -90, -150, 150
  angles <- c(-150, -90, -30, 30, 90, 150) * (pi / 180)
  hex_data <- data.frame(
    x = 0.5 + 0.5 * cos(angles),
    y = 0.5 + 0.5 * sin(angles)
  )
  
  # 2. Define Bar Data
  # Positions derived from the previous grid layout
  # Centers: 0.32, 0.5, 0.68. Width: 0.14. Base_y: 0.28.
  bar_data <- data.frame(
    id = factor(1:3),
    xmin = c(0.32, 0.50, 0.68) - 0.07,
    xmax = c(0.32, 0.50, 0.68) + 0.07,
    ymin = 0.35,
    ymax = 0.35 + c(0.25, 0.50, 0.35)
  )
  
  # 3. Construct Plot
  p <- ggplot() +
    
    # Hexagon Background (White fill, Black border)
    geom_polygon(
      data  = hex_data, 
      aes(x = x, y = y),
      fill  = "#F5F5F5",  # Light Gray
      color = "#4682B4",  # Steel Blue
      linewidth = 1.25
    ) +
    
    # Patterned Bars
    geom_rect(
      data = bar_data,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = id),
      color = "#333333",
      linewidth = 0.6
    ) +
    
    # Fill Pattern Scale
    scale_fill_pattern(
      patterns = c("stripe45", "brick", "hex"),
      fg       = c("firebrick", "steelblue", "forestgreen"),
      bg       = "#F5F5F5",
      width    = 1,
      lwd      = 1
    ) +
    
    # Thick Horizontal Line (Segment)
    annotate(
      "segment", 
      x = 0.2, xend = 0.8, 
      y = 0.35, yend = 0.35, 
      color = "#333333", 
      linewidth = 1.2
    ) +
    
    # Text Label
    annotate(
      "text", 
      x = 0.5, y = 0.25, 
      label    = "fillpattern", 
      fontface = "bold", 
      color    = "#333333", 
      size     = 5,  # Size in mm (approx)
      family   = "Segoe UI"
    ) +
    
    # Fix Aspect Ratio and Remove Axes
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position  = "none" # Hide the legend for the bars
    )
  
  return(p)
}

# Generate and print the plot
p <- draw_hex_logo_ggplot()
print(p)

# Save to file
ggsave("man/figures/logo.png", p, width = 200, height = 200, units = "px", bg = "transparent", dpi = 100)

