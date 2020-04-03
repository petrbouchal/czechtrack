library(sysfonts)
library(showtext)

sysfonts::font_add_google(family = "rbtc", name = "Roboto Condensed")
sysfonts::font_add_google(family = "rbt", name = "Roboto")
sysfonts::font_families()
showtext_auto()

theme_schola <- function(gridlines = c("y", "x", "both", "scatter"),
                         base_size = 11,
                         family = "rbtc", title_family = "rbt",
                         margin_side = 6,
                         margin_bottom = 6,
                         plot.title.position = "plot",
                         axis.title = ggplot2::element_blank(),
                         multiplot = FALSE,
                         ...) {
  tonecol <- "#e3f1ff"
  grd <- match.arg(gridlines)
  grid_col <- if(grd == "scatter" | multiplot) "white" else "grey92"
  bg_col <- if(grd == "scatter" | multiplot) tonecol else "white"
  element_gridline <- ggplot2::element_line(colour = grid_col, size = 0.3)
  thm <- ggplot2::theme_minimal(base_size = base_size, base_family = family) +
    ggplot2::theme(plot.title.position = plot.title.position,
                   plot.title = ggplot2::element_text(face = "bold",
                                                      size = base_size * 1.2,
                                                      family = title_family),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = if(grd != "y")
                     element_gridline else ggplot2::element_blank(),
                   panel.grid.major.y = if(grd != "x")
                     element_gridline else ggplot2::element_blank(),
                   # axis.line = ggplot2::element_line(),
                   panel.background = ggplot2::element_rect(fill = bg_col,
                                                            colour = NA),
                   axis.title = axis.title,
                   strip.text.x = ggplot2::element_text(hjust = 0),
                   plot.margin = ggplot2::unit(c(10, margin_side,
                                                 margin_bottom, margin_side),
                                               units = "pt"))
  if(multiplot) thm <- thm +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = tonecol,
                                                            colour = NA))
  
  thm <- thm +
    ggplot2::theme(...)
  
  return(thm)
}

hrbrthemes::update_geom_font_defaults(family = "rbtc")