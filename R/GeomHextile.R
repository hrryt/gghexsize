#' Hexagon key glyph for legends
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where `*`
#' stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [ggplot2::layer()] or examples below.)
#'
#' @inheritParams ggplot2::draw_key
#' @returns A grid grob.
#' @seealso [ggplot2::draw_key], [geom_hextile()].
#' @examples
#' library(ggplot2)
#'
#' d <- ggplot(diamonds, aes(carat, price, linewidth = after_stat(count))) +
#'   scale_linewidth(trans = "log10")
#' d + geom_hex(colour = "black")
#'
#' # key glyphs can be specified by their name
#' d + geom_hex(colour = "black", key_glyph = "hextile")
#'
#' # key glyphs can be specified via their drawing function
#' d + geom_hex(colour = "black", key_glyph = draw_key_hextile)
#'
#' @export
draw_key_hextile <- function(data, params, size) {
  one <- grid::unit(sqrt(3) / 2, "npc")
  fill <- ggplot2::fill_alpha(data$fill %||% "grey20", data$alpha)
  alpha <- fill_get_alpha(fill)
  fill0 <- ggplot2::fill_alpha(fill, 0.3 * alpha)
  width <- one * (data$size %||% 1) # - grid::unit(lwd, "mm")
  widths <- rep(grid::unit.c(one, width), each = 6)
  hexC <- hexbin::hexcoords(0.5)
  hexC$x <- hexC$x * widths + grid::unit(0.5, "npc")
  hexC$y <- hexC$y * widths + grid::unit(0.5, "npc")
  grob <- grid::polygonGrob(
    hexC$x, hexC$y,
    gp = grid::gpar(
      col = c(NA, data$colour %||% NA),
      fill = c(fill0, fill),
      lty = c(1, data$linetype %||% 1),
      lwd = c(NA, data$linewidth %||% 0) * ggplot2::.pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
    ), id.lengths = c(6, 6), vp = grid::vpTree(
      grid::viewport(layout = grid::grid.layout(respect = TRUE)),
      grid::vpList(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    )
  )
  # attr(grob, "width") <- lwd/5
  # attr(grob, "height") <- lwd/5
  grob
}

#' @name geom_hextile
#' @usage NULL
#' @format NULL
#' @export
GeomHextile <- ggplot2::ggproto(
  "GeomHextile", ggplot2::GeomHex,
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "grey50",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA,
    size = 1
  ),
  rename_size = FALSE,
  draw_key = draw_key_hextile,
  draw_group = function(data, panel_params, coord,
                        lineend = "butt", linejoin = "mitre", linemitre = 10) {
    if (empty(data)) {
      return(ggplot2::zeroGrob())
    }
    if(any(data$size > 1, na.rm = TRUE)) cli::cli_warn(
      "{.var size} aesthetic exceeds 1. Consider using {.fun scale_size_tile}."
    )
    if (!is.null(data$width)) {
      dx <- data$width[1]/2
    }
    else {
      dx <- ggplot2::resolution(data$x, FALSE, TRUE)
    }
    if (!is.null(data$height)) {
      dy <- data$height[1]/sqrt(3)/2
    }
    else {
      dy <- ggplot2::resolution(data$y, FALSE, TRUE)/sqrt(3)/2 * 1.15
    }
    hexC <- hexbin::hexcoords(dx, dy, n = 1)
    n <- nrow(data)
    hexdata <- data[rep(seq_len(n), each = 6), c("x", "y", "size")]
    hexdata$x <- rep.int(hexC$x, n) * hexdata$size + hexdata$x
    hexdata$y <- rep.int(hexC$y, n) * hexdata$size + hexdata$y
    hexdata$size <- NULL
    coords <- coord$transform(hexdata, panel_params)
    ggname("geom_hextile", grid::polygonGrob(coords$x, coords$y, gp = grid::gpar(
      col = data$colour, fill = ggplot2::fill_alpha(data$fill, data$alpha),
      lwd = data$linewidth * ggplot2::.pt, lty = data$linetype,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    ), default.units = "native", id.lengths = rep.int(6, n)))
  }
)
