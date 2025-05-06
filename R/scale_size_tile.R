#' @export
scale_size_tile <- function(
    name = ggplot2::waiver(), breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(), limits = NULL, range = c(0, 1),
    transform = "identity", trans = lifecycle::deprecated(),
    guide = "legend", aesthetics = "size", oob = scales::squish, ...) {
  palette <- if (!is.null(range)) scales::pal_area(range) else NULL
  ggplot2::continuous_scale(
    aesthetics, palette = palette, name = name,
    breaks = breaks, labels = labels, limits = limits, transform = transform,
    trans = trans, guide = guide, oob = oob, ...
  )
}

#' @export
scale_radius_tile <- function(
    name = ggplot2::waiver(), breaks = ggplot2::waiver(),
    labels = ggplot2::waiver(), limits = NULL, range = c(0, 1),
    transform = "identity", trans = lifecycle::deprecated(),
    guide = "legend", aesthetics = "size", oob = scales::squish, ...) {
  ggplot2::continuous_scale(
    aesthetics, palette = scales::pal_rescale(range), name = name,
    breaks = breaks, labels = labels, limits = limits, transform = transform,
    trans = trans, guide = guide, oob = oob, ...
  )
}
