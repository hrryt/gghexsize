#' Scales for area or radius of bin tiles
#'
#' Replacements for [ggplot2::scale_size_area()] and
#' [ggplot2::scale_size_binned_area()] with convenient defaults for
#' [geom_hextile()].
#'
#' These are convenience functions with the following changed defaults:
#' * `max_size = 1` rather than `6`,
#' * `oob = scales::squish`.
#'
#' In practice, this makes it easy to set a hard upper limit on a scale,
#' above which sizes are clamped to 1.
#'
#' @param ... Arguments passed on to [ggplot2::continuous_scale()] or
#' [ggplot2::binned_scale()].
#' @inheritParams ggplot2::scale_size
#' @param oob One of:
#'   - Function that handles limits outside of the scale limits
#'   (out of bounds). Also accepts rlang [lambda][rlang::as_function()]
#'   function notation.
#'   - The default ([scales::squish()]) squishes out of bounds values into range.
#'   - [scales::censor()] for replacing out of bounds values with `NA`.
#'   - [scales::squish_infinite()] for squishing infinite values into range.
#'
#' @examples
#' library(ggplot2)
#'
#' d <- ggplot(diamonds, aes(carat, depth, z = price)) +
#'   geom_hextile()
#' d + scale_size_tile(limits = c(NA, 100))
#'
#' d + scale_size_binned_tile(transform = "log10")
#'
#' @export
scale_size_tile <- function(
    name = ggplot2::waiver(), ..., max_size = 1,
    aesthetics = "size", oob = scales::squish) {
  ggplot2::continuous_scale(
    aesthetics, name = name, palette = scales::abs_area(max_size),
    rescaler = scales::rescale_max, oob = oob, ...
  )
}

#' @rdname scale_size_tile
#' @export
scale_size_binned_tile <- function(
    name = ggplot2::waiver(), ..., max_size = 1,
    aesthetics = "size", oob = scales::squish) {
  ggplot2::binned_scale(
    aesthetics, name = name, palette = scales::abs_area(max_size),
    rescaler = scales::rescale_max, oob = oob, ...
  )
}
