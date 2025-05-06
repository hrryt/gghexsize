#' Geom Hextile
#' @export
geom_hextile <- function(mapping = NULL, data = NULL, stat = "summary_hextile",
                         position = "identity", ..., na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHextile,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...))
}

#' @export
stat_summary_hextile <- function(mapping = NULL, data = NULL, geom = "hexagon",
                                 position = "identity", ..., bins = 30,
                                 binwidth = NULL, drop = TRUE,
                                 fun = "mean", fun.args = list(),
                                 fun2 = "mean", fun2.args = list(),
                                 fun3 = "mean", fun3.args = list(),
                                 na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatSummaryHextile,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = rlang::list2(
      bins = bins, binwidth = binwidth, drop = drop, fun = fun,
      fun.args = fun.args, na.rm = na.rm, ...
    )
  )
}
