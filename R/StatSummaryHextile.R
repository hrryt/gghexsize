StatSummaryHextile <- ggplot2::ggproto(
  "StatSummaryHextile", ggplot2::StatSummaryHex,
  default_aes = ggplot2::aes(
    fill = after_stat(value),
    size = after_stat(count),
    weight = 1
  ),
  required_aes = c("x", "y"),
  dropped_aes = c("z", "z2", "z3", "weight"),
  compute_group = function(data, scales, binwidth = NULL, bins = 30,
                           drop = TRUE, fun = "mean", fun.args = list(),
                           fun2 = "mean", fun2.args = list(),
                           fun3 = "mean", fun3.args = list()) {
    rlang::check_installed("hexbin", reason = "for `stat_summary_hextile()`.")
    binwidth <- binwidth %||% hex_binwidth(bins, scales)

    wt <- data$weight %||% rep(1L, nrow(data))
    out <- hexBinSummarise(data$x, data$y, wt, binwidth, sum)
    out$density <- as.vector(out$value/sum(out$value, na.rm = TRUE))
    out$ndensity <- out$density/max(out$density, na.rm = TRUE)
    out$count <- out$value
    out$ncount <- out$count/max(out$count, na.rm = TRUE)

    if(!is.null(data$z)) {
      fun <- rlang::as_function(fun)
      out$value <- hexBinSummarise(
        data$x, data$y, data$z, binwidth, fun = fun, fun.args = fun.args,
        drop = drop
      )$value
    }
    if(!is.null(data$z2)) {
      fun2 <- rlang::as_function(fun2)
      out$value2 <- hexBinSummarise(
        data$x, data$y, data$z2, binwidth, fun = fun2, fun.args = fun2.args,
        drop = drop
      )$value
    }
    if(!is.null(data$z3)) {
      fun3 <- rlang::as_function(fun3)
      out$value3 <- hexBinSummarise(
        data$x, data$y, data$z3, binwidth, fun = fun3, fun.args = fun3.args,
        drop = drop
      )$value
    }

    out
  }
)
