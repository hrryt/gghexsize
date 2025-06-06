fill_get_alpha <- function(fill) {
  if (!is.list(fill)) {
    return(get_alpha(fill))
  }
  if (is_pattern(fill) || any(vapply(fill, is_pattern, logical(1)))) {
    return(pattern_get_alpha(fill))
  }
  msg <- paste0("{.field fill} must be a vector of colours or list of ",
                "{.cls GridPattern} objects.")
  fill <- rlang::try_fetch(
    lapply(fill, get_alpha),
    error = function(cnd) {
      cli::cli_abort(msg)
    }
  )
  if (!all(lengths(fill) == 1)) {
    cli::cli_abort(msg)
  }
  unlist(fill)
}

is_pattern <- function(x) {
  inherits(x, "GridPattern")
}

get_alpha <- function(colour) {
  alpha <- farver::decode_colour(colour, alpha = TRUE)[, "alpha"]
  names(alpha) <- NULL
  alpha
}

pattern_get_alpha <- function(x) {
  UseMethod("pattern_get_alpha")
}

#' @export
pattern_get_alpha.default <- function(x) {
  if (!is.atomic(x)) {
    cli::cli_abort("Can't apply {.arg alpha} to {obj_type_friendly(x)}.")
  }
  get_alpha(x)
}

#' @export
pattern_get_alpha.list <- function(x) {
  msg <- paste0("{.field fill} must be a vector of colours or list of ",
                "{.cls GridPattern} objects.")
  fill <- rlang::try_fetch(
    lapply(x, pattern_get_alpha),
    error = function(cnd) {
      cli::cli_abort(msg)
    }
  )
  if (!all(lengths(fill) == 1)) {
    cli::cli_abort(msg)
  }
  unlist(fill)
}

#' @export
pattern_get_alpha.GridPattern <- function(x) {
  get_alpha(x$colours[1])
}

#' @export
pattern_get_alpha.GridTilingPattern <- function(x) {
  1
}
