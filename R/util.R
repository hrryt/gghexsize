ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || ggplot2::is.waiver(df)
}

hex_binwidth <- function(bins = 30, scales) {
  c(diff(scales$x$dimension())/bins, diff(scales$y$dimension())/bins)
}

hexBinSummarise <- function(x, y, z, binwidth, fun = mean, fun.args = list(),
                             drop = TRUE) {
  if (length(binwidth) == 1) {
    binwidth <- rep(binwidth, 2)
  }
  xbnds <- hex_bounds(x, binwidth[1])
  xbins <- diff(xbnds)/binwidth[1]
  ybnds <- hex_bounds(y, binwidth[2])
  ybins <- diff(ybnds)/binwidth[2]
  hb <- hexbin::hexbin(x, xbnds = xbnds, xbins = xbins, y,
                       ybnds = ybnds, shape = ybins/xbins, IDs = TRUE)
  value <- rlang::inject(tapply(z, hb@cID, fun, !!!fun.args))
  out <- hexbin::hcell2xy(hb)
  out <- data_frame0(!!!out)
  out$value <- as.vector(value)
  out$width <- binwidth[1]
  out$height <- binwidth[2]
  if (drop)
    out <- stats::na.omit(out)
  out
}

data_frame0 <- function(...) {
  vctrs::data_frame(..., .name_repair = "minimal")
}

hex_bounds <- function(x, binwidth) {
  c(
    round_any(min(x), binwidth, floor) - 1e-06,
    round_any(max(x), binwidth, ceiling) + 1e-06
  )
}

round_any <- function(x, accuracy, f = round) {
  check_numeric(x)
  f(x/accuracy) * accuracy
}

check_numeric <- function(x, what = "a {.cls numeric} vector", ..., arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  check_object(x, is.numeric, what, ..., arg = arg, call = call)
}

check_object <- function(x, check_fun, what, ..., allow_na = FALSE, allow_null = FALSE,
                         arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && rlang::is_null(x)) {
      return(invisible(NULL))
    }
    if (allow_na && all(is.na(x))) {
      return(invisible(NULL))
    }
  }
  stop_input_type(x, as_cli(what), ..., allow_null = allow_null,
                  arg = arg, call = call)
}

as_cli <- function(..., env = rlang::caller_env()) {
  cli::cli_fmt(cli::cli_text(..., .envir = env))
}

stop_input_type <- function(x, what, ..., allow_na = FALSE, allow_null = FALSE,
                            show_value = TRUE, arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  cli <- rlang::env_get_list(nms = c("format_arg", "format_code"),
                      last = topenv(), default = function(x) sprintf("`%s`",
                                                                     x), inherit = TRUE)
  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  }
  else {
    format_arg <- cli$format_arg
  }
  message <- sprintf("%s must be %s, not %s.", format_arg(arg),
                     what, obj_type_friendly(x, value = show_value))
  rlang::abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)
  if (n < 2) {
    return(chr)
  }
  head <- chr[seq_len(n - 1)]
  last <- chr[n]
  head <- paste(head, collapse = sep)
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  }
  else {
    paste0(head, " ", final, " ", last)
  }
}

obj_type_friendly <- function(x, value = TRUE) {
  if (rlang::is_missing(x)) {
    return("absent")
  }
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    }
    else {
      type <- class(x)[[1L]]
    }
    return(sprintf("a <%s> object", type))
  }
  if (!rlang::is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }
  n_dim <- length(dim(x))
  if (!n_dim) {
    if (!rlang::is_list(x) && length(x) == 1) {
      if (rlang::is_na(x)) {
        return(switch(typeof(x), logical = "`NA`", integer = "an integer `NA`",
                      double = if (is.nan(x)) {
                        "`NaN`"
                      } else {
                        "a numeric `NA`"
                      }, complex = "a complex `NA`", character = "a character `NA`",
                      .rlang_stop_unexpected_typeof(x)))
      }
      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        }
        else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }
      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }
        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x))
            "the complex number"
          else "the number"
          return(paste(what, number))
        }
        return(switch(typeof(x), logical = if (x) "`TRUE`" else "`FALSE`",
                      character = {
                        what <- if (nzchar(x)) "the string" else "the empty string"
                        paste(what, str_encode(x, quote = "\""))
                      }, raw = paste("the raw value", as.character(x)),
                      .rlang_stop_unexpected_typeof(x)))
      }
      return(switch(typeof(x), logical = "a logical value",
                    integer = "an integer", double = if (is.infinite(x)) show_infinites(x) else "a number",
                    complex = "a complex number", character = if (nzchar(x)) "a string" else "\"\"",
                    raw = "a raw value", .rlang_stop_unexpected_typeof(x)))
    }
    if (length(x) == 0) {
      return(switch(typeof(x), logical = "an empty logical vector",
                    integer = "an empty integer vector", double = "an empty numeric vector",
                    complex = "an empty complex vector", character = "an empty character vector",
                    raw = "an empty raw vector", list = "an empty list",
                    .rlang_stop_unexpected_typeof(x)))
    }
  }
  vec_type_friendly(x)
}

.rlang_as_friendly_type <- function(type) {
  switch(type, list = "a list", `NULL` = "`NULL`", environment = "an environment",
         externalptr = "a pointer", weakref = "a weak reference",
         S4 = "an S4 object", name = , symbol = "a symbol", language = "a call",
         pairlist = "a pairlist node", expression = "an expression vector",
         char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}

.rlang_stop_unexpected_typeof <- function(x, call = rlang::caller_env()) {
  rlang::abort(sprintf("Unexpected type <%s>.", typeof(x)), call = call)
}

vec_type_friendly <- function(x, length = FALSE) {
  if (!rlang::is_vector(x)) {
    rlang::abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))
  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    }
    else {
      type
    }
  }
  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    }
    else if (is.data.frame(x)) {
      return("a data frame")
    }
    else if (n_dim == 2) {
      return("a list matrix")
    }
    else {
      return("a list array")
    }
  }
  type <- switch(type, logical = "a logical %s", integer = "an integer %s",
                 numeric = , double = "a double %s", complex = "a complex %s",
                 character = "a character %s", raw = "a raw %s", type = paste0("a ",
                                                                               type, " %s"))
  if (n_dim < 2) {
    kind <- "vector"
  }
  else if (n_dim == 2) {
    kind <- "matrix"
  }
  else {
    kind <- "array"
  }
  out <- sprintf(type, kind)
  if (n_dim >= 2) {
    out
  }
  else {
    add_length(out)
  }
}

