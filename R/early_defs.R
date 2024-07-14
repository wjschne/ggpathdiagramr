# classes ----
class_ggplot <- new_S3_class("ggplot")
class_unit <- new_S3_class("unit")
class_margin <- new_S3_class("margin")
xy <- new_class("xy", abstract = TRUE)
shape_list <- new_class("shape_list", parent = class_list)

# generics ----
#' Addition
#'
#' @param e1 object
#' @param e2 object
#' @export
`+` <- new_generic("+", c("e1", "e2"))

method(`+`, list(class_any, class_any)) <- function(e1,e2) {
  .Primitive("+")(e1,e2)
}

method(`+`, list(class_character, class_character)) <- function(e1,e2) {
  paste0(e1,e2)
}
method(`+`, list(class_numeric, class_character)) <- function(e1,e2) {
  paste0(e1,e2)
}
method(`+`, list(class_character, class_numeric)) <- function(e1,e2) {
  paste0(e1,e2)
}

# generics ----
#' Get object data with styles in a tibble
#'
#' @param x object
#' @export
get_tibble <- new_generic("get_tibble", "x")
method(get_tibble, class_list) <- function(x) {
  purrr::map_df(S7_data(x), get_tibble)
}

#' Get points for making points
#'
#' @param x object
#' @keywords internal
get_points <- new_generic("get_points", "x")




#' Get object data in a tibble, filling in any missing styles with defaults
#'
#' @param x object
#' @export
#' @rdname get_tibble
get_tibble_defaults <- new_generic("get_tibble_defaults", "x")
method(get_tibble_defaults, class_any) <- function(x) {
  get_tibble(x)
  }

# unions ----
class_numeric_or_character <- new_union(class_numeric, class_character)
class_numeric_or_unit <- new_union(class_numeric, class_unit)
class_character_or_logical <- new_union(class_character, class_logical)


# internal states ----
the <- new.env(parent = emptyenv())
# the$point <- c("alpha", "color", "fill", "shape", "size", "stroke")
# the$line <- c("alpha", "color", "stroke", "lineend", "linejoin", "linetype", "linewidth")
# the$polygon <- c("alpha", "color", "fill", "linetype", "linewidth")
# the$text <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "size.unit", "vjust")
# the$richtext <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "vjust", "nudge_x", "nudge_y", "label.color", "label.padding", "label.margin", "label.r", "label.size", "lineheight")
the$arrow_head <- arrowheadr::arrow_head_deltoid()


# helpers ----

#' @keywords internal
allsameclass <- function(l, classname) {
  allsame <- all(sapply(lapply(l, class),
                        function(x)  classname %in% x))
  if (!allsame) {
    paste0("All items must be ", classname, ".")
  }
}

#' @keywords internal
aes_injection <- function(bare_mapping, identity_mapping, omit = NULL) {
  identity_mapping <- setdiff(identity_mapping, c(bare_mapping, omit))
  bare_mapping <- setdiff(bare_mapping, omit)
  i_styles <- purrr::map(
    rlang::syms(identity_mapping),
    \(i) call("I", i))
  names(i_styles) <- identity_mapping
  b_styles <- rlang::syms(bare_mapping)
  names(b_styles) <- bare_mapping

  rlang::inject(ggplot2::aes(!!!b_styles, !!!i_styles))

}



#' @keywords internal
get_tibble_defaults_helper <- function(x, default_style, required_aes = c("x", "y")) {
  d <- get_tibble(x)
  for (n in setdiff(colnames(d), required_aes)) {
    d_prop <- prop(default_style, n)

    d_prop <- ifelse(is.vector(d_prop), d_prop, c(d_prop))
    d[is.na(pull(d, n)), n] <- d_prop
  }
  d
}



#' @keywords internal
get_non_empty_props <- function(x) {
  Filter(function(s) length(s) > 0 , props(x))
}

#' @keywords internal
get_non_empty_list <- function(l) {
Filter(\(x) length(x) > 0, l)
}

#' @keywords internal
get_non_empty_tibble <- function(d) {
  d <- Filter(\(x) length(x) > 0, d)
  tibble::as_tibble(d)
  }

#' Probability rounding
#'
#' Rounds to significant digits, removing leading zeros.
#'
#' @param p probability
#' @param accuracy smallest increment
#' @param digits significant digits
#' @param max_digits maximum rounding digits
#' @param remove_leading_zero remove leading zero
#' @param round_zero_one round 0 and 1
#'
#' @return a character vector
#' @export
#' @examples
#' round_probability(c(0, .0012, .012, .12, .99, .992, .9997, 1), digits = 2)
round_probability <- function(p,
                       accuracy = 0.01,
                       digits = NULL,
                       max_digits = NULL,
                       remove_leading_zero = TRUE,
                       round_zero_one = TRUE) {
  if (is.null(digits)
  ) {
    l <- scales::number(p,
                        accuracy = accuracy)
  }
  else {
    sig_digits <- abs(ceiling(log10(p + p / 1e+09)) - digits)
    pgt99 <- p > 0.99
    sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - digits + 1)

    sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] <-
      sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= digits)] - 1

    sig_digits[is.infinite(sig_digits)] <- 0

    l <- purrr::map2_chr(p, sig_digits, formatC, format = "f", flag = "#")
  }
  if (remove_leading_zero)
    l <- sub("^-0", "-", sub("^0", "", l))
  if (round_zero_one) {
    l[p == 0] <- "0"
    l[p == 1] <- "1"
    l[p == -1] <- "-1"
  }
  if (!is.null(max_digits)) {
    if (round_zero_one) {
      l[round(p, digits = max_digits) == 0] <- "0"
      l[round(p, digits = max_digits) == 1] <- "1"
      l[round(p, digits = max_digits) == -1] <- "-1"
    }
    else {
      l[round(p, digits = max_digits) == 0] <- paste0(".", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == 1] <- paste0("1.", paste0(rep("0", max_digits), collapse = ""))
      l[round(p, digits = max_digits) == -1] <- paste0("-1.", paste0(rep("0", max_digits), collapse = ""))
    }
  }
  l <- sub(pattern = "-",
           replacement = "\u2212",
           x = l)
  Encoding(l) <- "UTF-8"
  dim(l) <- dim(p)
  l
}



