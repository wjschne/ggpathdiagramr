# classes ----
ggplot_class <- new_S3_class("ggplot")
unit_class <- new_S3_class("unit")
xy <- new_class("xy", abstract = TRUE)

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


# unions ----
numeric_or_character <- new_union(class_numeric, class_character)
numeric_or_unit <- new_union(class_numeric, unit_class)



# internal states ----
the <- new.env(parent = emptyenv())
# the$point <- c("alpha", "color", "fill", "shape", "size", "stroke")
# the$line <- c("alpha", "color", "stroke", "lineend", "linejoin", "linetype", "linewidth")
# the$polygon <- c("alpha", "color", "fill", "linetype", "linewidth")
# the$text <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "size.unit", "vjust")
# the$richtext <- c("alpha", "angle", "color", "family", "fontface", "hjust", "size", "vjust", "nudge_x", "nudge_y", "label.color", "label.padding", "label.margin", "label.r", "label.size", "lineheight")
the$arrow_head <- arrowheadr::arrow_head_deltoid()


# helpers


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
