# as.geom ----
#' Convert shapes to ggplot2 geoms
#'
#' @param x a shape
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass arguments to ggplot2::geom_point
#' @rdname as.geom
#' @export
as.geom <- new_generic("as.geom", c("x"))
method(as.geom, point_or_point_list) <- function(x, ...) {
  the_style <- get_non_empty_props(style_point(...))
  d <- get_tibble_defaults(x)
  myaes <- aes_injection(c("x", "y"), colnames(d))
  rlang::inject(ggplot2::geom_point(
    mapping = myaes,
    data = d,
    inherit.aes = FALSE,
    !!!the_style
  ))
}

method(as.geom, line_or_line_list) <- function(x, ...) {
  the_style <- get_non_empty_props(x = style_line(...))
  d <- get_tibble_defaults(x)

  # vertical, horizontal, or sloped
  if (x@b == 0) {
    myaes <- aes_injection(bare_mapping = c("xintercept"),
                           identity_mapping = colnames(d),
                           omit = c("slope", "intercept"))
    g <- rlang::inject(
      ggplot2::geom_vline(data = d,
                          mapping = myaes,
                          !!!the_style))

  } else if (x@a == 0) {
    d <- dplyr::rename(d, yintercept = intercept)
    myaes <- aes_injection(bare_mapping = c("yintercept"),
                           identity_mapping = colnames(d),
                           omit = c("slope", "xintercept"))
    g <- rlang::inject(
      ggplot2::geom_hline(data = d,
                          mapping = myaes,
                          !!!the_style))
  } else {
    myaes <- aes_injection(bare_mapping = c("intercept", "slope"),
                           identity_mapping = colnames(d),
                           omit = c("xintercept"))

    g <- rlang::inject(
      ggplot2::geom_abline(data = d,
                           mapping = myaes,
                           !!!the_style))
  }
  g
}
method(as.geom, segment_or_segment_list) <- function(x, ...) {
  the_style <- get_non_empty_props(style_line(...))
  d <- get_tibble_defaults(x)
  myaes <- aes_injection(c("x", "y", "xend", "yend"), colnames(d))
  rlang::inject(ggplot2::geom_segment(
    mapping = myaes,
    data = d,
    inherit.aes = FALSE,
    !!!the_style
  ))

}

method(as.geom, arrow_segment_or_arrow_segment_list) <- function(x, ...) {
  the_style <- get_non_empty_props(style_arrow(...))
  d <- get_tibble_defaults(x)
  myaes <- aes_injection(c("x", "y", "xend", "yend"), colnames(d))
  rlang::inject(ggarrow::geom_arrow_segment(
    mapping = myaes,
    data = d,
    inherit.aes = FALSE,
    !!!the_style
  ))
}

method(as.geom, circle) <- function(x, ...) {
  the_style <- get_non_empty_props(x = style_polygon(fill = NA, color = "black") + x@style + style_polygon(...))
  rlang::inject(ggforce::geom_circle(
    data = tibble::tibble(x0 = x@center@x, y0 = x@center@y, r = x@radius),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    inherit.aes = FALSE,
    !!!the_style))
}

method(as.geom, ellipse) <- function(x, ...) {
  the_style <- get_non_empty_props(x = style_polygon(fill = NA, color = "black") + x@style + style_polygon(...))
  rlang::inject(ggplot2::geom_polygon(data = as.data.frame(x@xy), aes(x = x, y = y), !!!the_style))
}

method(as.geom, rectangle) <- function(x, ...) {

  the_style <- get_non_empty_props(x = style_polygon() + x@style + style_polygon(...))

  d <- data.frame(x = x@center@x, y = x@center@y, width = x@width, height = x@height)

  rlang::inject(ggplot2::geom_tile(data = d, aes(x = x, y = y, width = width, height = height), !!!the_style))
}

method(as.geom, arc) <- function(x, ...) {
  the_style <- get_non_empty_props(x = style_line() + x@style + style_line(...))

  rlang::inject(
    ggarrow::geom_arrow(
      data = as.data.frame(x@xy),
      aes(x = x, y = y), !!!the_style))
}

method(as.geom, label_or_label_list) <- function(
    x,
    ...) {


  d_style_defaults <- list(
    fill = "white",
    label.margin = list(ggplot2::margin(
      t = 4,
      r = 4,
      b = 4,
      l = 4,
      unit = "pt"
    )),
    label.r = ggplot2::unit(.15, "lines"),
    label.padding = list(ggplot2::margin(t = 1,
                                    r = 1,
                                    b = 1,
                                    l = 1,
                                    unit = "pt")),
    label.color = NA
  )

  d <- get_tibble_defaults(x)
  for (col_name in colnames(d)) {
    d_style_defaults[col_name] <- NULL
  }
  d_style <- style_label()

  props(d_style) <- d_style_defaults

  the_style <- get_non_empty_props(d_style + style_label(...))

if (!is.null(the_style$polar_just_angle)) {
  if (!is.null(the_style$polar_just_multiplier)) {
    the_style$polar_just_multiplier <- 1.2
  }
  the_style$vjust <- polar2just(
    the_style$polar_just_angle,
    the_style$polar_just_multiplier,
    axis = "v"
  )
  the_style$hjust <- polar2just(
    the_style$polar_just_angle,
    the_style$polar_just_multiplier,
    axis = "h"
  )
}
  the_style$polar_just_angle <- NULL
  the_style$polar_just_multiplier <- NULL

  if (length(the_style$size) > 0) the_style$size <- the_style$size / ggplot2::.pt


  if (length(the_style$label.margin) > 0) {
    the_style$label.margin <- the_style$label.margin[[1]]

  }
  if (length(the_style$label.padding) > 0) {
    the_style$label.padding <- the_style$label.padding[[1]]
  }





  bare_mapping <- intersect(c("x", "y", "label", "vjust", "hjust"), colnames(d))
  omit <- c("polar_just_angle",
            "polar_just_multiplier")
  identity_mapping <- setdiff(colnames(d), c(bare_mapping, omit))



  myaes <- aes_injection(bare_mapping = bare_mapping,
                         identity_mapping = identity_mapping,
                         omit = omit)

  rlang::inject(ggtext::geom_richtext(
    mapping = myaes,
    data = d,
    inherit.aes = FALSE,
    !!!the_style
  ))






  # rlang::inject(ggtext::geom_richtext(
  #   data = data.frame(x = x@p@x,
  #                     y = x@p@y,
  #                     label = x@label),
  #   aes(x = x, y = y, label = label),
  #   inherit.aes = FALSE,
  #   !!!the_style))
}


method(as.geom, shape_list) <- function(x, ...) {
  purrr::map(x,\(x) as.geom(x, ...))
}
