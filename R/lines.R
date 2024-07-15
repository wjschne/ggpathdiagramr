# Line----

#' line class
#'
#' @param a coefficient in general form: a * x + b * y + c = 0
#' @param b coefficient in general form: a * x + b * y + c = 0
#' @param a constant in general form: a * x + b * y + c = 0
#' @param slope coefficient in y = slope * x + intercept
#' @param intercept value of y when x is 0
#' @param xintercept value of x when y is 0
#' @param style a style list
#' @param ... properties passed to style
#' @export
line <- new_class(
  "line",
  parent = shape,
  properties = list(
    # ax + by + c = 0
    a = new_property(class = class_numeric, default = 0),
    b = new_property(class = class_numeric, default = 0),
    c = new_property(class = class_numeric, default = 0),
    slope = new_property(
      class_numeric,
      getter = function(self) {
        -self@a / self@b
      }
    ),
    intercept = new_property(
      class_numeric,
      getter = function(self) {
        -self@c / self@b
      }
    ),
    xintercept = new_property(
      class_numeric,
      getter = function(self) {
        -self@c / self@a
      }
    ),
    angle = new_property(
      class_numeric,
      getter = function(self) {
        angle(radian = atan(self@slope))
      }
    ),
    style = style_line
  ),
  constructor = function(slope = class_missing,
                         intercept = class_missing,
                         xintercept = class_missing,
                         a = class_missing,
                         b = class_missing,
                         c = class_missing,
                         style = class_missing,
                         ...) {
    style <- style_line() + style + style_line(...)

    if (length(a) == 1 && length(b) == 1 && length(b) == 1) {
      if (a == 0 && b == 0 && c != 0) {
        stop("If a and b are 0, c must be 0.")
      }

      if (a <= 0 && b <= 0) {
        a <- a * -1
        b <- b * -1
        c <- c * -1
      }

      l <- new_object(S7_object(),
                          a = a,
                          b = b,
                          c = c,
                          style = style)
      if (length(slope) == 1 && slope != l@slope) {
        stop("The slope is incompatible with parameters a, b, and c.")
      }
      if (length(intercept) == 1 && intercept != l@intercept) {
        stop("The intercept is incompatible with parameters a, b, and c.")
      }
      if (length(xintercept) == 1 &&
          xintercept != l@xintercept) {
        stop("The xintercept is incompatible with parameters a, b, and c.")
      }
    } else if (length(slope) <= 1 &&
               length(intercept == 1) &&
               length(xintercept) == 0) {
      if (length(slope) == 0) {
        slope <- 0
      }

      if (is.infinite(slope) || is.infinite(intercept)) {
        stop("There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
      }

      a <- -slope
      b <- 1
      c <- -intercept
      l <- new_object(S7_object(),
                          a = a,
                          b = b,
                          c = c,
                          style = style)

    } else if (length(xintercept) == 1) {
      a <- 1
      b <- 0
      c <- -xintercept
      l <- new_object(S7_object(),
                          a = a,
                          b = b,
                          c = c,
                          style = style)
    } else {
      stop("There is insufficient information to create a line.")
    }
    l


  }
)


# Line list----
line_list <- new_class(
  name = "line_list",
  parent = shape_list,
  properties = list(
    # ax + by + c = 0
    a = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@a)),
    b = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@b)),
    c = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@c)),
    slope = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@slope)),
    intercept = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@intercept)),
    xintercept = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@xintercept)),
    angle = new_property(class = class_numeric, getter = function(self) sapply(self, \(x) x@angle))
  ),
  validator = function(self) {
    allsameclass(self, "line")
  }
)
line_or_line_list <- new_union(line, line_list)

method(get_tibble, line) <- function(x) {
  xs <- c(list(slope = x@slope,
               intercept = x@intercept,
               xintercept = x@xintercept),
          get_non_empty_props(x@style))

  rlang::inject(tibble::tibble(!!!xs))
}

method(get_tibble, line_list) <- function(x) {
  purrr::map_df(S7_data(x), get_tibble)
}


method(get_tibble_defaults, line) <- function(x) {
  get_tibble(x)
}

method(get_tibble_defaults, line_list) <- function(x) {
  sp <- style_line(
    alpha = 1,
    color = "black",
    linend = "black",
    linejoin = 16,
    linewidth = 1.5,
    linetype = 0.5
  )
  d <- get_tibble(x)
  for (n in setdiff(colnames(d), c("x", "y"))) {
    d[is.na(pull(d, n)), n] <- prop(sp, n)
  }
  d
}


# Segment----
#' segment class
#'
#' @param p1 point 1
#' @param p2 point 2
#' @param line line on which the segment lies
#' @param xy a 2 by 2 matrix of the coordinates of p1 and p2
#' @param style a style list
#' @param ... properties passed to style
#'
#' @export
segment <- new_class(
  name = "segment",
  parent = xy,
  properties = list(
    p1 = new_property(class = point, default = point(x = 0, y = 0)),
    p2 = new_property(class = point, default = point(x = 0, y = 0)),
    xy = new_property(
      getter = function(self) {
        xy <- rbind(self@p1@xy, self@p2@xy)
        rownames(xy) <- c("p1", "p2")
        colnames(xy) <- c("x", "y")
        xy
      }
    ),
    line = new_property(
      getter = function(self) {
        line(
          a = self@p1@y - self@p2@y,
          b = self@p2@x - self@p1@x,
          c = self@p1@x * self@p2@y - self@p2@x * self@p1@y
        )
      }
    ),
    style = style_or_style_line
  ),
  constructor = function(p1 = class_missing,
                         p2 = class_missing,
                         style = class_missing,
                         ...) {
    s_style <- style_line() + style + style_line(...)
    # many12 <- length(p1) > 1 && length(p2) == length(p1)
    # many1 <- length(p1) > 1 && length(p2) == 1
    # many2 <- length(p2) > 1 && length(p1) == 1
    d <- tibble::tibble(p1 = c(p1), p2 = c(p2), style = c(s_style))
    if (nrow(d) > 1) {
      return(segment_list(purrr::pmap(d, segment)))
    }

    # if (any(many12, many1, many2)) {
    #   m_p1 <- as.matrix(p1)
    #   m_p2 <- as.matrix(p2)
    #   d <- cbind(m_p1, m_p2)
    #   return(segment_list(apply(d, 1, \(r) {
    #     names(r) <- NULL
    #     segment(p1 = point(x = r[1], y = r[2]),
    #             p2 = point(x = r[3], y = r[4]))
    #   })))
    #
    # }
    new_object(S7_object(), p1 = p1, p2 = p2, style = s_style)
  }
)
method(`+`, list(segment, point)) <- function(e1, e2) {
  segment(e1@p1 + e2, e1@p2 + e2)
}

method(`-`, list(segment, point)) <- function(e1, e2) {
  segment(e1@p1 - e2, e1@p2 - e2)
}

method(`+`, list(point, segment)) <- function(e1, e2) {
  segment(e1 + e2@p1, e1 + e2@p2)
}

method(`-`, list(point, segment)) <- function(e1, e2) {
  segment(e1 - e2@p1, e1 - e2@p2)
}



# Segment list----
#' segment_list
#'
#' Make lists of segments
#' @param .data list of segments
#' @param p1 point_list of first points (p1)
#' @param p2 point_list of second points (p2)
#' @param line line_list of lines associated with segments
#' @param style style_list
#' @export
#' @examples
#' s1 <- segment(point(0,0), point(1,1))
#' s2 <- segment(point(1,1), point(0,1))
#' segment_list(c(s1,s2))
segment_list <- new_class(
  name = "segment_list",
  parent = shape_list,
  properties = list(
    p1 = new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p1))
      }
    ),
    p2 = new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p2))
      }
    ),
    line = new_property(
      line_list,
      getter = function(self) {
        line_list(lapply(self, \(x) x@line))
      }
    ),
    style = new_property(
      style_list,
      getter = function(self) {
        style_list(lapply(self, \(x) x@style))
      }
    )),
  validator = function(self) {
    allsameclass(self, "segment")
  }
)

segment_or_segment_list <- new_union(segment, segment_list)




method(get_tibble, segment) <- function(x) {
  xs <- c(list(x = x@p1@x,
               xend = x@p2@x,
               y = x@p1@y,
               yend = x@p2@y),
          get_non_empty_props(x@style))

  rlang::inject(tibble::tibble(!!!xs))
}


method(get_tibble_defaults, segment_list) <- function(x) {
  sp <- style_line(
    alpha = replace_na(as.double(ggplot2::GeomSegment$default_aes$alpha), 1),
    color = replace_na(ggplot2::GeomSegment$default_aes$colour, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggplot2::GeomSegment$default_aes$linewidth, 0.5),
    linetype = replace_na(ggplot2::GeomSegment$default_aes$linetype, 1)
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x", "y", "xend", "yend"))
}

# method(get_tibble_defaults, class_any) <- function(x) {
#   if (length(x) == 1) return(get_tibble(x))
#   style(
#     alpha = 1,
#     angle = 0,
#     arrow_head = the$arrow_head,
#     arrow_mid = ggarrow::arrow_fins_minimal(),
#     arrow_fins = ggarrow::arrow_fins_minimal(),
#     color = "black",
#     family = "sans",
#     fill = "black",
#     fontface = "plain",
#     hjust = .5,
#     justify = 0,
#     label.color = "black",
#     label.margin = ggplot2::margin(),
#     label.padding = ggplot2::margin(),
#     label.r = unit(0.15, "lines"),
#     label.size = .25,
#     length = 4,
#     length_head = NULL,
#     length_fins = NULL,
#     length_mid = NULL,
#     lineend = "butt",
#     lineheight = 1,
#     linejoin = "round",
#     linemitre = 10,
#     linewidth = .25,
#     linewidth_fins = .25,
#     linewidth_head = .25,
#     linetype = 1,
#     mid_place = 0.5,
#     resect = 0,
#     resect_head = NULL,
#     resect_fins = NULL
#
#   )
#
#   get_tibble_defaults_helper(x, sp)
# }
