# Point----

#' point class
#'
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @param r Radius = Distance from the origin to the point
#' @param theta Angle of the vector from the origin to the point
#' @param style a style object
#' @param ... properties passed to style
#' @export
point <- new_class(
  name = "point",
  parent = xy,
  properties = list(
    x = new_property(class = class_numeric, default = 0),
    y = new_property(class = class_numeric, default = 0),
    theta = new_property(
      getter = function(self)
        angle(radian = atan2(self@y, self@x))
    ),
    r = new_property(
      getter = function(self)
        sqrt(self@x ^ 2 + self@y ^ 2)

    ),
    xy = new_property(
      getter = function(self)
        matrix(
          c(self@x, self@y),
          ncol = 2,
          dimnames = list(NULL, c("x", "y"))
        )
    ),
    style = new_property(class = style_or_style_point, default = style_point(shape = 16))
  ),
  validator = function(self) {
    if (length(self@x) > 1) {
      stop("x must not have more than 1 element.")
    }
    if (length(self@y) > 1) {
      stop("y must not have more than 1 element.")
    }

  },
  constructor = function(x = class_missing,
                         y = class_missing,
                         theta = class_missing,
                         r = class_missing,
                         style = class_missing,
                         ...) {

    # We want to be able to pass arguments from a generic style class,
    # but we want to end up with a style_point object. So we start with an
    # empty style_point(), pass any filled in arguments from the style slot
    # (which can either be a general style or a style_point class), and then
    # pass any specifications from ...
    p_style <- style_point() + style + style_point(...)

    # slot x might be a 2-column matrix
    if ("matrix" %in% class(x)) {
      if (ncol(x) == 2) {
        y <- x[,2, drop = TRUE]
        x <- x[,1, drop = TRUE]
      } else {
        stop(paste("A point list can be created with a 2-column matrix, but this matrix has", ncol(x), "columns."))
      }
    }

    if (length(x) == 0 &&
        length(y) == 0 &&
        length(theta) > 0 &&
        length(r) > 0) {
      if (is.numeric(theta)) theta <- angle(radian = theta)
      x = r * cos(theta)
      y = r * sin(theta)
    }
    if (length(x) == 0) {
      x <- 0L
    }

    if (length(y) == 0) {
      y <- 0L
    }

    d <- tibble::tibble(x = x, y = y, style = c(p_style))
    if (nrow(d) > 1) {
      return(point_list(purrr::pmap(d, point)))
    }

    # if (any(manyxy, manyx, manyy)) {
    #   d <- cbind(x, y)
    #   return(point_list(apply(d, 1, \(r) {
    #     names(r) <- NULL
    #     point(x = r[1], y = r[2], style = p_style)
    #   })))
    # }


    new_object(S7_object(), x = x, y = y, style = p_style)
  }
)

method(`+`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x + e2@x
  e1@y <- e1@y + e2@y
  e1
}
method(`-`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x - e2@x
  e1@y <- e1@y - e2@y
  e1
}
method(`*`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x * e2@x
  e1@y <- e1@y * e2@y
  e1
}
method(`/`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x / e2@x
  e1@y <- e1@y / e2@y
  e1
}
method(`+`, list(class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 + e2@x
  e2@y <- e1 + e2@y
  e2
}
method(`-`, list(class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 - e2@x
  e2@y <- e1 - e2@y
  e2
}
method(`*`, list(class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 * e2@x
  e2@y <- e1 * e2@y
  e2
}
method(`/`, list(class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 / e2@x
  e2@y <- e1 / e2@y
  e2
}
method(`+`, list(point, class_numeric)) <- function(e1, e2) {
  e2 + e1
}
method(`-`, list(point, class_numeric)) <- function(e1, e2) {
  e1@x <- e1@x - e2
  e1@y <- e1@y - e2
  e1
}
method(`*`, list(point, class_numeric)) <- function(e1, e2) {
  e2 * e1
}
method(`/`, list(point, class_numeric)) <- function(e1, e2) {
  e1@x <- e1@x / e2
  e1@y <- e1@y / e2
  e1
}

method(`%*%`, list(point, point)) <- function(x, y) {
  x@xy[1, , drop = TRUE] %*% y@xy[1, , drop = TRUE]
}

method(`==`, list(point, point)) <- function(e1, e2) {
  e1@x == e2@x && e1@y == e2@y
}

method(`+`, list(ggplot_class, point)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

# Polar class ----
#' @rdname point
#' @export
polar <- new_class(
  name = "polar",
  parent = point,
  constructor = function(theta = class_missing,
                         r = class_missing,
                         style = class_missing,
                         ...) {
    p_style <- style_point() + style + style_point(...)
    if (length(r) == 0) r <- 0
    if (length(theta) == 0) theta <- angle(0)
    p <- point(theta = theta, r = r, style = p_style)
    if (length(p) > 1) {
      return(p)
    }
    new_object(p)
  })

# Point list----
#' point_list class
#'
#' @export
#' @rdname point
point_list <- new_class(
  name = "point_list",
  parent = class_list,
  properties = list(
    x = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@x)
      }
    ),
    y = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@y)
      }
    ),
    xy = new_property(
      class_numeric,
      getter = function(self) {
        as.matrix(self)
      }
    ),
    theta = new_property(
      angle_list,
      getter = function(self) {
        angle_list(lapply(self, \(x) x@theta))
      }
    ),
    r = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@r)
      }
    ),
    style = new_property(
      class_list,
      getter = function(self) {
        lapply(self, \(x) x@style)
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "point")
  }
)

method(`+`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x + e1))
}
method(`+`, list(point_list, point)) <- function(e1, e2) {
  e2 + e1
}

method(`-`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x - e1))
}
method(`-`, list(point_list, point)) <- function(e1, e2) {
  e2 - e1
}

method(`+`, list(ggplot_class, point_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

point_or_point_list <- new_union(point, point_list)



