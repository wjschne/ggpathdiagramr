# Point----

#' point class
#'
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @param slope Slope of the vector from the origin to the point
#' @param angle Angle of the vector from the origin to the point
#' @param distance Distance from the origin to the point
#' @param xy A 1 by 2 matrix of the x and y coordinates
#' @param style a style object
#' @param ... properties passed to style
#' @export
point <- S7::new_class(
  name = "point",
  parent = xy,
  properties = list(
    x = S7::new_property(class = S7::class_numeric, default = 0),
    y = S7::new_property(class = S7::class_numeric, default = 0),
    slope = S7::new_property(
      getter = function(self)
        self@y / self@x
    ),
    angle = S7::new_property(
      getter = function(self)
        angle(radian = atan2(self@y, self@x))
    ),
    distance = S7::new_property(
      getter = function(self)
        sqrt(self@x ^ 2 + self@y ^ 2)

    ),
    xy = S7::new_property(
      getter = function(self)
        matrix(
          c(self@x, self@y),
          ncol = 2,
          dimnames = list(NULL, c("x", "y"))
        )
    ),
    style = S7::new_property(class = style, default = style(size = 2, shape = 16))
  ),
  validator = function(self) {
    if (length(self@x) > 1) {
      stop("x must not have more than 1 element.")
    }
    if (length(self@y) > 1) {
      stop("y must not have more than 1 element.")
    }

  },
  constructor = function(x = S7::class_missing,
                         y = S7::class_missing,
                         angle = S7::class_missing,
                         distance = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    style <- style + style(...)

    if ("matrix" %in% class(x)) {
      if (ncol(x) == 2) {
        y <- x[,2, drop = TRUE]
        x <- x[,1, drop = TRUE]
      }
    }
    if (length(x) == 2 && length(y) == 0) {
      y <- x[2]
      x <- x[1]
    }
    if (length(x) == 0 &&
        length(y) == 0 && length(angle) > 0 && length(distance) > 0) {
      if (is.numeric(angle)) angle <- angle(degree = angle)
      x = distance * cos(angle)
      y = distance * sin(angle)
    }

    manyxy <- length(x) > 1 && length(y) == length(x)
    manyx <- length(x) > 1 && length(y) == 1
    manyy <- length(y) > 1 && length(x) == 1

    if (any(manyxy, manyx, manyy)) {
      d <- cbind(x, y)
      return(point_list(apply(d, 1, \(r) {
        names(r) <- NULL
        point(x = r[1], y = r[2], style = style)
      })))
    }


    S7::new_object(S7::S7_object(), x = x, y = y, style = style)
  }
)

S7::method(`+`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x + e2@x
  e1@y <- e1@y + e2@y
  e1
}
S7::method(`-`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x - e2@x
  e1@y <- e1@y - e2@y
  e1
}
S7::method(`*`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x * e2@x
  e1@y <- e1@y * e2@y
  e1
}
S7::method(`/`, list(point, point)) <- function(e1, e2) {
  e1@x <- e1@x / e2@x
  e1@y <- e1@y / e2@y
  e1
}
S7::method(`+`, list(S7::class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 + e2@x
  e2@y <- e1 + e2@y
  e2
}
S7::method(`-`, list(S7::class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 - e2@x
  e2@y <- e1 - e2@y
  e2
}
S7::method(`*`, list(S7::class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 * e2@x
  e2@y <- e1 * e2@y
  e2
}
S7::method(`/`, list(S7::class_numeric, point)) <- function(e1, e2) {
  e2@x <- e1 / e2@x
  e2@y <- e1 / e2@y
  e2
}
S7::method(`+`, list(point, S7::class_numeric)) <- function(e1, e2) {
  e2 + e1
}
S7::method(`-`, list(point, S7::class_numeric)) <- function(e1, e2) {
  e1@x <- e1@x - e2
  e1@y <- e1@y - e2
  e1
}
S7::method(`*`, list(point, S7::class_numeric)) <- function(e1, e2) {
  e2 * e1
}
S7::method(`/`, list(point, S7::class_numeric)) <- function(e1, e2) {
  e1@x <- e1@x / e2
  e1@y <- e1@y / e2
  e1
}

S7::method(`%*%`, list(point, point)) <- function(x, y) {
  x@xy[1, , drop = TRUE] %*% y@xy[1, , drop = TRUE]
}

S7::method(`==`, list(point, point)) <- function(e1, e2) {
  e1@x == e2@x && e1@y == e2@y
}

S7::method(`+`, list(ggplot_class, point)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

# Point list----
#' point_list class
#'
#' @export
point_list <- S7::new_class(
  name = "point_list",
  parent = S7::class_list,
  properties = list(
    x = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@x)
      }
    ),
    y = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@y)
      }
    ),
    xy = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        as.matrix(self)
      }
    ),
    slope = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@slope)
      }
    ),
    angle = S7::new_property(
      angle_list,
      getter = function(self) {
        angle_list(lapply(self, \(x) x@angle))
      }
    ),
    distance = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@distance)
      }
    ),
    style = S7::new_property(
      S7::class_list,
      getter = function(self) {
        lapply(self, \(x) x@style)
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "point")
  }
)

S7::method(`+`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x + e1))
}
S7::method(`+`, list(point_list, point)) <- function(e1, e2) {
  e2 + e1
}

S7::method(`-`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x - e1))
}
S7::method(`-`, list(point_list, point)) <- function(e1, e2) {
  e2 - e1
}

S7::method(`+`, list(ggplot_class, point_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

point_or_point_list <- S7::new_union(point, point_list)

radial_point <- S7::new_class(
  name = "radial_point",
  parent = point,
  constructor = function(angle = S7::class_missing,
                         distance = S7::class_missing) {
    if (length(distance) == 0) distance <- 0
    if (length(angle) == 0) angle <- angle(0)
    p <- point(angle = angle, distance = distance)
    S7::new_object(p)
  })

