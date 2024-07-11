



# Point----

#' point class
#'
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @export
point <- S7::new_class(
  name = "point",
  properties = list(
    x = S7::new_property(class = S7::class_numeric, default = 0),
    y = S7::new_property(class = S7::class_numeric, default = 0),
    slope = S7::new_property(
      getter = function(self)
        self@y / self@x
    ),
    angle = S7::new_property(
      getter = function(self)
        atan2(self@y, self@x)
    ),
    distance = S7::new_property(
      getter = function(self) sqrt(self@x ^ 2 + self@y ^ 2)

    ),
    xy = S7::new_property(
      getter = function(self) matrix(c(self@x, self@y), ncol = 2, dimnames = list(NULL, c("x", "y")))
    )
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
                         distance = S7::class_missing) {
    if (length(x) == 2 && length(y) == 0) {
      y <- x[2]
      x <- x[1]
    }
    if (length(x) == 0 && length(y) == 0 && length(angle) == 1 && length(distance) == 1) {
     x = distance * cos(angle)
     y = distance * sin(angle)
    }

    manyxy <- length(x) > 1 && length(y) == length(x)
    manyx <- length(x) > 1 && length(y) == 1
    manyy <- length(y) > 1 && length(x) == 1

    if (any(manyxy, manyx, manyy)) {
      d <- cbind(x, y)
      return(apply(d, 1, \(r) {
        names(r) <- NULL
        point(x = r[1], y = r[2])
        }
                   ))
    }
    S7::new_object(S7::S7_object(), x = x, y = y)
  }
)

S7::method(`+`, list(point, point)) <- function(e1,e2) {
  point(e1@x + e2@x, e1@y + e2@y)}
S7::method(`-`, list(point, point)) <- function(e1,e2) {
  point(e1@x - e2@x, e1@y - e2@y)}
S7::method(`*`, list(point, point)) <- function(e1,e2) {
  point(e1@x * e2@x, e1@y * e2@y)}
S7::method(`/`, list(point, point)) <- function(e1,e2) {
  point(e1@x / e2@x, e1@y / e2@y)}
S7::method(`+`, list(S7::class_numeric, point)) <- function(e1,e2) {
  point(e1 + e2@x, e1 + e2@y)}
S7::method(`-`, list(S7::class_numeric, point)) <- function(e1,e2) {
  point(e1 - y@x, e1 - e2@y)}
S7::method(`*`, list(S7::class_numeric, point)) <- function(e1,e2) {
  point(e1 * e2@x, x * e2@y)}
S7::method(`/`, list(S7::class_numeric, point)) <- function(e1,e2) {
  point(e1 / e2@x, e1 / e2@y)}
S7::method(`+`, list(point, S7::class_numeric)) <- function(e1,e2) {
  point(e1@x + e2, e1@x + e2)}
S7::method(`-`, list(point, S7::class_numeric)) <- function(e1,e2) {
  point(e1@x - e2, e1@x - e2)}
S7::method(`*`, list(point, S7::class_numeric)) <- function(e1,e2) {
  point(e1@x * e2, e1@x * e2)}
S7::method(`/`, list(point, S7::class_numeric)) <- function(e1,e2) {
  point(e1@x / x, e1@x / e2)}
# Node----

#' node class
#'
#' @export
node <- S7::new_class(name = "node",
                      parent = point,
                      properties = list(label = S7::class_character),
                      constructor = function(x = S7::class_missing,
                                             y = S7::class_missing,
                                             label = S7::class_missing) {
                        if ("point" %in% class(x)) {
                          p = x
                          if (length(label) == 0  && length(y) == 1 && "character" %in% class(y)) {
                            label = y
                            }
                        } else {
                          p = point(x = x, y = y)
                        }
                        S7::new_object(p, label = label)
                      })

point_or_node <- S7::new_union(point, node)

S7::method(`-`, list(node, node)) <- function(e1,e2) {
  node(e1@x - e2@x, e1@y - e2@y, label = paste0(e1@label, " - ", e2@label))
}

S7::method(`+`, list(node, node)) <- function(e1,e2) {
  node(e1@x + e2@x, e1@y + e2@y, label = paste0(e1@label, " + ", e2@label))
}

# Line----

#' line class
#'
#' @export
line <- S7::new_class(
  "line",
  properties = list(
    # ax + by + c = 0
    a = S7::new_property(class = S7::class_numeric, default = 0),
    b = S7::new_property(class = S7::class_numeric, default = 0),
    c = S7::new_property(class = S7::class_numeric, default = 0),
    slope = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        -self@a / self@b
      }
    ),
    intercept = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        -self@c / self@b
      }
    ),
    x_intercept = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        -self@c / self@a
      }
    ),
    angle = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        atan(self@slope)
      }
    )
  ),
  constructor = function(slope = S7::class_missing,
                         intercept = S7::class_missing,
                         x_intercept = S7::class_missing,
                         a = S7::class_missing,
                         b = S7::class_missing,
                         c = S7::class_missing) {
    if (length(a) == 1 && length(b) == 1 && length(b) == 1) {
      if (a == 0 && b == 0 && c != 0)
        stop("If a and b are 0, c must be 0.")
      l <- S7::new_object(S7::S7_object(),
                          a = a,
                          b = b,
                          c = c)
      if (length(slope) == 1 && slope != l@slope) {
        stop("The slope is incompatible with parameters a, b, and c.")
      }
      if (length(intercept) == 1 && intercept != l@intercept) {
        stop("The intercept is incompatible with parameters a, b, and c.")
      }
      if (length(x_intercept) == 1 && x_intercept != l@x_intercept) {
        stop("The x_intercept is incompatible with parameters a, b, and c.")
      }
      } else if (length(slope) <= 1 &&
               length(intercept == 1)) {
      if (length(slope) == 0) {
        slope <- 0
        }
      a <- -slope
      b <- 1
      c <- -intercept
      l <- S7::new_object(S7::S7_object(),
                          a = a,
                          b = b,
                          c = c)

    } else if (length(x_intercept) == 1) {
      a <- 1
      b <- 0
      c <- -x_intercept
      l <- S7::new_object(S7::S7_object(),
                          a = a,
                          b = b,
                          c = c)
    } else {
      stop("There is insufficient information to create a line.")
    }
    l


  }
)



# Segment----
#' segment class
#'
#' @param p1 point 1
#' @param p2 point 2
#' @param line line on which the segment lies
#' @param xy a 2 by 2 matrix of the coordinates of p1 and p2
#'
#' @export
segment <- S7::new_class(
  "segment",
  properties = list(
    p1 = S7::new_property(class = point, default = point(x = 0, y = 0)),
    p2 = S7::new_property(class = point, default = point(x = 0, y = 0)),
    # slope = S7::new_property(
    #   getter = function(self) {
    #     slope(self@p1, self@p2)
    #   },
    #   setter = function(self, value) {
    #     self@p2 <- point(
    #       x = self@p1@x + self@distance * cos(atan(value)),
    #       y = self@p1@y + self@distance * sin(atan(value))
    #     )
    #     self
    #   }
    # ),
    # angle = S7::new_property(
    #   getter = function(self) {
    #     angle(self@p1, self@p2)
    #   },
    #   setter = function(self, value) {
    #     self@p2 <- point(
    #       x = self@p1@x + self@distance * cos(value),
    #       y = self@p1@y + self@distance * sin(value)
    #     )
    #     self
    #   }
    # ),
    # angle_degree = S7::new_property(
    #   getter = function(self)
    #     self@angle * 180 / pi
    # ),
    xy = S7::new_property(
      getter = function(self) {
        xy <- rbind(self@p1@xy, self@p2@xy)
        rownames(xy) <- c("p1", "p2")
        colnames(xy) <- c("x", "y")
        xy
      }
    ),
    line = S7::new_property(
      getter = function(self) {
      line(a = self@p1@y - self@p2@y,
           b = self@p2@x - self@p1@x,
           c = self@p1@x * self@p2@y - self@p2@x * self@p1@y)
      }
    )
  )
)
S7::method(`+`, list(segment, point)) <- function(e1,e2) {
  segment(e1@p1 + e2, e1@p2 + e2)
}

S7::method(`-`, list(segment, point)) <- function(e1,e2) {
  segment(e1@p1 - e2, e1@p2 - e2)
}

S7::method(`+`, list(point, segment)) <- function(e1,e2) {
  segment(e1 + e2@p1, e1 + e2@p2)
}

S7::method(`-`, list(point, segment)) <- function(e1,e2) {
  segment(e1 - e2@p1, e1 - e2@p2)
}



# Circle----

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param n number of points in circle
#' @param xy 2-column matrix of points
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' circle(p, radius = 6)
#'
#' # make a regular polygon
#' xy <- circle(p, n = 6)@xy
#' plot(xy, asp = 1, type = "n")
#' polygon(xy)
#'
#' @export
circle <- S7::new_class(
  name = "circle",
  properties = list(
    center = S7::new_property(class = point, default = point(x = 0, y = 0)),
    radius = S7::new_property(class = S7::class_numeric, default = 1L),
    n = S7::new_property(class = S7::class_numeric, default = 360L),
    xy = S7::new_property(
      getter = function(self) {
        theta <- seq(0, 2 * pi, length.out = self@n + 1)[-(self@n + 1)]
        x_p <- cos(theta) * self@radius + self@center@x
        y_p <- sin(theta) * self@radius + self@center@y
        cbind(x = x_p, y = y_p)
      }
    )
  )
)

S7::method(`+`, list(circle, point)) <- function(e1,e2) {
  circle(e1@center + e2, e1@radius)
}

S7::method(`-`, list(circle, point)) <- function(e1,e2) {
  circle(e1@center - e2, e1@radius)
}

S7::method(`+`, list(point, circle)) <- function(e1,e2) {
  circle(e2@center + e1, e2@radius)
}

S7::method(`-`, list(point, circle)) <- function(e1,e2) {
  circle(e1 - e2@center, e2@radius)
}


# Ellipse ----

#' an ellipse
#'
#' @export
ellipse <- S7::new_class(
  "ellipse",
  properties = list(
    center = S7::new_property(class = point, default = point(0, 0)),
    a = S7::new_property(S7::class_numeric, default = 1),
    b = S7::new_property(S7::class_numeric, default = 1),
    theta  = S7::new_property(S7::class_numeric, default = 0),
    n = S7::new_property(S7::class_numeric, default = 360),
    xy = S7::new_property(
      getter = function(self) {
        t <- seq(0, 2 * pi, length.out = self@n + 1)[-(self@n + 1)]
        xy <- cbind(x = self@a * cos(t),
                    y = self@b * sin(t))
       xy <- rotate2columnmatrix(xy, theta = self@theta)
       xy[,"x"] <- xy[,"x"] + self@center@x
       xy[,"y"] <- xy[,"y"] + self@center@y
       xy

      }
    )
  )
  # constructor = function (center = point(0, 0),
  #                         a = 1,
  #                         b = 1,
  #                         theta = 0,
  #                         n = 360L)
  #   new_object(
  #     NULL,
  #     center = center,
  #     a = a,
  #     b = b,
  #     theta = theta,
  #     n = n
  #   )
)
