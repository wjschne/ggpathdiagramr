# ggplot class----
ggplot_class <- S7::new_S3_class("ggplot")


numeric_or_character <- S7::new_union(S7::class_numeric, S7::class_character)
# style----
#' style class
#'
#' @param alpha numeric value for alpha transparency
#' @param angle angle of text
#' @param color character string for color
#' @param family font family
#' @param fill character string for fill color
#' @param hjust horizontal justification
#' @param lineheight height of line of text
#' @param linewidth width of lines
#' @param linetype type of lines
#' @param pch number for point character
#' @param shape type of shape
#' @param size numeric size
#' @param vjust vertical justification
#' @export
style <- S7::new_class("style", properties = list(
  alpha = S7::class_numeric,
  angle = S7::class_numeric,
  color = S7::class_character,
  family = S7::class_character,
  fill = S7::class_character,
  hjust = S7::class_numeric,
  lineheight = S7::class_numeric,
  linewidth = S7::class_numeric,
  linetype = numeric_or_character,
  pch = S7::class_numeric,
  shape = S7::class_numeric,
  size = S7::class_numeric,
  vjust = S7::class_numeric
  )
)


# xy ----
xy <- S7::new_class("xy", abstract = TRUE)

# Angle ----
#' angle class
#'
#' @param turn proportion of full turns of a circle (1 turn = 2 * pi radians)
#' @param radian radians
#' @param degree degrees
#' @param gradian gradians, gons, or grads (right angle = 100 gradians)
#' @export
#' @examples
#' # Four Different ways to make a right angle
#' ## A quarter turn
#' angle(.25)
#'
#' ## half pi radians
#' angle(radian = .5 * pi)
#'
#' ## 90 degrees
#' angle(degree = 90)
#'
#' ## 100 gradians
#' angle(gradian = 100)
#'
#' # Operations
#' angle(degree = 30) + angle(degree = 20)
#' angle(degree = 350) + angle(degree = 20)
#' angle(degree = 30) - angle(degree = 20)
#' angle(degree = 30) - angle(degree = 50)
#' 2 * angle(degree = 30)
#' angle(degree = 30) / 3
angle <- S7::new_class(
  name = "angle",
  properties = list(
    turn = S7::new_property(
      name = "turn",
      class = S7::class_numeric,
      default = 0
    ),
    radian = S7::new_property(
      getter = function(self)
        self@turn * 2 * pi
    ),
    degree = S7::new_property(
      getter = function(self)
        self@turn * 360
    ),
    gradian = S7::new_property(
      getter = function(self)
        self@turn * 400
    )
  ),
  constructor = function(turn = S7::class_missing,
                         radian = S7::class_missing,
                         degree = S7::class_missing,
                         gradian = S7:class_missing) {
    if (length(turn) > 0) {
      x <- turn
    } else if (length(radian) > 0) {
      x <- radian / (2 * pi)
    } else if (length(degree) > 0) {
      x <- degree / 360
    } else if (length(gradian) > 0) {
      x <- gradian / 400
    } else {
      x <- list()
    }
    if (length(x) > 0) {
      # x_neg <- x < 0
      x <- x %% 1
      # x[x_neg] <- x[x_neg] - 1
    }

    if (length(x) > 1) {
      return(angle_list(lapply(x, angle)))
    }

    S7::new_object(S7::S7_object(), turn = x)


  }
)




S7::method(cos, angle) <- function(x) {
  cospi(x@turn * 2)
}
S7::method(sin, angle) <- function(x) {
  sinpi(x@turn * 2)
}
S7::method(tan, angle) <- function(x) {
  tanpi(x@turn * 2)
}

S7::method(`+`, list(angle, angle)) <- function(e1, e2) {
  angle(e1@turn + e2@turn)
}
S7::method(`-`, list(angle, angle)) <- function(e1, e2) {
  angle(e1@turn - e2@turn)
}
S7::method(`*`, list(angle, angle)) <- function(e1, e2) {
  angle(e1@turn * e2@turn)
}
S7::method(`/`, list(angle, angle)) <- function(e1, e2) {
  angle(e1@turn / e2@turn)
}

S7::method(`+`, list(angle, S7::class_numeric)) <- function(e1, e2) {
  e1 + angle(radian = e2)
}
S7::method(`+`, list(S7::class_numeric, angle)) <- function(e1, e2) {
  angle(radian = e1) + e2
}

S7::method(`-`, list(angle, S7::class_numeric)) <- function(e1, e2) {
  e1 - angle(radian = e2)
}
S7::method(`-`, list(S7::class_numeric, angle)) <- function(e1, e2) {
  angle(radian = e1) - e2
}

S7::method(`*`, list(angle, S7::class_numeric)) <- function(e1, e2) {
  angle(e1@turn * e2)
}

S7::method(`*`, list(S7::class_numeric, angle)) <- function(e1, e2) {
  angle(e1 * e2@turn)
}

S7::method(`/`, list(angle, S7::class_numeric)) <- function(e1, e2) {
  angle(e1@turn / e2)
}

S7::method(`==`, list(angle, angle)) <- function(e1, e2) {
  e1@turn == e2@turn
}

class_angle_or_numeric <- S7::new_union(angle, S7::class_numeric)

# Angle list----
#' angle list class
#'
#' @rdname angle
#' @export
angle_list <- S7::new_class(
  name = "angle_list",
  parent = S7::class_list,
  properties = list(
    turn = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@turn)
      }
    ),
    radian = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@radian)
      }
    ),
    degree = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@degree)
      }
    ),
    gradian = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@gradian)
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "angle")
  }
)


# Point----

#' point class
#'
#' @param x Coordinate on the x-axis
#' @param y Coordinate on the y-axis
#' @param slope Slope of the vector from the origin to the point
#' @param angle Angle of the vector from the origin to the point
#' @param distance Distance from the origin to the point
#' @param xy A 1 by 2 matrix of the x and y coordinates
#' @param style a style list
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
    style = S7::new_property(class = style, default = style(size = 2, pch = 16))
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
    if (length(style) == 0) {
      style <- style(...)
    }

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
        length(y) == 0 && length(angle) == 1 && length(distance) == 1) {
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

point_or_point_list <- S7::new_union(point, point_list)

# label----

#' label class
#'
#' @param p point
#' @param label text label
#' @param style a style list
#' @param ... properties passed to style
#' @export
label <- S7::new_class(
  name = "label",
  parent = xy,
  properties = list(
    p = S7::new_property(point, default = point(0, 0, style = style(size = 1))),
    label = S7::new_property(S7::class_character, default = ""),
    xy = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        self@p@xy
      }
    ),
    style = S7::new_property(style)
  ),
  constructor = function(p = S7::class_missing,
                         label = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    if (length(style) == 0) {
      style <- style(...)
    }
    if (length(label) == 0) {
      label = paste0("(", round(p@x, 2), ",", round(p@y, 2), ")")
    } else if (is.numeric(label)) {
      label <- formatC(label, format = "fg", digits = 2)
    }
    S7::new_object(S7::S7_object(), p = p, label = label, style  = style)
  }
)

point_or_label <- S7::new_union(point, label)

S7::method(`-`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p - e2@p, label = paste0(e1@label, " - ", e2@label))
}

S7::method(`+`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p + e2@p, label = paste0(e1@label, " + ", e2@label))
}
S7::method(`+`, list(ggplot_class, label)) <- function(e1, e2) {
  e1 + as.geom(e2)
}


# Centerpoint ----
centerpoint <- S7::new_class("centerpoint", properties = list(center = S7::new_property(class = point, default = point(0,0))), parent = xy)

S7::method(`+`, list(centerpoint, point)) <- function(e1, e2) {
  circle(e1@center + e2, e1@radius)
}

S7::method(`-`, list(centerpoint, point)) <- function(e1, e2) {
  circle(e1@center - e2, e1@radius)
}

S7::method(`+`, list(point, centerpoint)) <- function(e1, e2) {
  circle(e2@center + e1, e2@radius)
}

S7::method(`-`, list(point, centerpoint)) <- function(e1, e2) {
  circle(e1 - e2@center, e2@radius)
}

# Line----

#' line class
#'
#' @param a coefficient in general form: a * x + b * y + c = 0
#' @param b coefficient in general form: a * x + b * y + c = 0
#' @param a constant in general form: a * x + b * y + c = 0
#' @param slope coefficient in y = slope * x + intercept
#' @param intercept value of y when x is 0
#' @param x_intercept value of x when y is 0
#' @param style a style list
#' @param ... properties passed to style
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
        angle(radian = atan(self@slope))
      }
    ),
    style = style
  ),
  constructor = function(slope = S7::class_missing,
                         intercept = S7::class_missing,
                         x_intercept = S7::class_missing,
                         a = S7::class_missing,
                         b = S7::class_missing,
                         c = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    if (length(style) == 0) {
      style <- style(...)
    }

    if (length(a) == 1 && length(b) == 1 && length(b) == 1) {
      if (a == 0 && b == 0 && c != 0) {
        stop("If a and b are 0, c must be 0.")
      }

      if (a <= 0 && b <= 0) {
        a <- a * -1
        b <- b * -1
        c <- c * -1
      }

      l <- S7::new_object(S7::S7_object(),
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
      if (length(x_intercept) == 1 &&
          x_intercept != l@x_intercept) {
        stop("The x_intercept is incompatible with parameters a, b, and c.")
      }
    } else if (length(slope) <= 1 &&
               length(intercept == 1) &&
               length(x_intercept) == 0) {
      if (length(slope) == 0) {
        slope <- 0
      }

      if (is.infinite(slope) || is.infinite(intercept)) {
        stop("There is not enough information to make a line. Specify the x-intercept or the a,b,c parameters.")
      }

      a <- -slope
      b <- 1
      c <- -intercept
      l <- S7::new_object(S7::S7_object(),
                          a = a,
                          b = b,
                          c = c,
                          style = style)

    } else if (length(x_intercept) == 1) {
      a <- 1
      b <- 0
      c <- -x_intercept
      l <- S7::new_object(S7::S7_object(),
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

S7::method(`+`, list(ggplot_class, line)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

# Line list----
line_list <- S7::new_class(
  name = "line_list",
  parent = S7::class_list,
  properties = list(
    # ax + by + c = 0
    a = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@a)),
    b = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@b)),
    c = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@c)),
    slope = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@slope)),
    intercept = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@intercept)),
    x_intercept = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@x_intercept)),
    angle = S7::new_property(class = S7::class_numeric, getter = function(self) sapply(self, \(x) x@angle))
  ),
  validator = function(self) {
    allsameclass(self, "segment")
  }
)


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
segment <- S7::new_class(
  "segment",
  properties = list(
    p1 = S7::new_property(class = point, default = point(x = 0, y = 0)),
    p2 = S7::new_property(class = point, default = point(x = 0, y = 0)),
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
        line(
          a = self@p1@y - self@p2@y,
          b = self@p2@x - self@p1@x,
          c = self@p1@x * self@p2@y - self@p2@x * self@p1@y
        )
      }
    ),
    style = style
  ),
  constructor = function(p1 = S7::class_missing,
                           p2 = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    if (length(style) == 0) {
      style <- style(...)
    }
    many12 <- length(p1) > 1 && length(p2) == length(p1)
    many1 <- length(p1) > 1 && length(p2) == 1
    many2 <- length(p2) > 1 && length(p1) == 1

    if (any(many12, many1, many2)) {
      m_p1 <- as.matrix(p1)
      m_p2 <- as.matrix(p2)
      d <- cbind(m_p1, m_p2)
      return(segment_list(apply(d, 1, \(r) {
        names(r) <- NULL
        segment(p1 = point(x = r[1], y = r[2]),
                p2 = point(x = r[3], y = r[4]))
      })))

    }
    S7::new_object(S7::S7_object(), p1 = p1, p2 = p2, style = style)
  }
)
S7::method(`+`, list(segment, point)) <- function(e1, e2) {
  segment(e1@p1 + e2, e1@p2 + e2)
}

S7::method(`-`, list(segment, point)) <- function(e1, e2) {
  segment(e1@p1 - e2, e1@p2 - e2)
}

S7::method(`+`, list(point, segment)) <- function(e1, e2) {
  segment(e1 + e2@p1, e1 + e2@p2)
}

S7::method(`-`, list(point, segment)) <- function(e1, e2) {
  segment(e1 - e2@p1, e1 - e2@p2)
}

S7::method(`+`, list(ggplot_class, segment)) <- function(e1, e2) {
  e1 + as.geom(e2)
  }

# Segment list----
segment_list <- S7::new_class(
  name = "segment_list",
  parent = S7::class_list,
  properties = list(
    p1 = S7::new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p1))
        }
      ),
    p2 = S7::new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p2))
      }
    ),
    line = S7::new_property(
      line_list,
      getter = function(self) {
        line_list(lapply(self, \(x) x@line))
      }
    )),
  validator = function(self) {
    allsameclass(self, "segment")
  }
)




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
  parent = centerpoint,
  properties = list(
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




# Ellipse ----

#' an ellipse
#'
#' @param center point at center of ellipse
#' @param a distance of semi-major axis
#' @param b distance of semi-minor axis
#' @param m1 exponent of semi-major axis
#' @param m2 exponent of semi-minor axis
#' @param theta ellipse rotation
#' @param n number of polygon points
#' @param xy polygon points
#'
#' @export
ellipse <- S7::new_class(
  "ellipse",
  parent = centerpoint,
  properties = list(
    center = S7::new_property(class = point, default = point(0, 0)),
    a = S7::new_property(S7::class_numeric, default = 1),
    b = S7::new_property(S7::class_numeric, default = 1),
    m1 = S7::new_property(S7::class_numeric, default = 2),
    m2 = S7::new_property(S7::class_numeric, default = 2),
    theta  = S7::new_property(angle, default = angle(0)),
    n = S7::new_property(S7::class_numeric, default = 360),
    xy = S7::new_property(
      getter = function(self) {
        t <- seq(0, 2, length.out = self@n + 1)[-(self@n + 1)]
        cost <- cospi(t)
        sint <- sinpi(t)
        x <- sign(cost) * self@a * abs(cost) ^ (2 / m1)
        y <- sign(sint) * self@b * abs(sint) ^ (2 / m2)
        xy <- cbind(x = x, y = y)
        xy <- rotate2columnmatrix(xy, theta = self@theta)
        xy[, "x"] <- xy[, "x"] + self@center@x
        xy[, "y"] <- xy[, "y"] + self@center@y
        xy

      }
    )
  ),
  constructor = function(center = S7::class_missing,
                         a = S7::class_missing,
                         b = S7::class_missing,
                         m1 = S7::class_missing,
                         m2 = S7::class_missing,
                         theta = S7::class_missing,
                         n = S7::class_missing) {
    if (is.numeric(theta))
      theta <- angle(radian = theta)

    S7::new_object(
      centerpoint(center = center),
      a = a,
      b = b,
      m1 = m1,
      m2 = m2,
      theta = theta,
      n = n
    )
  }
)






# Rectangle ----

#' rectangle class
#' @param center point at center of the circle
#' @param width width
#' @param height height
#' @param xy 2-column matrix of points
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' rectangle(p, width = 2, height = 2)
#' @export
rectangle <- S7::new_class(
  name = "rectangle",
  parent = centerpoint,
  properties = list(
    width = S7::new_property(class = S7::class_numeric, default = 1),
    height = S7::new_property(class = S7::class_numeric, default = 1),
    northeast = S7::new_property(point, getter = function(self){
      point(self@width / 2, self@height / 2) + self@center
    }),
    northwest = S7::new_property(point, getter = function(self){
      point(self@width / -2, self@height / 2) + self@center
    }),
    southwest = S7::new_property(point, getter = function(self){
      point(self@width / -2, self@height / -2) + self@center
    }),
    southeast = S7::new_property(point, getter = function(self){
      point(self@width / 2, self@height / -2) + self@center
    }),
        xy = S7::new_property(
      getter = function(self) {
        p <- as.matrix(point_list(c(self@northeast,
                               self@northwest,
                               self@southwest,
                               self@southeast)))
        rownames(p) <- c("northeast",
                         "northwest",
                         "southwest",
                         "southeast")
        p
      }
    )
  ),
  constructor = function(
    center = S7::class_missing,
    width = S7::class_missing,
    height = S7::class_missing,
    northeast = S7::class_missing,
    northwest = S7::class_missing,
    southwest = S7::class_missing,
    southeast = S7::class_missing) {
    hasnorth <- FALSE
    hassouth <- FALSE
    haseast <- FALSE
    haswest <- FALSE
    if (length(northeast) == 1) {
      north <- northeast@y
      east <- northeast@x
      hasnorth <- TRUE
      haseast <- TRUE
    }
    if (length(northwest) == 1) {
      north <- northwest@y
      west <- northwest@x
      hasnorth <- TRUE
      haswest <- TRUE
    }

    if (length(southeast) == 1) {
      south <- southeast@y
      east <- southeast@x
      hassouth <- TRUE
      haseast <- TRUE
    }

    if (length(southwest) == 1) {
      south <- southwest@y
      west <- southwest@x
      hassouth <- TRUE
      haswest <- TRUE
    }

    if (hassouth && hasnorth) {
      height <- abs(north - south)
    }

    if (haswest && haseast) {
      width <- abs(west - east)
    }

    if (length(center) == 1 && length(width) == 1 && length(height) == 1) {
      r <- S7::new_object(S7::S7_object(), center = center, width = width, height = height)
    } else if (length(center) == 1 && (hasnorth || hassouth) && (haswest || haseast)) {
      if (haseast) {
        width <- abs(center@x - east) * 2
      } else {
        width <- abs(center@x - west) * 2
      }
      if (hasnorth) {
        height <- abs(center@y - north) * 2
      } else {
        height <- abs(center@y - south) * 2
      }
      r <- S7::new_object(S7::S7_object(), center = center, width = width, height = height)
    } else if (length(width) == 1 && length(height) == 1 && (hasnorth || hassouth) && (haswest || haseast)) {
      if (haseast) {
        c.x <- east - width / 2
      } else {
        c.x <- west + width / 2
      }
      if (hasnorth) {
        c.y <- north - height / 2
      } else {
        c.y <- south + height / 2
      }
      r <- S7::new_object(S7::S7_object(), center = point(c.x, c.y), width = width, height = height)
    } else {
      stop("There is not enough information to make a rectangle.")
    }
      r
  }
)


