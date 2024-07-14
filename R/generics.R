


# distance----
#' Calculate distance between 2 points
#'
#' @param x a point, line, segment, or circle
#' @param y a point, line, or circle
#' @param center logical. if the distance between 2 circles should be calculated from their centers or their edges
#' @rdname distance
#' @return numeric
#' @examples
#' # Distance between two objects
#' p1 <- point(0, 0)
#' p2 <- point(3, 4)
#' distance(p1, p2)
#'
#' # Distance between the endpoints of a segment
#' s1 <- segment(p1, p2)
#' distance(s1)
#'
#' # Distance between a point and a line
#' l1 <- line(slope = 0, intercept = 1)
#' distance(p1, l1)
#'
#' # Shortest distance between the edges of 2 circles
#' c1 <- circle(p1, radius = 1)
#' c2 <- circle(p2, radius = 2)
#' distance(c1, c2)
#'
#' # Distance between the centers of 2 circles
#' distance(c1, c2, center = TRUE)
#' @export
distance <- new_generic("distance", c("x", "y"))
method(distance, list(point, point)) <- function(x,y) {
  d <- (y - x)
  d@r
}
method(distance, list(point, class_missing)) <- function(x,y) {
  x@r
}
method(distance, list(point, line)) <- function(x,y) {
  abs(y@a * x@x + y@b * x@y + y@c) / sqrt(y@a * y@a + y@b * y@b)
}
method(distance, list(line, point)) <- function(x,y) {
  distance(y, x)
}
method(distance, list(segment, class_missing)) <- function(x,y) {
  distance(x@p1, x@p2)
}
method(distance, list(circle, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x@center)
  if (!center) {
    if (x@radius + y@radius > distance(d)) {
      d <- point(0,0)
    } else {
      px <- anchor(x, d@theta)
      py <- anchor(y, pi + d@theta)
      d <- py - px
    }

  }
  distance(d)
}
method(distance, list(point, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x)
  if (!center) {
    py <- anchor(y, pi + d@theta)
    d <- py - x
  }
  d@r
}
method(distance, list(circle, point)) <- function(x,y, center = FALSE) {
  distance(y, x)
}



# Intersection angle----
#' Compute the angle of the intersection of two objects
#'
#' @param x an object (point, segment, line)
#' @param y an object (point, segment, line)
#' @param degrees return angle in degrees if TRUE
#' @return an angle in radians
#' @export
intersection_angle <- new_generic("intersection_angle", c("x", 'y'))
method(intersection_angle, list(line, line)) <- function(x, y) {
  y@angle - x@angle

}
method(intersection_angle, list(segment, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@line@angle
  } else
    NA_real_
}
method(intersection_angle, list(line, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@angle - x@line@angle
  } else
    NA_real_
}
method(intersection_angle, list(segment, line)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@angle
  } else
    NA_real_
}

# slope----
#' Find slope of a line or object associated with a line
#'
#' @param x point, segment, line, or angle
#' @export
slope <- new_generic("slope", "x")
method(slope, point_or_point_list) <- function(x) {
  x@y / x@x
}

method(slope, segment_or_segment_list) <- function(x) {
  x@line@slope
}

method(slope, angle_or_angle_list) <- function(x,y) {
  tan(x)
}


# midpoint----
#' @export
midpoint <- new_generic("midpoint", c("x", "y"), fun = function(x,y, position = .5, ...) {
  S7::S7_dispatch()
})
method(midpoint, list(point, point)) <- function(x,y, position = .5, ...) {
  m <- x + ((y - x) * position)
  addstyle <- style(...)
  if (length(get_non_empty_props(addstyle))) {
    if (S7_inherits(m, point_list)) {
      purrr::walk(m, \(p) {
        p@style <- p@style + addstyle
      })

    } else {
      m@style <- m@style + addstyle
    }
  }
  m
}
method(midpoint, list(segment, class_missing)) <- function(x,y, position = .5, ...) {
  x@p1@style <- x@p1@style + x@style
  x@p2@style <- x@p2@style + x@style
  midpoint(x@p1, x@p2, position = position, ...)
}

method(midpoint, list(arrow_segment, class_missing)) <- function(x,y, position = .5, ...) {
  midpoint(x@p1, x@p2, position = position, ...)
}

method(midpoint, list(arc, class_missing)) <- function(x,y, position = .5, ...) {
  m <- x@start@turn + (x@theta@turn * position)
  x@center + polar(theta = turn(m), r = x@radius, style = style_point() + x@style + style(...))
}

# anchor----
#' @export
anchor <- new_generic("anchor", dispatch_args = c("x", "position"))

## anchor circle----
method(anchor, list(circle, angle)) <- function(x, position) {

  point(x = cos(position), y = sin(position)) * x@radius + x@center

}

method(anchor, list(circle, class_numeric)) <- function(x, position) {

  position <- angle(degree = position)

  anchor(x,position)

}

method(anchor, list(circle, class_character)) <- function(x, position) {
  position <- cardinalpoint(position)
  anchor(x,position)
}

## anchor ellipse----
method(anchor, list(ellipse, angle)) <- function(x, position) {
  cost <- cos(position)
  sint <- sin(position)
  x <- sign(cost) * x@a * abs(cost) ^ (2 / x@m1)
  y <- sign(sint) * x@b * abs(sint) ^ (2 / x@m2)
  xy <- cbind(x = x, y = y)
  xy <- rotate2columnmatrix(xy, theta = x@theta)
  point(xy) + x@center

}
method(anchor, list(ellipse, class_numeric)) <- function(x, position) {
  position <- angle(radian = position)
  anchor(x,position)
}

method(anchor, list(ellipse, class_character)) <- function(x, position) {
  position <- cardinalpoint(position)
  anchor(x,position)
}


method(anchor, list(line, class_numeric)) <- function(x, position) {
  point(position, position * x@slope + x@intercept)
}

method(anchor, list(class_list, class_missing)) <- function(x, position) {
  point_list(lapply(x,\(x) anchor(x, position)))
}

# inside----
#' is a point inside a shape ?
#'
#' @param x object
#' @param y object
#' @export
inside <- new_generic("inside", c("x", "y"))
method(inside, list(point, rectangle)) <- function(x,y) {
 insideTF <-  (x@x <= y@northeast@x &&
    x@x >= y@northwest@x &&
    x@y <= y@northeast@y &&
    x@y >= y@southeast@y) * 2 - 1

 if (length(intersection(x,y)) > 0) {
   insideTF <- 0
 }

 insideTF
}

method(inside, list(point, circle)) <- function(x,y) {
  xc <- x - y@center
  -1 * sign(xc@r - y@radius)
}

method(inside, list(point, ellipse)) <- function(x,y) {
  rx <- rotate(x, -1*y@theta, origin = y@center)
  xc <- rx - y@center
  -1 * sign((xc@x ^ 2) / (y@a ^ 2) + (xc@y ^ 2) / (y@b ^ 2) - 1)
}

method(inside, list(point_list, rectangle)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}

method(inside, list(point_list, circle)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}

method(inside, list(point_list, ellipse)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}


# intersection----
#' intersection of 2 objects (e.g., lines)
#'
#' @param x object
#' @param y object
#' @export
intersection <- new_generic("intersection", c("x", "y"))
method(intersection, list(line, line)) <- function(x,y) {
  c_p <- x@a * y@b - y@a * x@b
  if (identical(TRUE, all.equal(c_p, 0))) {
    p <- list()
  } else {
    a_p <- (x@b * y@c - y@b * x@c)
    b_p <- (x@c * y@a - y@c * x@a)

    p <- point(a_p / c_p, b_p / c_p)
  }
  p
}
method(intersection, list(segment, segment)) <- function(x,y) {

  i_line <- intersection(x@line, y@line)
  p <- list()
  if (length(i_line) > 0) {
    A <- t(rbind((x@p2 - x@p1)@xy,
                 (y@p1 - y@p2)@xy))
    B <- t((y@p1 - x@p1)@xy)
    C <- solve(A, B)
    if (all(C >= 0 & C <= 1)) {
      p <- i_line
    }
  }
  p
}



method(intersection, list(line, segment)) <- function(x,y) {
  intersection(intersection(x, y@line), y)
}

method(intersection, list(segment, line)) <- function(x,y) {
  intersection(y, x)
}

method(intersection, list(line, circle)) <- function(x,y) {
  # https://cp-algorithms.com/geometry/circle-line-intersection.html
  c0 <- circle(center = point(0,0), radius = y@radius)
  A <- x@a
  B <- x@b
  C <- x@c + A * y@center@x + B * y@center@y
  d0 <- abs(C) / sqrt(A ^ 2 + B ^ 2)
  A2B2 <- (A * A + B * B)
  x0 <- -A * C / A2B2
  y0 <- -B * C / A2B2
  if (C * C > y@radius * y@radius * A2B2 + .Machine$double.eps) {
    p <- list()
  } else if (abs(C * C - y@radius * y@radius * A2B2) < 3 * .Machine$double.eps) {
    p <- point(x0 , y0) + y@center
  } else {
    d <- y@radius * y@radius - C * C / A2B2
    m <- sqrt(d / A2B2)
    ax <- x0 + B * m
    bx <- x0 - B * m
    ay <- y0 - A * m
    by <- y0 + A * m
    pa <- point(ax, ay) + y@center
    pb <- point(bx, by) + y@center
    p <- c(pa, pb)
  }
  p

}

method(intersection, list(circle, line)) <- function(x,y) {
  intersection(y,x)
}

method(intersection, list(point, line)) <- function(x, y) {
  if (identical(TRUE, all.equal(0, y@a * x@x + y@b * x@y + y@c))) {
    x
  } else {
    list()
  }
}
method(intersection, list(line, point)) <- function(x, y) {
  intersection(y, x)
}


method(intersection, list(point, segment)) <- function(x, y) {
  if (identical(x, intersection(x, y@line))) {
    xp1 <- distance(x, y@p1)
    xp2 <- distance(x, y@p2)
    p1p2 <- distance(y)
    if (identical(TRUE, all.equal(p1p2, xp1 + xp2))) {
      x
    } else {
      list()
    }
  } else {
    list()
  }
}

method(intersection, list(segment, point)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(line, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                            p2 = y@northwest)),
    intersection(x, segment(p1 = y@northwest,
                            p2 = y@southwest)),
    intersection(x, segment(p1 = y@southwest,
                            p2 = y@southeast)),
    intersection(x, segment(p1 = y@southeast,
                            p2 = y@northeast))
  ))
}
method(intersection, list(rectangle, line)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(segment, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                                   p2 = y@northwest)),
           intersection(x, segment(p1 = y@northwest,
                                   p2 = y@southwest)),
           intersection(x, segment(p1 = y@southwest,
                                   p2 = y@southeast)),
           intersection(x, segment(p1 = y@southeast,
                                   p2 = y@northeast))
  ))
}
method(intersection, list(rectangle, segment)) <- function(x, y) {
  intersection(y, x)
}

method(intersection, list(point, rectangle)) <- function(x, y) {
  unique(c(intersection(x, segment(p1 = y@northeast,
                                   p2 = y@northwest)),
           intersection(x, segment(p1 = y@northwest,
                                   p2 = y@southwest)),
           intersection(x, segment(p1 = y@southwest,
                                   p2 = y@southeast)),
           intersection(x, segment(p1 = y@southeast,
                                   p2 = y@northeast))
  ))
}

method(intersection, list(rectangle, point)) <- function(x, y) {
  intersection(y, x)
}


method(intersection, list(segment, ellipse)) <- function(x, y, sep = .01) {
  p <- seq(0,1,sep)
  sp <- point_list(lapply(p, \(m) midpoint(x,position = m)))
  i <- inside(sp, y)
  d <- data.frame(p = p, i = i, lag_i = c(i[-1], 2))
  d_change <- d[d$i * d$lag_i == -1,]
  d_on <- d[d$i == 0, ]
  if (nrow(d_change) == 0 && nrow(d_on) == 0) {
    return(NULL)
  } else {
    pp <- list()
    for (ii in d_on$p) {
      pp <- c(pp, midpoint(x, position = ii))
    }

    for (ii in d_change$p) {
      s <- segment(midpoint(x, position = ii), midpoint(x, position = ii + sep))

      if (distance(s) < (.1 ^ 15)) {
        pp <- c(pp, s@p1)
      } else {
        pp <- c(pp, intersection(s, y, sep = .5))
      }
    }
  }
  point_list(pp[])

}


# points(x = intersect_line_segment@x, y = intersect_line_segment@y)
method(intersection, list(line, ellipse)) <- function(x, y) {
  # theta <- angle(degree = seq(0, 360))
  x <- line(slope = 1, intercept = 0)
  y <- ellipse(center = point(0,0),a = 1, b = 1)
  eps <- distance(anchor(y, angle(radian = 0)), anchor(y, angle(radian = .Machine$double.eps)))
  # eps <- .00001
  # par(pty = "s")
  xmax <- max(y@xy[,1])
  xmin <- min(y@xy[,1])
  ymax <- max(y@xy[,2])
  ymin <- min(y@xy[,2])
  if (is.infinite(x@slope)) {
    s <- segment(point(x@xintercept,ymin), point(x@xintercept,ymax))
  } else {
    s1 <- segment(anchor(x, xmin),
                  anchor(x, xmax))
    s2 <- segment(point((ymin - x@intercept) / x@slope, ymin),
                  point((ymax - x@intercept) / x@slope,ymax))
    if (distance(s1) < distance(s2)) {
      s <- s1
    } else {
      s <- s2
    }
  }



  d <- s1@p2 - s1@p1
  s_points <- s1@p1 + seq(0,1,.5) * d
  c_points <- s_points - y@center
  anchor(y, c_points@angle@degree)

  test_x <- seq(xmax, ymax, length.out = 1000)
  text_y <- x@slope * test_x + x@intercept

  delta_angle <- 2
  current_angle <- 0
  # plot(y@xy, type = "l", pty = "s")
  # abline(b = x@slope, a = x@intercept)
  m1 <- point(1,0)
  first <- FALSE
  second <- FALSE

  line_ellipse_intersection <- list()
  i <- 0

  while (current_angle < 362 && !(m1@r < eps * 10) && delta_angle > .Machine$double.eps * 10) {
    s <- segment(anchor(y, angle(degree = current_angle)),
                 anchor(y, angle(degree = current_angle + delta_angle)))
    intersect_line_segment <- intersection(x, s)
    # cat(paste0(distance(s), "\n"))



    if (length(intersect_line_segment) > 0) {
      first <- TRUE
      s_low <- segment(anchor(y, angle(degree = current_angle)),
                       anchor(y, angle(degree = current_angle + delta_angle / 2)))

      s_high <- segment(anchor(y, angle(degree = current_angle + delta_angle / 2)),
                        anchor(y, angle(degree = current_angle + delta_angle )))

      s_low@p1 == s@p1
      s_low@p1 == s_high@p1
      s_low@p2 == s_high@p1
      s_low@p1 == s_low@p2

      i_test_low <- intersection(x, s_low@line)
      i_test_high <- intersection(x, s_high@line)

      if (length(i_test_low) + length(i_test_high) == 0) {
        stop("wtf")
        s_low <- segment(anchor(y, angle(degree = current_angle - delta_angle * 0.0000001)),
                         anchor(y, angle(degree = current_angle + .5 * delta_angle)))

        s_high <- segment(anchor(y, angle(degree = current_angle + .5 * delta_angle)),
                          anchor(y, angle(degree = current_angle + delta_angle * 1.0000001 )))
        i_test_low <- intersection(x, s_low@line)
        i_test_high <- intersection(x, s_high@line)

        }

      delta_angle <- delta_angle / 2

      if (length(i_test_low) == 0) {
        current_angle <- current_angle + delta_angle
      }





      d1 <- s@p1 - intersect_line_segment
      m1 <- midpoint(s) - intersect_line_segment
      if (d1@r < eps * 10) {
        m1 <- point(0,0)
        line_ellipse_intersection <- s@p1
      }
      d2 <- s@p2 - intersect_line_segment
      if (d2@r < eps * 10) {
        m1 <- point(0,0)
        line_ellipse_intersection <- s@p2
      }
    } else {
      # delta_angle <- delta_angle * 1.01
      i <- i + 1
      if (delta_angle < 2) {
        if (first && !second) {
          line_ellipse_intersection$first <- anchor(y, angle(degree = current_angle))
          second <- TRUE
        }
        if (second) {
          line_ellipse_intersection$second <- anchor(y, angle(degree = current_angle))
        }

        delta_angle <- 2
        current_angle <- current_angle + delta_angle

      } else {
        current_angle <- current_angle + delta_angle
      }

    }
  }
  line_ellipse_intersection
}

# x <- line(slope = .1, intercept = 0)
# y <- ellipse(a = 100, b = 100)
# pp <- intersection(x,y)



# Perpendicular ----

#' Find point perpendicular to 2 points
#'
#' @name perpendicular_point
#' @param e1 first point
#' @param e2 second point
#' @examples
#' x <- point(0,0)
#' y <- point(1,1)
#' # Find point perpendicular to x and y going vertically first
#' x %|-% y
#' # Find point perpendicular to x and y going horizontally first
#' x %-|% y
NULL

#' @name perpendicular_vertical
#' @rdname perpendicular_point
#' @aliases %|-%
#' @export
`%|-%` <- new_generic("%|-%", c("e1", "e2"))
method(`%|-%`, list(point, point)) <- function(e1,e2) {
  point(e1@x, e2@y)}

#' @name perpendicular_horizontal
#' @rdname perpendicular_point
#' @aliases %-|%
#' @param e1 first point
#' @param e2 second point
#' @export
`%-|%` <- new_generic("%-|%", c("e1", "e2"))
method(`%-|%`, list(point, point)) <- function(e1,e2) {
  point(e2@x, e1@y)}



# Rotate ----

#' Rotate an object in 2 dimensions
#'
#' @param x object
#' @param theta angle
#' @param center length 2 vector  or point about which rotation occurs
#' @export
rotate <- new_generic(name = "rotate", dispatch_args = c("x", "theta"), function(x, theta, ..., origin = point(0, 0)) {
  S7_dispatch()
})
# Rotate Line
method(rotate, list(line, angle)) <- function(x, theta, origin = point(0, 0)) {
  # https://math.stackexchange.com/a/2278909
  A <- x@a * cos(theta) + x@b * sin(theta)
  B <- x@b * cos(theta) + x@a * sin(theta)
  C <- (x@a - A) * origin@x + (x@b - B) * origin@y + x@c
  line(a = A, b = B, c = C)
}
method(rotate, list(line, class_numeric)) <- function(x, theta, origin = point(0, 0)) {
  rotate(x, angle(radian = theta), origin = origin)
}
# Rotate point
method(rotate, list(point, angle)) <- function(x, theta, origin = point(0, 0)) {
  x0 <- x - origin
  xr <- rotate2columnmatrix(x0@xy, theta)
  dimnames(xr) <- list(NULL, NULL)
  point(xr[1, 1, drop = TRUE], xr[1, 2, drop = TRUE]) + origin
}
method(rotate, list(point, class_numeric)) <- function(x, theta, origin = point(0, 0)) {
  rotate(x, angle(radian = theta), origin = origin)
}

# Rotate segment
method(rotate, list(segment, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  p1r <- rotate(x@p1, theta, origin = origin)
  p2r <- rotate(x@p2, theta, origin = origin)
  segment(p1r, p2r)
}

# Rotate centerpoint
method(rotate, list(centerpoint, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  x_center_r <- rotate(x@center, theta, origin = origin)
  x@center <- x_center_r
  x
  }

# Rotate ellipse
method(rotate, list(ellipse, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  x_center_r <- rotate(x@center, theta, origin = origin)
  ellipse(center = x_center_r, a = x@a, b = x@b, theta = x@theta + theta, n = x@n)
}
# Rotate rectangle
method(rotate, list(rectangle, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {

  point_list(c(
    rotate(point(x.northeast), theta),
    rotate(point(x.northwest), theta),
    rotate(point(x.southwest), theta),
    rotate(point(x.southeast), theta)
    ))
}

# Projection ----
#' Find projection of a point on a line
#'
#' @param x point
#' @param y line
#' @export
projection <- new_generic("projection", c("x", "y"))
method(projection, list(point, line)) <- function(x,y) {
ab <- y@a * y@a + y@b * y@b
xp <- (y@b * y@b * x@x - y@b * y@a * x@y - y@a * y@c) / ab
yp <- (y@a * y@a * x@y - y@a * y@b * x@x - y@b * y@c) / ab
point(xp, yp, style = x@style)
}

# method(rotate, list(class_numeric,
#                         class_numeric)) <-
#   function(x, theta, center = NULL, ...) {
#     if (!is.matrix(x)) x <- matrix(x)
#     if (is.null(center)) {
#       center <- rep(0, ncol(x))
#     }
#     if (length(center) != ncol(x)) stop(paste0("The center's length must equal the number of columns in x. In this case: length(center) = ", length(center), " ncol(x) = ", ncol(x)))
#     o <- matrix(rep(center, nrow(x)), byrow = TRUE, nrow = nrow(x))
#     xo <- x - o
#     if (length(theta) == 1) {
#       sin_theta <- sin(theta)
#       cos_theta <- cos(theta)
#       rotation_matrix <- matrix(c(cos_theta, -sin_theta, sin_theta, cos_theta), nrow = 2, byrow = TRUE)
#     }
#
#     if (length(theta) == 3) {
#       sin_theta <- sin(theta)
#       cos_theta <- cos(theta)
#       rotation_matrix <- matrix(c(cos_theta, -sin_theta, sin_theta, cos_theta), nrow = 2, byrow = TRUE)
#     }
#
#     result <- xo %*% rotation_matrix + o
#     colnames(result) <- colnames(x)
#     result
#
#   }

# glimpser ----
glimpser <- new_generic(name = "glimpser", dispatch_args = "x")
method(glimpser, class_character) <- function(x, type = "chr", prepend_spaces = 1) {
  add_ellipse <- ""
  n <- ""
  if (length(x) > 1) {
    n <- paste0("[1:", length(x), "] ")
  }
  if (length(x) > 4) {
    x <- head(x, n = 4)
    add_ellipse <- ", ..."
  }
  paste0(type,
         paste0(rep(" ", prepend_spaces), collapse = ""),
         n,
         paste0(x, collapse = ", "), add_ellipse)
}

method(glimpser, class_integer) <- function(x, type = "int", prepend_spaces = 1) {
  glimpser(as.character(x), type, prepend_spaces)
}

method(glimpser, class_double) <- function(x, type = "num", prepend_spaces = 1) {
  if (all(x == round(x))) {
    glimpser(as.integer(x), type, prepend_spaces)
  } else {
    glimpser(formatC(x, digits = 2, format = "f"), type, prepend_spaces)
  }
}

method(glimpser, angle) <- function(x, type = "angle", prepend_spaces = 1) {
  glimpser(x@radian, type, prepend_spaces)
}

# str ----
#' structure
#'
#' @param object object
#' @keywords internal
str <- new_generic(
  name = "str",
  dispatch_args = "object")

# from S7
obj_type <- function(x) {
  if (identical(x, quote(expr = ))) {
    "missing"
  }
  else if (inherits(x, "S7_object")) {
    "S7"
  }
  else if (isS4(x)) {
    "S4"
  }
  else if (is.object(x)) {
    "S3"
  }
  else {
    "base"
  }
}

obj_desc <- function(x) {
  switch(
    obj_type(x),
    missing = "MISSING",
    base = paste0("<", typeof(x), ">"),
    S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
    S4 = paste0("S4<", class(x), ">"),
    S7 = paste0("<", class(x)[[1]], ">")
  )
}

# modified from S7 internals
str_nest <- function(object,
                     prefix,
                     nest.lev = 0,
                     omit,
                     indent.str = paste(
                       rep.int(" ",
                               max(0, nest.lev)),
                       collapse = ".."
                       ),
                     ...
) {
  names <- format(names(object))
  for (i in seq_along(object)) {
    cat(indent.str, prefix, " ", names[[i]], ":", sep = "")
    xi <- object[[i]]

    if (is.function(xi)) {
      S7:::str_function(xi, nest.lev = nest.lev + 1)
    } else if (S7::S7_inherits(xi)) {
      str(xi, ..., nest.lev = nest.lev + 1)
    } else {
      utils::str(xi, ...)
    }

  }
}

str_properties <- function(
    object,
    nest.lev = 0,
    additional = TRUE,
    omit) {
      p_names <- prop_names(object)
  cat(if (nest.lev > 0) " ")
  cat(S7:::obj_desc(object))
  if (length(omit) > 0 & additional & nest.lev < 1) {
    additional_text <- paste0(" Omitted props: ", paste(p_names[p_names %in% omit], collapse = ", "))
    cat(additional_text)
  }
  cat("\n")

  str_nest(object = props(object)[!(p_names %in% omit)],
           prefix = "@",
           nest.lev = nest.lev)


}


method(str, point_or_point_list) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = c("theta", "r", "xy", "style", ".data")) {
  str_properties(object,
                     omit = omit,
                     nest.lev = nest.lev)
}

method(str, polar) <- function(object,
                                   nest.lev = 0,
                                   additional = FALSE,
                                   omit = c("xy", "style", ".data")) {
  str_properties(object,
                     omit = omit,
                     nest.lev = nest.lev)

}


method(str, label_or_label_list) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = c("xy", "style")) {
  str_properties(object,
                     omit = omit,
                     nest.lev = nest.lev)
}

method(str, line_or_line_list) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = c("xy",
             "style",
             ".data",
             "a",
             "b",
             "c",
             "angle",
             "xintercept")) {
  str_properties(object,
                     omit = omit,
                     nest.lev = nest.lev)
}

method(str, rectangle) <- function(
    object,
    nest.lev = 0,
    additional = FALSE,
    omit = c(
      "northeast",
      "northwest",
      "southwest",
      "southeast",
      "xy",
      "style",
      ".data",
      "a",
      "b",
      "c",
      "angle",
      "xintercept")) {
  str_properties(
    object,
    omit = omit,
    nest.lev = nest.lev)
}



method(str, angle_or_angle_list) <- function(object,
                                        nest.lev = 0,
                                        additional = FALSE,
                                        omit = ".data") {
  str_properties(object,
                     omit = omit,
                     nest.lev = nest.lev)
}

method(str, class_any) <- function(object) {
  S7:::str.S7_base_class(object)
}



method(print, rectangle) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, point) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, point_list) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, line) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, label) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, label_list) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(str, style_or_style_list) <- function(object,
  nest.lev = 0,
  additional = FALSE,
  omit = NULL) {

  omit_names <- names(props(object))
  omit <- omit %||% Filter(\(o_name) {length(prop(object, name = o_name)) == 0}, omit_names)


str_properties(object,
omit = omit,
nest.lev = nest.lev, additional = FALSE)

}

method(print, style_base) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}

method(print, style_list) <- function(x, ...) {
  str(x, ...)
  invisible(x)
}


method(print, angle) <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}



# as.matrix ----

# Convert to matrix, data.frame
method(as.matrix, xy) <- function(x) {
  x@xy
}
class(point(0,3))
method(as.matrix, point_list) <- function(x) {
  do.call(rbind, lapply(x, function(p) p@xy))
}
method(as.data.frame, point_list) <- function(x) {
  as.data.frame(as.matrix(x))
}




# as.data.frame ----

method(as.data.frame, point) <- function(x) {


  as.data.frame(as.matrix(x))
}

# method(as.matrix, rectangle) <- function(x) {
#   x@xy
# }
# method(as.matrix, circle) <- function(x) {
#   x@xy
# }
# method(as.matrix, ellipse) <- function(x) {
#   x@xy
# }
#
# method(as.matrix, segment) <- function(x) {
#   x@xy
# }

method(as.matrix, segment_list) <- function(x) {
  p1 <- do.call(rbind, lapply(x, function(p) p@p1@xy))
  p2 <- do.call(rbind, lapply(x, function(p) p@p2@xy))
  m <- cbind(p1,p2)
  rownames(m) <- NULL
  colnames(m) <- c("p1_x", "p1_y", "p2_x", "p2_y")
  m
}


# Resect ----
#' resect
#'
#' Shorten segments
#' @param resect a numeric distance
#' @export
resect <- new_generic("resect", c("x", "resect"))
method(resect,
           list(segment,
                class_numeric)) <- function(x,resect, resect_end = resect) {
  d <- x@p2 - x@p1
  x@p1 <- x@p1 + point(theta = d@theta, r = resect)
  x@p2 <- x@p2 + point(theta = d@theta + angle(turn = .5), r = resect_end)
  x
  }
method(
  resect,
  list(point, class_numeric)) <- function(
    x,
    resect,
    resect_end = resect,
    origin = point(0, 0)) {
  resect(segment(origin, x), resect)@p2
}


# Equation ----
#' equation
#'
#' @param x object
#' @param type equation type. Can be `y`, `general`, or `parametric`
#' @export
equation <- new_generic("equation", dispatch_args = "x")
method(equation, line) <- function(x,
                                   type = c("y", "general", "parametric"),
                                   digits = 2) {
  type <- match.arg(type)
  eq <- ""
  if (type == "y") {
    eq <- trimmer(paste0(
      "*y* =",
      rounder(x@slope, digits = digits, add = FALSE),
      "*x*",
      rounder(x@intercept, digits = digits, add = TRUE)
    ))
  } else if (type == "general") {
    eq <- trimmer(paste0(
      rounder(x@a, digits = digits),
      "*x*",
      rounder(x@b, digits = digits, add = TRUE),
      "*y*",
      rounder(x@c, digits = digits, add = TRUE),
      " = 0"
    ))

  } else if (type == "parametric") {
    eq <- trimmer(paste0(
      "*y* = ",
      rounder(x@slope, digits = digits),
      "*t*",
      rounder(x@intercept, digits = digits, add = TRUE),
      "<br>*x* = *t*"
    ))
  }
  eq
}

# convert ----
method(convert, list(style, class_list)) <- function(from, to) {
  l <- list()
  for (i in prop_names(from)) {
    pp <- prop(from, i)
    if (length(pp) > 0) {
      l[[i]] <- pp
    }
  }
  l
}

# polar_just ----
#' Create a style object with hjust and vjust specified via radial coordinates
#'
#' @param angle numeric (radians), angle, or point (which contains angle and distance (multiplier))
#' @param multiplier make hjust and vjust larger or smaller
#' @export
polar_just <- new_generic(name = "polar_just", dispatch_args = c("style", "angle", "multiplier"))
method(polar_just, list(style_or_style_label, class_angle_or_numeric, class_numeric)) <- function(style, angle, multiplier) {

  hjust <- polar2just(angle, multiplier, axis = "h")
  vjust <- polar2just(angle, multiplier, axis = "v")
  style + style_label(hjust = hjust, vjust = vjust)
}
method(polar_just, list(style_or_style_label, class_angle_or_numeric, class_missing)) <- function(style, angle, multiplier) {
  polar_just(style, angle, 1)
}

method(polar_just, list(style_or_style_label, point, class_missing)) <- function(style, angle, multiplier) {
  polar_just(style, angle@angle@radian, angle@r)
}

method(polar_just, list(class_missing, class_angle_or_numeric, class_numeric)) <- function(style, angle, multiplier) {
  polar_just(style_label(), angle, multiplier)
}


method(polar_just, list(class_missing, class_angle_or_numeric, class_missing)) <- function(style, angle, multiplier) {
  polar_just(style_label(), angle)
}

method(polar_just, list(class_missing, point, class_missing)) <- function(style, angle, multiplier) {
  polar_just(style_label(), angle@angle@radian, angle@r)
}

# get points ----

method(get_points, segment) <- function(x) {
  p1@style <- p1@style + x@style
  p2@style <- p2@style + x@style
  point_list(c(x@p1, x@p2))
}

method(get_points, label_or_label_list) <- function(x) {
  x@p
}

method(get_points, point_or_point_list) <- function(x) {
  x
}


method(get_points, class_list) <- function(x) {
  allsameclass(x, "point")
  point_list(x)
}



