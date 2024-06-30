
# distance----
#' Calculate distance between 2 points
#'
#' @param x a point or segment
#' @param y a point or NULL
#' @param center logical. if the distance between 2 circles should be calculated from their centers or their edges
#' @rdname distance
#' @return numeric
#' @export
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
distance <- S7::new_generic("distance", c("x", "y"))
S7::method(distance, list(point, point)) <- function(x,y) {
  d <- (y - x)
  d@distance
}
S7::method(distance, list(point, S7::class_missing)) <- function(x,y) {
  x@distance
}
S7::method(distance, list(point, line)) <- function(x,y) {
  abs(y@a * x@x + y@b * x@y + y@c) / sqrt(y@a * y@a + y@b * y@b)
}
S7::method(distance, list(line, point)) <- function(x,y) {
  distance(y, x)
}
S7::method(distance, list(segment, S7::class_missing)) <- function(x,y) {
  distance(x@p1, x@p2)
}
S7::method(distance, list(circle, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x@center)
  if (!center && !identical(x,y)) {
    if (x@radius + y@radius > distance(d)) {
      d <- point(0,0)
    } else {
      px <- anchor(x, d@angle)
      py <- anchor(y, pi + d@angle)
      d <- py - px
    }

  }
  distance(d)
}
S7::method(distance, list(point, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x)
  if (!center) {
    py <- anchor(y, pi + d@angle)
    d <- py - x
  }
  d@distance
}
S7::method(distance, list(circle, point)) <- function(x,y, center = FALSE) {
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
#' @examples
#' # Angle of a line between two points
#' p1 <- point(1, 1)
#' p2 <- point(4, 5)
#' angle(p1, p2)
#' # Angle of a line segment
#' s1 <- segment(p1, p2)
#' angle(s1)
#' # Angle of a line
#' l1 <- s1@line
#' angle(l1)
#' # Angle between two lines
#' l2 <- line(slope = 2, intercept = 0)
#' angle(l1, l2)
intersection_angle <- S7::new_generic("intersection_angle", c("x", 'y'))
S7::method(intersection_angle, list(line, line)) <- function(x, y) {
  y@angle - x@angle

}
S7::method(intersection_angle, list(segment, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@line@angle
  } else
    NA_real_
}
S7::method(intersection_angle, list(line, segment)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@angle - x@line@angle
  } else
    NA_real_
}
S7::method(intersection_angle, list(segment, line)) <- function(x, y) {
  if (length(intersection(x, y)) > 0) {
    y@line@angle - x@angle
  } else
    NA_real_
}

# slope----
#' @export
slope <- S7::new_generic("slope", c("x", 'y'))
S7::method(slope, list(point, point)) <- function(x,y) {
  d <- y - x
  d@y / d@x
}
S7::method(slope, list(segment, NULL)) <- function(x,y) {
  x@line@slope
}
S7::method(slope, list(line, NULL)) <- function(x,y) {
  x@slope
}

# midpoint----
#' @export
midpoint <- S7::new_generic("midpoint", c("x", "y"))
S7::method(midpoint, list(point, point)) <- function(x,y, position = .5) {
    x + ((y - x) * position)
}
S7::method(midpoint, list(segment, S7::class_missing)) <- function(x,y, position = .5) {
  midpoint(x@p1, x@p2, position = position)
}

# anchor----
#' @export
anchor <- S7::new_generic("anchor", dispatch_args = c("x", "position"))

## anchor circle----
S7::method(anchor, list(circle, angle)) <- function(x, position) {

  point(x = cos(position) * x@radius + x@center@x,
        y = sin(position) * x@radius + x@center@y)

}

S7::method(anchor, list(circle, S7::class_numeric)) <- function(x, position) {

  position <- angle(radian = position)

  anchor(x,position)

}

S7::method(anchor, list(circle, S7::class_character)) <- function(x, position) {
  position <- cardinalpoint(position)
  anchor(x,position)
}

## anchor ellipse----
S7::method(anchor, list(ellipse, angle)) <- function(x, position) {
  cost <- cos(position)
  sint <- sin(position)
  x <- sign(cost) * x@a * abs(cost) ^ (2 / x@m1)
  y <- sign(sint) * x@b * abs(sint) ^ (2 / x@m2)
  xy <- cbind(x = x, y = y)
  xy <- rotate2columnmatrix(xy, theta = x@theta)
  point(xy) + x@center

}
S7::method(anchor, list(ellipse, S7::class_numeric)) <- function(x, position) {
  position <- angle(radian = position)
  anchor(x,position)
}

S7::method(anchor, list(ellipse, S7::class_character)) <- function(x, position) {
  position <- cardinalpoint(position)
  anchor(x,position)
}


S7::method(anchor, list(line, S7::class_numeric)) <- function(x, position) {
  point(position, position * x@slope + x@intercept)
}

S7::method(anchor, list(S7::class_list, S7::class_missing)) <- function(x, position) {
  point_list(lapply(x,\(x) anchor(x, position)))
}

# inside----
#' is a point inside a shape ?
#'
#' @param x object
#' @param y object
#' @export
inside <- S7::new_generic("inside", c("x", "y"))
S7::method(inside, list(point, rectangle)) <- function(x,y) {
 insideTF <-  (x@x <= y@northeast@x &&
    x@x >= y@northwest@x &&
    x@y <= y@northeast@y &&
    x@y >= y@southeast@y) * 2 - 1

 if (length(intersection(x,y)) > 0) {
   insideTF <- 0
 }

 insideTF
}

S7::method(inside, list(point, circle)) <- function(x,y) {
  xc <- x - y@center
  -1 * sign(xc@distance - y@radius)
}

S7::method(inside, list(point, ellipse)) <- function(x,y) {
  rx <- rotate(x, -1*y@theta, origin = y@center)
  xc <- rx - y@center
  -1 * sign((xc@x ^ 2) / (y@a ^2) + (xc@y ^ 2) / (y@b ^2) - 1)
}

S7::method(inside, list(point_list, rectangle)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}

S7::method(inside, list(point_list, circle)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}

S7::method(inside, list(point_list, ellipse)) <- function(x,y) {
  sapply(x, \(p) inside(p, y))
}


# intersection----
#' intersection of 2 objects (e.g., lines)
#'
#' @param x object
#' @param y object
#' @export
intersection <- S7::new_generic("intersection", c("x", "y"))
S7::method(intersection, list(line, line)) <- function(x,y) {
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
S7::method(intersection, list(segment, segment)) <- function(x,y) {

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



S7::method(intersection, list(line, segment)) <- function(x,y) {
  intersection(intersection(x, y@line), y)
}

S7::method(intersection, list(segment, line)) <- function(x,y) {
  intersection(y, x)
}

S7::method(intersection, list(line, circle)) <- function(x,y) {
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

S7::method(intersection, list(circle, line)) <- function(x,y) {
  intersection(y,x)
}

S7::method(intersection, list(point, line)) <- function(x, y) {
  if (identical(TRUE, all.equal(0, y@a * x@x + y@b * x@y + y@c))) {
    x
  } else {
    list()
  }
}
S7::method(intersection, list(line, point)) <- function(x, y) {
  intersection(y, x)
}


S7::method(intersection, list(point, segment)) <- function(x, y) {
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

S7::method(intersection, list(segment, point)) <- function(x, y) {
  intersection(y, x)
}

S7::method(intersection, list(line, rectangle)) <- function(x, y) {
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
S7::method(intersection, list(rectangle, line)) <- function(x, y) {
  intersection(y, x)
}

S7::method(intersection, list(segment, rectangle)) <- function(x, y) {
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
S7::method(intersection, list(rectangle, segment)) <- function(x, y) {
  intersection(y, x)
}

S7::method(intersection, list(point, rectangle)) <- function(x, y) {
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

S7::method(intersection, list(rectangle, point)) <- function(x, y) {
  intersection(y, x)
}

# points(x = intersect_line_segment@x, y = intersect_line_segment@y)
S7::method(intersection, list(segment, ellipse)) <- function(x, y, sep = .01) {
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
    # print(d_change)
    for (ii in d_change$p) {
      s <- segment(midpoint(x, position = ii), midpoint(x, position = ii + sep))
      # print(distance(s))
      if (distance(s) < (.1 ^ 15)) {
        pp <- c(pp, s@p1)
      } else {
        # print(s)
        pp <- c(pp, intersection(s, y, sep = .5))
      }
    }
  }
  point_list(pp[])

}


# points(x = intersect_line_segment@x, y = intersect_line_segment@y)
S7::method(intersection, list(line, ellipse)) <- function(x, y) {
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
    s <- segment(point(x@x_intercept,ymin), point(x@x_intercept,ymax))
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

  intersection(s, y)

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

  while (current_angle < 362 && !(m1@distance < eps * 10) && delta_angle > .Machine$double.eps * 10) {
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
      if (d1@distance < eps * 10) {
        m1 <- point(0,0)
        line_ellipse_intersection <- s@p1
      }
      d2 <- s@p2 - intersect_line_segment
      if (d2@distance < eps * 10) {
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
#' @param e1 first point
#' @param e2 second point
#' @export
`%|-%` <- S7::new_generic("%|-%", c("e1", "e2"))
S7::method(`%|-%`, list(point, point)) <- function(e1,e2) {
  point(e1@x, e2@y)}

#' Find point perpendicular to 2 points
#'
#' @param e1 first point
#' @param e2 second point
#' @export
`%-|%` <- S7::new_generic("%-|%", c("e1", "e2"))
S7::method(`%-|%`, list(point, point)) <- function(e1,e2) {
  point(e2@x, e1@y)}



# Rotate ----

#' Rotate an object in 2 dimensions
#'
#' @param x object
#' @param theta angle
#' @param center length 2 vector  or point about which rotation occurs
#' @export
rotate <- S7::new_generic(name = "rotate", dispatch_args = c("x", "theta"), function(x, theta, ..., origin = point(0, 0)) {
  S7::S7_dispatch()
})
# Rotate Line
S7::method(rotate, list(line, angle)) <- function(x, theta, origin = point(0, 0)) {
  # https://math.stackexchange.com/a/2278909
  A <- x@a * cos(theta) + x@b * sin(theta)
  B <- x@b * cos(theta) + x@a * sin(theta)
  C <- (x@a - A) * origin@x + (x@b - B) * origin@y + x@c
  line(a = A, b = B, c = C)
}
S7::method(rotate, list(line, S7::class_numeric)) <- function(x, theta, origin = point(0, 0)) {
  rotate(x, angle(radian = theta), origin = origin)
}
# Rotate point
S7::method(rotate, list(point, angle)) <- function(x, theta, origin = point(0, 0)) {
  x0 <- x - origin
  xr <- rotate2columnmatrix(x0@xy, theta)
  dimnames(xr) <- list(NULL, NULL)
  point(xr[1, 1, drop = TRUE], xr[1, 2, drop = TRUE]) + origin
}
S7::method(rotate, list(point, S7::class_numeric)) <- function(x, theta, origin = point(0, 0)) {
  rotate(x, angle(radian = theta), origin = origin)
}

# Rotate segment
S7::method(rotate, list(segment, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  p1r <- rotate(x@p1, theta, origin = origin)
  p2r <- rotate(x@p2, theta, origin = origin)
  segment(p1r, p2r)
}

# Rotate centerpoint
S7::method(rotate, list(centerpoint, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  x_center_r <- rotate(x@center, theta, origin = origin)
  x@center <- x_center_r
  x
  }

# Rotate ellipse
S7::method(rotate, list(ellipse, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {
  x_center_r <- rotate(x@center, theta, origin = origin)
  ellipse(center = x_center_r, a = x@a, b = x@b, theta = x@theta + theta, n = x@n)
}
# Rotate rectangle
S7::method(rotate, list(rectangle, class_angle_or_numeric)) <- function(x, theta, origin = point(0, 0)) {

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
projection <- S7::new_generic("projection", c("x", "y"))
S7::method(projection, list(point, line)) <- function(x,y) {
ab <- y@a * y@a + y@b * y@b
xp <- (y@b * y@b * x@x - y@b * y@a * x@y - y@a * y@c) / ab
yp <- (y@a * y@a * x@y - y@a * y@b * x@x - y@b * y@c) / ab
point(xp, yp, style = x@style)
}

# S7::method(rotate, list(S7::class_numeric,
#                         S7::class_numeric)) <-
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


# Make label ----
make_label <- S7::new_generic("make_label", c("x", "y"))
S7::method(make_label, list(point, S7::class_character)) <- function(x,y) {
  label(x = x@x, y = x@y, label = y)
}


#
# library(grid)
# Plot ----
# S7::method(plot, circle) <- function(x) {
#   mycircle <- circleGrob(x = x@center@x, y = x@center@y, r = x@radius, gp = gpar(col = "gray", lty = 1, fill = "black") )
#   mycircle
# }
# plot(segment(point(1,2), point(4,5)))
# plot(circle(point(4,5), radius = 2))
# grid.draw(plot(circle(point(.5,.5), radius = .1)))

# S7::method(plot, xy) <- function(x, ...) {
#   if ("point" %in% class(x)) {
#     points(x@xy, pch = 16, ...)
#   } else {
#     polygon(x@xy, ...)
#   }
# }
#
# S7::method(plot, point) <- function(x, pch = 16, ...) {
#     points(x@xy, pch = pch, ...)
# }
#
# S7::method(plot, point_list) <- function(x, pch = 16, ...) {
#     points(x@xy, pch = pch, ...)
# }



# as.geom ----
#' Convert shapes to ggplot2 geoms
#'
#' @param x a shape
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass arguments to ggplot2::geom_point
#' @rdname as.geom
#' @export
as.geom <- S7::new_generic("as.geom", c("x"))
S7::method(as.geom, point_or_point_list) <- function(x, ...) {
   out <- striplist(c(rlang::list2(...),
            S7::convert(x@style, to = S7::class_list)),
            c("color", "pch", "size", "shape", "stroke","alpha"))
  rlang::inject(ggplot2::geom_point(data = as.data.frame(x@xy),
                                    ggplot2::aes(x = x, y = y),
                                    !!!out))
}

S7::method(as.geom, line) <- function(x, ...) {
  out <- striplist(c(rlang::list2(...),
                     S7::convert(x@style, to = S7::class_list)),
           c("color", "linewidth", "linetype", "alpha"))
  if (x@b == 0) {
    g <- rlang::inject(ggplot2::geom_vline(xintercept = x@x_intercept, !!!out))
  } else if (x@a == 0) {
    g <- rlang::inject(ggplot2::geom_hline(yintercept = x@intercept, !!!out))
  } else {
    g <- rlang::inject(ggplot2::geom_abline(slope = x@slope, intercept = x@intercept, !!!out))
  }
  g
}
S7::method(as.geom, segment) <- function(x, ...) {
  out <- striplist(c(rlang::list2(...),
                     S7::convert(x@style, to = S7::class_list)),
                   c("color", "linewidth", "linetype", "alpha"))
  rlang::inject(ggplot2::geom_line(data = as.data.frame(x@xy), aes(x = x, y = y), !!!out))
}

S7::method(as.geom, segment_list) <- function(x, ...) {
  out <- striplist(c(rlang::list2(...),
                     S7::convert(x@style, to = S7::class_list)),
                   c("color", "linewidth", "linetype", "alpha"))
  d_p1 <- as.data.frame(x@p1@xy)
  colnames(d_p1) <- c("p1_x", "p1_y")
  d_p2 <- as.data.frame(x@p2@xy)
  colnames(d_p1) <- c("p2_x", "p2_y")
  d <- cbind(d_p1, d_p2)
  rlang::inject(ggplot2::geom_segment(data = d, aes(x = p1_x, y = p1_y, xend = p2_x, yend = p2_y), !!!out))
}

S7::method(as.geom, circle) <- function(x, ...) {
  ggplot2::geom_polygon(data = as.data.frame(x@xy), aes(x = x, y = y), ...)
}

S7::method(as.geom, ellipse) <- function(x, ...) {
  ggplot2::geom_polygon(data = as.data.frame(x@xy), aes(x = x, y = y), ...)
}

S7::method(as.geom, rectangle) <- function(x, ...) {
  d <- data.frame(x = x@center@x, y = x@center@y, width = x@width, height = x@height)

  ggplot2::geom_tile(data = s, aes(x = x, y = y, width = width, height = height), ...)
}

S7::method(as.geom, label) <- function(
    x,
    ...,
    geom = c("richtext", "text", "label", "marquee"),
    label.margin = ggplot2::margin(t = 1,
                                   r = 1,
                                   b = 1,
                                   l = 1,
                                   unit = "pt"),
    label.padding = ggplot2::margin(t = 0,
                                   r = 0,
                                   b = 0,
                                   l = 0,
                                   unit = "pt"),
    label.color = NA,
    fill = NA) {
  out <- striplist(c(rlang::list2(...),
                     S7::convert(x@style, to = S7::class_list)),
                   c("color", "family", "fontface", "alpha", "size", "fill", "angle", "vjust", "hjust"))
  geom <- match.arg(geom)

  if (geom == "text") {
    geom2text <- ggplot2::geom_text
  } else if (geom == "label") {
    geom2text <- ggplot2::geom_label
  } else if (geom == "richtext") {
    geom2text <- purrr::partial(ggtext::geom_richtext, label.margin = label.margin, fill = fill, label.color = NA)
  } else if (geom == "richtext") {
    geom2text <- marquee::geom_marquee
  }
  rlang::inject(geom2text(
    data = data.frame(x = x@p@x,
                      y = x@p@y,
                      label = x@label),
    aes(x = x, y = y, label = label), !!!out))
}

# as.matrix ----

# Convert to matrix, data.frame
S7::method(as.matrix, xy) <- function(x) {
  x@xy
}
class(point(0,3))
S7::method(as.matrix, point_list) <- function(x) {
  do.call(rbind, lapply(x, function(p) p@xy))
}
S7::method(as.data.frame, point_list) <- function(x) {
  as.data.frame(as.matrix(x))
}

# S7::method(as.matrix, rectangle) <- function(x) {
#   x@xy
# }
# S7::method(as.matrix, circle) <- function(x) {
#   x@xy
# }
# S7::method(as.matrix, ellipse) <- function(x) {
#   x@xy
# }
#
# S7::method(as.matrix, segment) <- function(x) {
#   x@xy
# }

S7::method(as.matrix, segment_list) <- function(x) {
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
resect <- S7::new_generic("resect", c("x", "resect"))
S7::method(resect,
           list(segment,
                S7::class_numeric)) <- function(x,resect, resect_end = resect) {
  d <- x@p2 - x@p1
  x@p1 <- x@p1 + point(angle = d@angle, distance = resect)
  x@p2 <- x@p2 + point(angle = d@angle + angle(.5), distance = resect_end)
  x
  }
S7::method(
  resect,
  list(point, S7::class_numeric)) <- function(
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
equation <- S7::new_generic("equation", dispatch_args = "x")
S7::method(equation, line) <- function(x, type = c("y", "general", "parametric"), digits = 2) {
type <- match.arg(type)
eq <- ""
if (type == "y") {

  eq <- paste0("*y* =",
               rounder(x@slope, digits = digits),
               "*x*",
               rounder(x@intercept, digits = digits, add = TRUE))
} else if (type == "general") {
  eq <- paste0(rounder(x@a, digits = digits),
               "*x*",
               rounder(x@b, digits = digits, add = TRUE),
               "*y*",
               rounder(x@c, digits = digits, add = TRUE),
               " = 0")

} else if (type == "parametric") {
  eq <- paste0("*y* = ",
               rounder(x@slope, digits = digits),
               "*t*",
               rounder(x@intercept, digits = digits, add = TRUE),
               "<br>*x* = *t*")
}
eq
}

# convert ----
S7::method(convert, list(style, S7::class_list)) <- function(from, to) {
  l <- list()
  for (i in S7::prop_names(from)) {
    pp <- S7::prop(from, i)
    if (length(pp) > 0) {
      l[[i]] <- pp
    }
  }
  l
}
