
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
  abs(y@intercept + y@slope * x@x - x@y) / sqrt(1 + y@slope ^ 2)
}
S7::method(distance, list(line, point)) <- function(x,y) {
  distance(y, x)
}
S7::method(distance, list(segment, S7::class_missing)) <- function(x,y) {
  distance(x@p1, x@p2)
}
S7::method(distance, list(circle, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x@center)
  if (!center & !identical(x,y)) {
    px <- anchor(x, angle(d), radians = TRUE)
    py <- anchor(y, pi + angle(d), radians = TRUE)
    d <- py - px
  }
  distance(d)
}
S7::method(distance, list(point, circle)) <- function(x,y, center = FALSE) {
  d <- (y@center - x)
  if (!center) {
    py <- anchor(y, pi + angle(d), radians = TRUE)
    d <- py - x
  }
  distance(d)
}
S7::method(distance, list(circle, point)) <- function(x,y, center = FALSE) {
  distance(y, x)
}



# angle----
#' Compute the angle of a line, segment or intersection of two lines
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
#'
angle <- S7::new_generic("angle", c("x", 'y'))
S7::method(angle, list(point, S7::class_missing)) <- function(x, y) {
  atan2(x@y, x@x)
}
S7::method(angle, list(point, point)) <- function(x, y) {
  d <- y - x
  atan2(d@y, d@x)
}

S7::method(angle, list(segment, S7::class_missing)) <-
  function(x, y) {
    x@line@angle
  }
S7::method(angle, list(line, S7::class_missing)) <- function(x, y) {
  x@angle
}
S7::method(angle, list(line, line)) <- function(x, y) {
  y@angle - x@angle

}
S7::method(angle, list(segment, segment)) <- function(x, y) {
  if (length(intersection(x,y)) > 0) {
    y@line@angle - x@line@angle
  } else NA_real_
}
S7::method(angle, list(line, segment)) <- function(x, y) {
  if (length(intersection(x,y)) > 0) {
    y@angle - x@line@angle
  } else NA_real_
}
S7::method(angle, list(segment, line)) <- function(x, y) {
  if (length(intersection(x,y)) > 0) {
    y@line@angle - x@angle
  } else NA_real_
}


# slope----
#' @export
slope <- S7::new_generic("slope", c("x", 'y'))
S7::method(slope, list(point, point)) <- function(x,y) {
  d <- y - x
  d@y / d@x
}
S7::method(slope, list(segment, NULL)) <- function(x,y) {
  x@slope
}
S7::method(slope, list(line, NULL)) <- function(x,y) {
  x@slope
}

# midpoint----
#' @export
midpoint <- S7::new_generic("midpoint", c("x", "y"))
S7::method(midpoint, list(point, point)) <- function(x,y, position = .5) {
  theta <- angle(x, y)
  d <- distance(x,y)
  point(x@x + cos(theta) * d * position, x@y + sin(theta) * d * position)
}
S7::method(midpoint, list(segment, S7::class_missing)) <- function(x,y, position = .5) {
  theta <- x@angle
  d <- x@distance
  point(x@p1@x + cos(theta) * d * position, x@p1@y + sin(theta) * d * position)
}

# anchor----
#' @export
anchor <- S7::new_generic("anchor", dispatch_args = "x")
S7::method(anchor, circle) <- function(x, position, radians = FALSE) {
  namedpositions <- c(
    east = 0,
    northeast = 45,
    north = 90,
    northwest = 135,
    west = 180,
    southwest = 225,
    south = 270,
    southeast = 315
  )

  if (position %in% names(namedpositions))
    position <- unname(namedpositions[position])
  if (radians)
    theta <- position
  else
    theta <- pi * position / 180
  point(x = cos(theta) * x@radius + x@center@x,
        y = sin(theta) * x@radius + x@center@y)

}

S7::method(anchor, ellipse) <- function(x, position, radians = FALSE) {
  namedpositions <- c(east = 0, northeast = 45,
                      north = 90, northwest = 135,
                      west = 180, southwest = 225,
                      south = 270, southeast = 315)

  if (position %in% names(namedpositions)) position <- unname(namedpositions[position])
  if (radians) theta <- position else theta <- pi * position / 180

  xy <- cbind(
    x = cos(theta) * x@a + x@center@x,
    y = sin(theta) * x@b + x@center@y
  )
  xy <- rotate2columnmatrix(xy, theta = x@theta)
  point(xy) + x@center

}
S7::method(anchor, line) <- function(x, position) {
  point(position, position * x@slope + x@intercept)
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
  A <- t(rbind((x@p2 - x@p1)@xy,
               (y@p1 - y@p2)@xy))
  B <- t((y@p1 - x@p1)@xy)
  C <- solve(A, B)
  p <- list()
  if (all(C >= 0 & C <= 1)) {
    p <- intersection(x@line, y@line)
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
  cprime <- x@c - x@a * y@center@x - x@b * y@center@y
  D <- (y@radius ^ 2) * (x@a ^ 2 + x@b ^ 2) - cprime ^ 2
  ab <- x@a ^ 2 + x@b ^ 2
  if (D < 0) {
    p <- list()
  } else if (D == 0) {
      p <- point(x@a * cprime / ab, x@b * cprime / ab) + y@center
  } else {
    p1 <- point((x@a * cprime - x@b * sqrt(D)) / ab,
                (x@b * cprime + x@a * sqrt(D)) / ab) + y@center
    p2 <- point((x@a * cprime + x@b * sqrt(D)) / ab,
                (x@b * cprime - x@a * sqrt(D)) / ab) + y@center


    p <- c(p1, p2)
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

# Perpendicular

#' Find point perpendicular to 2 points
#'
#' @param e1 first point
#' @param e2 second point
#' @export
`%|-%` <- S7::new_generic("%|-%", c("e1", "e2"))
S7::method(`%|-%`, list(point, point)) <- function(e1,e2) {
  point(e1@x, e2@y)}

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
# rotate2d <- S7::new_generic(name = "rotate", dispatch_args = c("x", "theta"), function(x, theta, ...) {
#   S7::S7_dispatch()
# })
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


# Make Node ----
make_node <- S7::new_generic("make_node", c("x", "y"))
S7::method(make_node, list(point, S7::class_character)) <- function(x,y) {
  node(x = x@x, y = x@y, label = y)
}

library(grid)
# Plot ----
S7::method(plot, circle) <- function(x) {
  mycircle <- circleGrob(x = x@center@x, y = x@center@y, r = x@radius, gp = gpar(col = "gray", lty = 1, fill = "black") )
  mycircle
}
# plot(segment(point(1,2), point(4,5)))
plot(circle(point(4,5), radius = 2))
grid.draw(plot(circle(point(.5,.5), radius = .1)))
