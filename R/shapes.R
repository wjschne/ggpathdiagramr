# Centerpoint----
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







# Circle----

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param n number of points in circle
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
    ),
    style = S7::new_property(class = style, default = style(size = 2, shape = 16))
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
#' @param style a style object
#' @param <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
    ),
    style = S7::new_property(class = style, default = style(size = 2, shape = 16))
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
#' @param style a style object
#' @param <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
    northeast = S7::new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / 2) + self@center
      }
    ),
    northwest = S7::new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / 2) + self@center
      }
    ),
    southwest = S7::new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / -2) + self@center
      }
    ),
    southeast = S7::new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / -2) + self@center
      }
    ),
    xy = S7::new_property(
      getter = function(self) {
        p <- as.matrix(point_list(
          c(
            self@northeast,
            self@northwest,
            self@southwest,
            self@southeast
          )
        ))
        rownames(p) <- c("northeast", "northwest", "southwest", "southeast")
        p
      }
    ),
    style = S7::new_property(class = style, default = style(size = 2, shape = 16))
  ),
  constructor = function(center = S7::class_missing,
                         width = S7::class_missing,
                         height = S7::class_missing,
                         northeast = S7::class_missing,
                         northwest = S7::class_missing,
                         southwest = S7::class_missing,
                         southeast = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    style <- style + style(...)
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

    if (length(center) == 1 &&
        length(width) == 1 && length(height) == 1) {
      r <- S7::new_object(
        S7::S7_object(),
        center = center,
        width = width,
        height = height,
        style = style
      )
    } else if (length(center) == 1 &&
               (hasnorth || hassouth) && (haswest || haseast)) {
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
      r <- S7::new_object(
        S7::S7_object(),
        center = center,
        width = width,
        height = height,
        style = style
      )
    } else if (length(width) == 1 &&
               length(height) == 1 &&
               (hasnorth || hassouth) && (haswest || haseast)) {
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
      r <- S7::new_object(
        S7::S7_object(),
        center = point(c.x, c.y),
        width = width,
        height = height,
        style = style
      )
    } else {
      stop("There is not enough information to make a rectangle.")
    }
    r
  }
)

S7::method(`==`, list(rectangle, rectangle)) <- function(e1, e2) {
  e1@center == e2@center && e1@width == e2@width && e1@height == e2@height
}

S7::method(`+`, list(ggplot_class, rectangle)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

# Arc----

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param n number of points in circle
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
arc <- S7::new_class(
  name = "arc",
  parent = centerpoint,
  properties = list(
    radius = S7::new_property(class = S7::class_numeric, default = 1L),
    start = S7::new_property(class = class_angle_or_numeric),
    end = S7::new_property(class = class_angle_or_numeric),
    theta = S7::new_property(class = class_angle_or_numeric,
                                      getter = function(self) {
                                        self@end - self@start
                                      },
                                      setter = function(self, value) {
                                        self@end = self@start + value
                                        self
                                      }),
    n = S7::new_property(class = S7::class_numeric, default = 360L),
    xy = S7::new_property(
      getter = function(self) {
        if (self@start@radian > self@end@radian) {
          end <- self@end@radian + 2 * pi
        } else {
          end <- self@end@radian
        }
        theta <- seq(self@start@radian,
                     end,
                     length.out = self@n)
        x_p <- cos(theta) * self@radius + self@center@x
        y_p <- sin(theta) * self@radius + self@center@y
        cbind(x = x_p, y = y_p)
      }
    ),
    style = S7::new_property(class = style, default = style())
  ),
  constructor = function(center = S7::class_missing,
                         radius = S7::class_missing,
                         start = S7::class_missing,
                         end = S7::class_missing,
                         theta = S7::class_missing,
                         n = S7::class_missing,
                         style = style(),
                         ...) {

    style <- style + style(...)

    if (length(start) > 0 && length(end) == 0 && length(theta) > 0) {
      end <- start + theta
    }

    if (length(start) == 0 && length(end) > 0 && length(theta) > 0) {
      start <- end - theta
    }

    if (length(start) == 0) {
      start <- angle(0)
    }

    if ( length(end) == 0 && length(theta) > 0) {
      end <- theta
    }

    if (length(end) == 0) {
      end <- angle(0)
    }

    if (is.numeric(start)) start <- angle(radian = start)
    if (is.numeric(end)) end <- angle(radian = end)
    if (is.numeric(theta)) end <- angle(radian = theta)

    S7::new_object(.parent =
      centerpoint(center = center),
      radius = radius,
      start = start,
      end = end,
      n = n,
      style = style
    )
  }

)

S7::method(`+`, list(ggplot_class, arc)) <- function(e1, e2) {
  e1 + as.geom(e2)
}
