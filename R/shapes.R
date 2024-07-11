# Centerpoint----
centerpoint <- new_class(
  name = "centerpoint",
  properties = list(center = new_property(
    class = point,
    default = point(0, 0, style = style_point())
  )),
  parent = xy
)

method(`+`, list(centerpoint, point)) <- function(e1, e2) {
  circle(e1@center + e2, e1@radius)
}

method(`-`, list(centerpoint, point)) <- function(e1, e2) {
  circle(e1@center - e2, e1@radius)
}

method(`+`, list(point, centerpoint)) <- function(e1, e2) {
  circle(e2@center + e1, e2@radius)
}

method(`-`, list(point, centerpoint)) <- function(e1, e2) {
  circle(e1 - e2@center, e2@radius)
}







# Circle----

#' circle class
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param n number of points in circle
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
circle <- new_class(
  name = "circle",
  parent = centerpoint,
  properties = list(
    radius = new_property(class = class_numeric, default = 1L),
    n = new_property(class = class_numeric, default = 360L),
    style = style_or_style_polygon,
    xy = new_property(
      getter = function(self) {
        theta <- seq(0, 2 * pi, length.out = self@n + 1)[-(self@n + 1)]
        x_p <- cos(theta) * self@radius + self@center@x
        y_p <- sin(theta) * self@radius + self@center@y
        cbind(x = x_p, y = y_p)
      })
  ),
  constructor = function(
    center = class_missing,
    radius = class_missing,
    n = class_missing,
    style = class_missing,
    ...) {
    c_style <- style_polygon() + style + style_polygon(...)

    new_object(centerpoint(center = center),
               radius = radius,
               n = n,
               style = c_style)
  }
)


method(`+`, list(ggplot_class, circle)) <- function(e1, e2) {
  e1 + as.geom(e2)
}



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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#'
#' @export
ellipse <- new_class(
  name = "ellipse",
  parent = centerpoint,
  properties = list(
    center = new_property(class = point, default = point(0, 0)),
    a = new_property(class_numeric, default = 1),
    b = new_property(class_numeric, default = 1),
    m1 = new_property(class_numeric, default = 2),
    m2 = new_property(class_numeric, default = 2),
    theta  = new_property(angle, default = angle(0)),
    n = new_property(class_numeric, default = 360),
    xy = new_property(
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
    style = new_property(class = style_or_style_polygon)
  ),
  constructor = function(center = class_missing,
                         a = class_missing,
                         b = class_missing,
                         m1 = class_missing,
                         m2 = class_missing,
                         theta = class_missing,
                         n = class_missing,
                         style = class_missing,
                         ...) {

    style <- style_polygon() + style + style_polygon(...)
    if (is.numeric(theta))
      theta <- angle(radian = theta)

    new_object(
      centerpoint(center = center),
      a = a,
      b = b,
      m1 = m1,
      m2 = m2,
      theta = theta,
      n = n,
      style = style
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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' rectangle(p, width = 2, height = 2)
#' @export
rectangle <- new_class(
  name = "rectangle",
  parent = centerpoint,
  properties = list(
    width = new_property(class = class_numeric, default = 1),
    height = new_property(class = class_numeric, default = 1),
    northeast = new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / 2) + self@center
      }
    ),
    northwest = new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / 2) + self@center
      }
    ),
    southwest = new_property(
      point,
      getter = function(self) {
        point(self@width / -2, self@height / -2) + self@center
      }
    ),
    southeast = new_property(
      point,
      getter = function(self) {
        point(self@width / 2, self@height / -2) + self@center
      }
    ),
    xy = new_property(
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
    style = new_property(
      class = style_or_style_polygon,
      default = style_polygon())
  ),
  constructor = function(center = class_missing,
                         width = class_missing,
                         height = class_missing,
                         northeast = class_missing,
                         northwest = class_missing,
                         southwest = class_missing,
                         southeast = class_missing,
                         style = class_missing,
                         ...) {
    style <- style_polygon(color = "black", fill = NA_character_, linewidth = .5) + style + style_polygon(...)
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
      r <- new_object(
        S7_object(),
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
      r <- new_object(
        S7_object(),
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
      r <- new_object(
        S7_object(),
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

method(`==`, list(rectangle, rectangle)) <- function(e1, e2) {
  e1@center == e2@center && e1@width == e2@width && e1@height == e2@height
}

method(`+`, list(ggplot_class, rectangle)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

# Arc----

#' arc class
#'
#' @param center point at center of the circle
#' @param radius distance between center and edge circle
#' @param start start angle
#' @param end end angle
#' @param theta interior angle (end - start)
#' @param n number of points in circle
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
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
arc <- new_class(
  name = "arc",
  parent = centerpoint,
  properties = list(
    radius = new_property(class = class_numeric, default = 1L),
    start = new_property(class = class_angle_or_numeric),
    end = new_property(class = class_angle_or_numeric),
    theta = new_property(class = class_angle_or_numeric,
                                      getter = function(self) {
                                        angle(turn = self@end@turn - self@start@turn)
                                      },
                                      setter = function(self, value) {
                                        self@end = self@start + value
                                        self
                                      }),
    n = new_property(class = class_numeric, default = 360L),
    xy = new_property(
      getter = function(self) {
        theta <- seq(self@start@turn, self@end@turn, length.out = self@n)
        # end <- self@end@radian
        # theta <- seq(self@start@radian,
        #             self@end@radian,
        #              length.out = self@n)
        x_p <- cospi(theta * 2) * self@radius + self@center@x
        y_p <- sinpi(theta * 2) * self@radius + self@center@y
        cbind(x = x_p, y = y_p)
      }
    ),
    style = new_property(class = style_or_style_line)
  ),
  constructor = function(center = class_missing,
                         radius = class_missing,
                         start = class_missing,
                         end = class_missing,
                         theta = class_missing,
                         n = class_missing,
                         style = class_missing,
                         ...) {

    style <- style_arrow(arrow_head = ggarrow::arrow_head_minimal()) + style + style(...)

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
    if (is.numeric(theta)) theta <- angle(radian = theta)

    new_object(.parent =
      centerpoint(center = center),
      radius = radius,
      start = start,
      end = end,
      n = n,
      style = style
    )
  }

)

method(`+`, list(ggplot_class, arc)) <- function(e1, e2) {
  e1 + as.geom(e2)
}
