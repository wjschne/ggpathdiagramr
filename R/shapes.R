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
#' @param n number of points in circle (default = 360)
#' @param xy 2-column matrix of points
#' @param style a style object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @examples
#' # specify center point and radius
#' p <- point(0,0)
#' circle(p, radius = 6)
#' @export
circle <- new_class(
  name = "circle",
  parent = centerpoint,
  properties = list(
    radius = new_property(class = class_numeric, default = 1L),
    n = new_property(class = class_numeric),
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
    if (length(n) == 0) n <- 360L

    d <- tibble::tibble(center = c(center),
                        radius = c(radius),
                        n = c(n),
                        style = c(c_style))
    if (nrow(d) > 1) {
      return(circle_list(purrr::pmap(d, circle)))
    }



    new_object(centerpoint(center = center),
               radius = radius,
               n = n,
               style = c_style)
  }
)


# Circle list ----
circle_list <- new_class(
  name = "circle_list",
  parent = shape_list,
  properties = list(
    center = new_property(
      point_list,
      getter = function(self) {
        point_list(sapply(self, \(x) x@center))
      }
    ),
    radius = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@radius)
      }
    ),
    n = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@n)
      }
    ),
    style = new_property(
      class_list,
      getter = function(self) {
        style_list(lapply(self, \(x) x@style))
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "circle")
  }
)


circle_or_circle_list <- new_union(circle, circle_list)

method(get_tibble, circle) <- function(x) {
  xs <- c(list(x0 = x@center@x,
               y0 = x@center@y,
               r = x@radius,
               n = x@n),
          get_non_empty_props(x@style))

  rlang::inject(tibble::tibble(!!!xs))
}


method(get_tibble_defaults, circle_list) <- function(x) {
  sp <- style_polygon(
    alpha = replace_na(as.double(ggforce::GeomCircle$default_aes$alpha), 1),
    color = replace_na(ggforce::GeomCircle$default_aes$colour, "black"),
    fill = replace_na(ggforce::GeomCircle$default_aes$fill, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggforce::GeomCircle$default_aes$linewidth, 0.5),
    linetype = replace_na(ggforce::GeomCircle$default_aes$default_aes$linetype, 1)
  )
  get_tibble_defaults_helper(x, sp,required_aes = c("x0", "y0", "r", "n"))
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
#' arc(p, radius = 6, start = degree(30), end = degree(90))
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

    style <- style_arrow() + style + style(...)

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

# pp <- new_class("pp",
#   parent = class_data.frame,
#   properties = list(
#     x = new_property(
#       class = class_numeric,
#       getter = function(self) {
#         S7_data(self)$x
#       },
#       setter = function(self, value) {
#         S7_data(self)$x <- value
#         self
#       }
#     ),
#     y = new_property(
#       class = class_numeric,
#       getter = function(self) {
#         S7_data(self)$y
#       },
#       setter = function(self, value) {
#         S7_data(self)$y <- value
#       }
#     )
#   ),
#   validator = function(self) {
#     if (!all(c("x","y") %in% names(S7_data(self)) )) {
#       stop("x and y are required aesthetics.")
#     }

#   },
#   constructor = function(x = class_numeric, y = class_numeric) {
#     new_object(tibble::tibble(x = x, y = y))
#   })


# # x@x <- 2:4
# # x@x <- NULL
# # x$x
# # View(x)
# # x@x <- 2:4
# # S7_class(x@x)

# angle_names <- c("degree", "radian", "gradian", "turn")
# angle_multiplier <- c(360, 2 * pi, 400, 1)
# pr <- purrr::map2(angle_names, angle_multiplier, \(x, multiplier) {
#   new_property(
#      class = class_numeric,
#      getter = function(self) {
#        x <- S7_data(self)
#        denominator <- ifelse(x < 0, -1, 1)
#        (x %% denominator) * eval(multiplier)
#      },
#      setter = function(self, value) {
#        S7_data(self) <- value / eval(multiplier)
#        self
#      },
#      name = eval(x)
#    )
#  })
# names(pr) <- angle_names



# angle <- new_class("angle",
#   parent = class_double,
#   properties =  pr,
#   constructor = function(
#     degree= class_missing,
#     radian = class_missing,
#     gradian = class_missing,
#     turn = class_missing) {

#     if (length(turn) > 0) {
#       turn <- as.double(turn)
#     } else if (length(radian) > 0) {
#       turn <- radian / (2 * pi )
#     } else if (length(degree)) {
#       turn <- degree / 360
#     } else if (length(gradian)) {
#       turn <- gradian / 400
#     }
#     new_object(turn)
#   })

# purrr::walk(list(`+`, `-`, `*`, `/`), \(.f) {
#   method(.f, list(angle, angle)) <- function(e1, e2) {
#     angle(turn = .f(S7_data(e1), S7_data(e2)))
#   }
#   method(.f, list(angle, class_numeric)) <- function(e1, e2) {
#     angle(turn = .f(S7_data(e1), e2))
#   }
#   method(.f, list(class_numeric, angle)) <- function(e1, e2) {
#     angle(turn = .f(e1, S7_data(e2)))
#   }
# })

# method(cos, angle) <- function(x) {
#   cospi(x@turn * 2)
# }
# method(sin, angle) <- function(x) {
#   sinpi(x@turn * 2)
# }
# method(tan, angle) <- function(x) {
#   tanpi(x@turn * 2)
# }


# # Angle wrappers ----
# #' degree class
# #'
# #' @rdname angle
# #' @export
# degree <- new_class(
#   name = "degree",
#   parent = angle,
#   constructor = function(degree = class_missing) {
#       if (S7_inherits(degree, angle)) degree <- degree@degree
#       new_object(angle(degree = degree))
#   }
# )

# #' radian class
# #'
# #' @rdname angle
# #' @export
# radian <- new_class(
#   name = "radian",
#   parent = angle,
#   constructor = function(radian = class_missing) {
#       if (S7_inherits(radian, angle)) radian <- radian@radian
#       new_object(angle(radian = radian))
#   }
# )

# #' turn class
# #'
# #' @rdname angle
# #' @export
# turn <- new_class(
#   name = "turn",
#   parent = angle,
#   constructor = function(turn = class_missing) {

#       if (S7_inherits(turn, angle)) turn <- turn@turn
#       new_object(angle(turn = turn))

#   }
# )


# #' gradian class
# #'
# #' @rdname angle
# #' @export
# gradian <- new_class(
#   name = "gradian",
#   parent = angle,
#   constructor = function(gradian = class_missing) {
#       if (S7_inherits(gradian, angle)) gradian <- gradian@gradian
#       new_object(angle(gradian = gradian))
#   }
# )

#  method(`[`, angle) <- function(x, i) {
#    angle(turn = S7_data(x)[i])
#  }

#  method(`[[`, angle) <- function(x, i) {
#   S7_data(x)[i]
# }



#  purrr::walk(list(turn, radian, gradian, degree), \(.a) {
#   method(`[`, .a) <- function(x, i) {
#     .a(angle(turn = S7_data(x)[i]))
#   }

#   method(`[[`, .a) <- function(x, i) {
#     prop(x, name = .a@name)[i]
#     S7::prop(x, )
#   }


#  })


