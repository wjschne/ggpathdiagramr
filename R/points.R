
# get pointxy----
#' Get x and y values for making points
#'
#' @param x object
#' @param y numeric
#' @param theta angle or numeric
#' @param r numeric
#' @keywords internal
make_point <- new_generic("make_point", c("x", "y", "theta", "r"))
method(generic = make_point, list(class_numeric, class_numeric, class_missing, class_missing)) <- function(x, y, theta, r) {
         list(x = x, y = y)
       }

method(make_point,list(x = class_missing,y = class_missing,theta = angle_or_angle_list, r = class_numeric)) <- function(x, y, theta, r) {
         list(x = r * cos(theta),
              y = r * sin(theta))
       }

method(make_point,list(x = class_missing,y = class_missing,theta = class_numeric,r = class_numeric)) <- function(x, y, theta, r) {
         make_point(r = r, theta = radian(theta))
       }

method(make_point,list(x = class_missing,y = class_missing,theta = class_missing,r = class_missing)) <- function(x, y, theta, r) {
         list(x = 0, y = 0)
       }


method(make_point,list(x = class_numeric,y = class_missing,theta = class_missing,r = class_missing)) <- function(x, y, theta, r) {
         if ("matrix" %in% class(x)) {
           if (ncol(x) == 2) {
             return(list(x = y <- x[,2, drop = TRUE],
                         y = x[,1, drop = TRUE]))

           } else {
             stop(paste("A point list can be created with a 2-column matrix, but this matrix has", ncol(x), "columns."))
           }
         }
         list(x = x,
              y = 0)
       }

method(make_point,list(x = class_missing,y = class_numeric,theta = class_missing,r = class_missing)) <- function(x, y, theta, r) {
         list(x = 0,
              y = y)
}

method(make_point,list(x = class_missing,y = class_missing, theta = angle_or_angle_list,r = class_missing)) <- function(x, y, theta, r) {
  make_point(r = 1, theta = radian(theta))
}

method(make_point,list(x = class_missing,y = class_missing, theta = class_missing,r = class_numeric)) <- function(x, y, theta, r) {
  make_point(r = r, theta = radian(0))
}


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
        radian(radian = atan2(self@y, self@x))
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
    style = new_property(class = style_or_style_point)
  ),
  validator = function(self) {
    if (length(self@x) > 1) {
      stop("x must not have more than 1 element.")
    }
    if (length(self@y) > 1) {
      stop("y must not have more than 1 element.")
    }

  },
  # constructor = make_point
  constructor = function(x = class_missing,
                         y = class_missing,
                         theta = class_missing,
                         r = class_missing,
                         alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                         style = point@properties$style$default,
                         ...) {


    p_style <- style(
      alpha = alpha,
      color = color,
      fill = fill,
      shape = shape,
      size = size,
      stroke = stroke) +
      style +
      style(...)

    # slot x might be a 2-column matrix
    if ("matrix" %in% class(x)) {
      if (ncol(x) == 2) {
        y <- x[,2, drop = TRUE]
        x <- x[,1, drop = TRUE]
      } else {
        stop(paste("A point list can be created with a 2-column matrix, but this matrix has", ncol(x), "columns."))
      }
    }

    # if (length(x) == 0 &&
    #     length(y) == 0 &&
    #     length(theta) > 0 &&
    #     length(r) > 0) {
    #   if (is.numeric(theta)) theta <- angle(radian = theta)
    #   x = r * cos(theta)
    #   y = r * sin(theta)
    # }
    # if (length(x) == 0) {
    #   x <- 0L
    # }
    #
    # if (length(y) == 0) {
    #   y <- 0L
    # }

    non_empty_list <- get_non_empty_list(list(x = x, y = y, theta = theta, r = r))


    l <- rlang::inject(make_point(!!!non_empty_list))


    d <- tibble::tibble(x = l$x, y = l$y, style = c(p_style))
    if (nrow(d) > 1) {
      return(point_list(purrr::pmap(d, point)))
    }

    new_object(S7_object(), x = l$x, y = l$y, style = p_style)
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
  # The point function is needed because e1 might have length > 1.
  point(x = e1 + e2@x,
        y = e1 + e2@y,
        style = e2@style)
}
method(`-`, list(class_numeric, point)) <- function(e1, e2) {
  point(x = e1 - e2@x,
    y = e1 - e2@y,
    style = e2@style)
}
method(`*`, list(class_numeric, point)) <- function(e1, e2) {
  point(x = e1 * e2@x,
    y = e1 * e2@y,
    style = e2@style)
}
method(`/`, list(class_numeric, point)) <- function(e1, e2) {
  point(x = e1 / e2@x,
    y = e1 / e2@y,
    style = e2@style)
}
method(`+`, list(point, class_numeric)) <- function(e1, e2) {
  e2 + e1
}
method(`-`, list(point, class_numeric)) <- function(e1, e2) {
  point(x = e1@x - e2,
    y = e1@y - e2,
    style = e1@style)
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

method(polar2just, point) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  polar2just(x@theta@radian, multiplier, axis)
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
  parent = shape_list,
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

# point list addition ----

method(`+`, list(point_list,  point_list)) <- function(e1, e2) {
  point_list(purrr::map2(e1, e2, `+`))
}

method(`+`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x + e1))
}
method(`+`, list(point_list, point)) <- function(e1, e2) {
  e2 + e1
}

# point list subtraction ----

method(`-`, list(point_list,  point_list)) <- function(e1, e2) {
  point_list(purrr::map2(e1, e2, `-`))
}

method(`-`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x - e1))
}
method(`-`, list(point_list, point)) <- function(e1, e2) {
  point_list(lapply(e1, \(x) e1 - x))
}

# point list multiplication----

method(`*`, list(point_list,  point_list)) <- function(e1, e2) {
  point_list(purrr::map2(e1, e2, `*`))
}

method(`*`, list(point_list,  point)) <- function(e1, e2) {
  point_list(lapply(e1, \(x) x * e2))
}

method(`*`, list(point, point_list)) <- function(e1, e2) {
  point_list(lapply(e2, \(x) x * e1))
}

# point_list

point_or_point_list <- new_union(point, point_list)


method(get_tibble, point) <- function(x) {
  xs <- c(list(x = x@x,
             y = x@y),
          get_non_empty_props(x@style))

  rlang::inject(tibble::tibble(!!!xs))
}



method(get_tibble_defaults, point_list) <- function(x) {
  sp <- style_point(
    alpha = replace_na(ggplot2::GeomPoint$default_aes$alpha, 1),
    color = replace_na(ggplot2::GeomPoint$default_aes$colour, "black"),
    fill = replace_na(ggplot2::GeomPoint$default_aes$fill, "black"),
    shape = replace_na(ggplot2::GeomPoint$default_aes$shape, 19),
    size = replace_na(ggplot2::GeomPoint$default_aes$size, 5),
    stroke = replace_na(ggplot2::GeomPoint$default_aes$stroke, 0.5)
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y"))
}

