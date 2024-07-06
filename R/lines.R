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
    style <- style + style(...)

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
    allsameclass(self, "line")
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
  name = "segment",
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
    style <- style + style(...)
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
#' segment_list
#'
#' Make lists of segments
#' @param .data list of segments
#' @param p1 point_list of first points (p1)
#' @param p2 point_list of second points (p2)
#' @param line line_list of lines associated with segments
#' @export
#' @examples
#' s1 <- segment(point(0,0), point(1,1))
#' s2 <- segment(point(1,1), point(0,1))
#' segment_list(c(s1,s2))
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


