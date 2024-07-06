# Angle ----
#' angle class
#'
#' @param radian radians
#' @param degree degrees
#' @param turn proportion of full turns of a circle (1 turn = 2 * pi radians)
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
    radian = S7::new_property(
      getter = function(self)
        self@turn * 2 * pi
    ),
    degree = S7::new_property(
      getter = function(self)
        self@turn * 360
    ),
    turn = S7::new_property(
      name = "turn",
      class = S7::class_numeric,
      default = 0
    ),
    gradian = S7::new_property(
      getter = function(self)
        self@turn * 400
    )
  ),
  constructor = function(
    radian = S7::class_missing,
    degree = S7::class_missing,
    turn = S7::class_missing,
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
  angle(turn = e1@turn + e2@turn)
}
S7::method(`-`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn - e2@turn)
}
S7::method(`*`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn * e2@turn)
}
S7::method(`/`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn / e2@turn)
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
  angle(turn = e1@turn * e2)
}

S7::method(`*`, list(S7::class_numeric, angle)) <- function(e1, e2) {
  angle(turn = e1 * e2@turn)
}

S7::method(`/`, list(angle, S7::class_numeric)) <- function(e1, e2) {
  angle(turn = e1@turn / e2)
}

S7::method(`==`, list(angle, angle)) <- function(e1, e2) {
  abs(e1@turn - e2@turn) <= .Machine$double.eps
}


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

S7::method(cos, angle_list) <- function(x) {
  cospi(x@turn * 2)
}
S7::method(sin, angle_list) <- function(x) {
  sinpi(x@turn * 2)
}
S7::method(tan, angle_list) <- function(x) {
  tanpi(x@turn * 2)
}

class_angle_or_numeric <- S7::new_union(angle, S7::class_numeric)

#' degree class
#'
#' @rdname angle
#' @export
degree <- S7::new_class(
  name = "degree",
  parent = angle,
  constructor = function(degree) {
    if (length(degree) > 1) {
      turn <- degree / 360
      return(angle_list(lapply(turn, \(x) angle(turn = x))))
    } else {
      S7::new_object(angle(degree = degree))
    }
  }
)

#' radian class
#'
#' @rdname angle
#' @export
radian <- S7::new_class(
  name = "radian",
  parent = angle,
  constructor = function(radian) {
    if (length(radian) > 1) {
      turn <- radian / (2 * pi)
      return(angle_list(lapply(turn, \(x) angle(turn = x))))
    }
    else {
      S7::new_object(angle(radian = radian))
    }
  }
)

#' turn class
#'
#' @rdname angle
#' @export
turn <- S7::new_class(
  name = "turn",
  parent = angle,
  constructor = function(turn) {
    if (length(turn) > 1) {
      return(angle_list(lapply(turn, \(x) angle(turn = x))))
    }
    else {
      S7::new_object(angle(turn = turn))
    }
  }
)

#' gradian class
#'
#' @rdname angle
#' @export
gradian <- S7::new_class(
  name = "turn",
  parent = angle,
  constructor = function(gradian) {
    if (length(gradian) > 1) {
      turn <- gradian / 400
      return(angle_list(lapply(turn, \(x) angle(turn = x))))
    } else {
      S7::new_object(angle(gradian = gradian))
    }
  }
)



