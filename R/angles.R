# Angle ----
#' angle class
#'
#' Creates an angle in the metric of radians, degrees, turns, and gradians.
#'
#' Angles turns can be any real number, but degrees are displayed as values between -360 and +360,
#' radians are between -2pi and +2pi, and gradians are between -400 and +400.
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
#' # Convenience wrappers
#' degree(90)
#' radian(.5 * pi)
#' gradian(100)
#' turn(.25)
#'
#' # Operations
#' angle(degree = 30) + angle(degree = 20)
#' angle(degree = 350) + angle(degree = 20)
#' angle(degree = 30) - angle(degree = 20)
#' angle(degree = 30) - angle(degree = 50)
#'
#' 2 * angle(degree = 30)
#' angle(degree = 30) / 3
#'
#' angle(degree = 30) + pi # added or subtracted numbers are radians
#' radian(1) + 1 # added or subtracted numbers are radians
#' degree(30) + 180 # added or subtracted numbers are degrees
#' turn(.25) + .25 # added or subtracted numbers are turns
#' gradian(50) + 50 # added or subtracted numbers are turns
#'
#' Trigonometric functions work as normal
#' x <- angle(degree = 180)
#' sin(x)
#' cos(x)
#' tan(x)
angle <- new_class(
  name = "angle",
  properties = list(
    radian = new_property(
      getter = function(self) {
        (self@turn %% ifelse(self@turn < 0, -1, 1)) * 2 * pi
        }
    ),
    degree = new_property(
      getter = function(self) {(self@turn %% ifelse(self@turn < 0, -1, 1)) * 360}
    ),
    turn = new_property(
      name = "turn",
      class = class_numeric
    ),
    gradian = new_property(
      getter = function(self) {(self@turn %% ifelse(self@turn < 0, -1, 1)) * 400}
    )
  ),
  constructor = function(
    radian = class_missing,
    degree = class_missing,
    turn = class_missing,
    gradian = class_missing) {
    if (length(turn) > 0) {
      x <- turn
    } else if (length(radian) > 0) {
      x <- radian / (2 * pi)
    } else if (length(degree) > 0) {
      x <- degree / 360
    } else if (length(gradian) > 0) {
      x <- gradian / 400
    } else {
      x <- 0
    }

    if (length(x) > 1) {
      return(angle_list(lapply(x, \(x) angle(turn = x))))
    }



    new_object(S7_object(), turn = x)


  }
)

num2turn <- function(x, object) {
  e2_class <- match.arg(arg = class(object)[1],
  choices = c("degree", "radian", "gradian", "turn", "angle"))
  denominator <- switch(e2_class, degree = 360, radian = 2 * pi, gradian = 400, turn = 1, angle = 2 * pi)
  x / denominator
}


method(cos, angle) <- function(x) {
  cospi(x@turn * 2)
}
method(sin, angle) <- function(x) {
  sinpi(x@turn * 2)
}
method(tan, angle) <- function(x) {
  tanpi(x@turn * 2)
}






class_angle_or_numeric <- new_union(class_numeric, angle)
class_angle_or_character <- new_union(class_character, angle)

# Angle list----
#' angle list class
#'
#' @param .data vector of angles
#' @rdname angle
#' @export
#' @examples
#' # Two ways to make an angle_list
#' angle(c(0,1))
#' angle_list(c(angle(0), angle(1)))
angle_list <- new_class(
  name = "angle_list",
  parent = shape_list,
  properties = list(
    turn = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@turn)
      }
    ),
    radian = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@radian)
      }
    ),
    degree = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@degree)
      }
    ),
    gradian = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@gradian)
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "angle")
  }
)

angle_or_angle_list <- new_union(angle, angle_list)

method(cos, angle_list) <- function(x) {
  cospi(x@turn * 2)
}
method(sin, angle_list) <- function(x) {
  sinpi(x@turn * 2)
}
method(tan, angle_list) <- function(x) {
  tanpi(x@turn * 2)
}

method(`+`, list(angle, angle)) <- function(e1, e2) {
  # This preserves e2's type
  e2@turn <- e1@turn + e2@turn
  e2
}



# Angle wrappers ----
#' degree class
#'
#' @rdname angle
#' @export
degree <- new_class(
  name = "degree",
  parent = angle,
  constructor = function(degree = class_missing) {
    if (length(degree) > 1) {
      return(angle_list(lapply(degree, \(x) degree(x))))
    } else {
      if (S7_inherits(degree, angle)) degree <- degree@degree
      new_object(angle(degree = degree))
    }
  }
)

#' radian class
#'
#' @rdname angle
#' @export
radian <- new_class(
  name = "radian",
  parent = angle,
  constructor = function(radian = class_missing) {
    if (length(radian) > 1) {
      return(angle_list(lapply(radian, \(x) radian(x))))
    }
    else {
      if (S7_inherits(radian, angle)) radian <- radian@radian
      new_object(angle(radian = radian))
    }
  }
)

#' turn class
#'
#' @rdname angle
#' @export
turn <- new_class(
  name = "turn",
  parent = angle,
  constructor = function(turn = class_missing) {
    if (length(turn) > 1) {
      return(angle_list(lapply(turn, \(x) turn(x))))
    }
    else {
      if (S7_inherits(turn, angle)) turn <- turn@turn
      new_object(angle(turn = turn))
    }
  }
)


#' gradian class
#'
#' @rdname angle
#' @export
gradian <- new_class(
  name = "gradian",
  parent = angle,
  constructor = function(gradian = class_missing) {
    if (length(gradian) > 1) {
      return(angle_list(lapply(gradian, \(x) gradian(x))))
    } else {
      if (S7_inherits(gradian, angle)) gradian <- gradian@gradian
      new_object(angle(gradian = gradian))
    }
  }
)


## addition ----
method(`+`, list(angle, angle_list)) <- function(e1, e2) {
  angle_list(purrr::map(e2, \(x) `+`(e1 = e1, e2 = x)))
}
method(`+`, list(angle_list, angle)) <- function(e1, e2) {
  angle_list(purrr::map(e1, \(x) `+`(e1 = x, e2 = e2)))
}

method(`+`, list(angle, class_numeric)) <- function(e1, e2) {
  # This preserves e2's type
  e1@turn <- e1@turn + num2turn(e2, e1)
  e1
}
method(`+`, list(class_numeric, angle)) <- function(e1, e2) {
  e2@turn <- num2turn(e1, e2) + e2@turn
  e2
}



## subtraction ----

method(`-`, list(angle, angle)) <- function(e1, e2) {
  # This preserves e2's type
  e2@turn <- e1@turn - e2@turn
  e2
}
method(`-`, list(angle, angle_list)) <- function(e1, e2) {
  angle_list(purrr::map(e2, \(x) `-`(e1 = e1, e2 = x)))
}
method(`-`, list(angle_list, angle)) <- function(e1, e2) {
  angle_list(purrr::map(e1, \(x) `-`(e1 = x, e2 = e2)))
}

method(`*`, list(angle, angle)) <- function(e1, e2) {
  e2@turn <- e1@turn * e2@turn
  e2
}
method(`/`, list(angle, angle)) <- function(e1, e2) {
  e2@turn <- e1@turn / e2@turn
  e2
}



method(`-`, list(angle, class_numeric)) <- function(e1, e2) {
  e1@turn <- e1@turn - num2turn(e2, e1)
  e1
}
method(`-`, list(class_numeric, angle)) <- function(e1, e2) {
  e2@turn <- num2turn(e1, e2) - e2@turn
  e2
}

method(`*`, list(angle, class_numeric)) <- function(e1, e2) {
  e1@turn <- e1@turn * e2
  e1
}

method(`*`, list(class_numeric, angle)) <- function(e1, e2) {
  e2@turn <- e1 * e2@turn
  e2
}

method(`/`, list(angle, class_numeric)) <- function(e1, e2) {
  e1@turn <- e1@turn / e2
  e1
}

method(`==`, list(angle, angle)) <- function(e1, e2) {
  abs(e1@turn - e2@turn) <= .Machine$double.eps
}

method(`<`, list(angle, angle)) <- function(e1, e2) {
  e1@turn < e2@turn
}

method(`>`, list(angle, angle)) <- function(e1, e2) {
  e1@turn > e2@turn
}



method(as.character, angle) <- function(x,
  ...,
  digits = NULL,
  type = NULL) {
  if (is.null(type)) {
    a_class <- match.arg(
      arg = class(x)[1],
      choices = c("degree", "radian", "gradian", "turn", "angle"))
  } else {
    a_class <- rlang::arg_match(type, c("degree", "radian", "turn", "gradian"))
  }


  if (a_class == "angle") a_class <- "degree"

  if (is.null(digits)) {
    digits <- c(degree = 0, radian = 2, turn = 2, gradian = 0)[a_class]
  }
  switch(
    a_class,
    degree = paste0(signs::signs(round(x@degree, digits)), "\u00B0"),
    radian = ifelse(
      abs(x@radian - pi) < .Machine$double.eps,
      "\u03C0",
      paste0(round(x@turn * 2, digits), "\u03C0")),
    gradian = paste0(round(x@gradian, digits), ""),
    turn = paste0(round_probability(x@turn, digits = digits)),
    angle = 2 * pi)
}

method(as.character, angle_list) <- function(x, ...) {
  purrr::map_chr(x, as.character)
}

method(`+`, list(class_character, angle)) <- function(e1, e2) {
  e1 + as.character(e2)
}

method(`+`, list(angle, class_character)) <- function(e1, e2) {
  as.character(e1) + e2
}

method(`+`, list(degree, class_character)) <- function(e1, e2) {
  as.character(e1) + e2
}


#' Convert hjust and vjust parameters from polar coordinates
#' @param x angle
#' @param multiplier distance
#' @param axis vertical (v) or horizontal (h)
#' @export
polar2just <- new_generic(
  name = "polar2just",
  dispatch_args = "x",
  fun = function(x, multiplier = 1.2, axis = c("h", "v")) {
    S7_dispatch()
  }
)
method(polar2just, class_numeric) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  if (length(multiplier) == 0) multiplier <- 1.2
  axis <- match.arg(axis)
  if (axis == "h") {
    (((cos(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  } else {
    (((sin(x + pi) + 1)/2) - 0.5) * multiplier + 0.5
  }

}

method(polar2just, angle) <- function(x, multiplier = 1.2, axis = c("h", "v")) {
  polar2just(x@radian, multiplier, axis)
}



