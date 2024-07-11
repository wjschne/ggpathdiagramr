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
      getter = function(self)
       (self@turn %% ifelse(self@turn < 0, -1, 1)) * 2 * pi
    ),
    degree = new_property(
      getter = function(self)
      (self@turn %% ifelse(self@turn < 0, -1, 1)) * 360
    ),
    turn = new_property(
      name = "turn",
      class = class_numeric
    ),
    gradian = new_property(
      getter = function(self)
      (self@turn %% ifelse(self@turn < 0, -1, 1)) * 400
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
      return(angle_list(lapply(x, angle)))
    }

    new_object(S7_object(), turn = x)


  }
)




method(cos, angle) <- function(x) {
  cospi(x@turn * 2)
}
method(sin, angle) <- function(x) {
  sinpi(x@turn * 2)
}
method(tan, angle) <- function(x) {
  tanpi(x@turn * 2)
}

method(`+`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn + e2@turn)
}
method(`-`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn - e2@turn)
}
method(`*`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn * e2@turn)
}
method(`/`, list(angle, angle)) <- function(e1, e2) {
  angle(turn = e1@turn / e2@turn)
}

method(`+`, list(angle, class_numeric)) <- function(e1, e2) {
  angle(turn = e1@turn + e2 / (2 * pi))
}
method(`+`, list(class_numeric, angle)) <- function(e1, e2) {
  angle(turn = e1 / (2 * pi) + e2@turn)
}

method(`-`, list(angle, class_numeric)) <- function(e1, e2) {
  angle(turn = e1@turn - e2 / (2 * pi))
}
method(`-`, list(class_numeric, angle)) <- function(e1, e2) {  
  angle(turn = e1 / (2 * pi) - e2@turn)
}

method(`*`, list(angle, class_numeric)) <- function(e1, e2) {
  angle(turn = e1@turn * e2)
}

method(`*`, list(class_numeric, angle)) <- function(e1, e2) {
  angle(turn = e1 * e2@turn)
}

method(`/`, list(angle, class_numeric)) <- function(e1, e2) {
  angle(turn = e1@turn / e2)
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


class_angle_or_numeric <- new_union(angle, class_numeric)


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
  parent = class_list,
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

method(cos, angle_list) <- function(x) {
  cospi(x@turn * 2)
}
method(sin, angle_list) <- function(x) {
  sinpi(x@turn * 2)
}
method(tan, angle_list) <- function(x) {
  tanpi(x@turn * 2)
}


angle_or_angle_list <- new_union(angle, angle_list)

# Angle wrappers ----
#' degree class
#'
#' @rdname angle
#' @export
degree <- new_class(
  name = "degree",
  parent = angle,
  constructor = function(degree) {
    if (length(degree) > 1) {
      return(angle_list(lapply(degree, \(x) degree(x))))
    } else {
      new_object(angle(degree = degree))
    }
  }
)

method(`+`, list(angle, degree)) <- function(e1, e2) {
  degree((e1@turn + e2@turn) * 360)
}

method(`-`, list(angle, degree)) <- function(e1, e2) {
  degree((e1@turn - e2@turn) * 360)
}


method(`+`, list(degree, class_numeric)) <- function(e1, e2) {
  e1 + degree(e2)
}
method(`+`, list(class_numeric, degree)) <- function(e1, e2) {
  degree(e1) + e2
}

method(`-`, list(degree, class_numeric)) <- function(e1, e2) {
  e1 - degree(e2)
}
method(`-`, list(class_numeric, degree)) <- function(e1, e2) {
  degree(e1) - e2
}

#' radian class
#'
#' @rdname angle
#' @export
radian <- new_class(
  name = "radian",
  parent = angle,
  constructor = function(radian) {
    if (length(radian) > 1) {
      return(angle_list(lapply(radian, \(x) radian(x))))
    }
    else {
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
  constructor = function(turn) {
    if (length(turn) > 1) {
      return(angle_list(lapply(turn, \(x) turn(x))))
    }
    else {
      new_object(angle(turn = turn))
    }
  }
)


method(`+`, list(angle, turn)) <- function(e1, e2) {
  turn(e1@turn + e2@turn)
}

method(`-`, list(angle, turn)) <- function(e1, e2) {
  turn(e1@turn - e2@turn)
}

method(`+`, list(turn, class_numeric)) <- function(e1, e2) {
  e1 + turn(e2)
}
method(`+`, list(class_numeric, turn)) <- function(e1, e2) {
  turn(e1) + e2
}

method(`-`, list(turn, class_numeric)) <- function(e1, e2) {
  e1 - turn(e2)
}
method(`-`, list(class_numeric, turn)) <- function(e1, e2) {
  turn(e1) - e2
}

#' gradian class
#'
#' @rdname angle
#' @export
gradian <- new_class(
  name = "gradian",
  parent = angle,
  constructor = function(gradian) {
    if (length(gradian) > 1) {
      turn <- gradian / 400
      return(angle_list(lapply(turn, \(x) gradian(x))))
    } else {
      new_object(angle(gradian = gradian))
    }
  }
)


method(`+`, list(angle, gradian)) <- function(e1, e2) {  
  gradian((e1@turn + e2@turn) * 400)
}

method(`-`, list(angle, gradian)) <- function(e1, e2) {
  gradian((e1@turn - e2@turn) * 400)
}

method(`+`, list(gradian, class_numeric)) <- function(e1, e2) {
  e1 + gradian(e2)
}
method(`+`, list(class_numeric, gradian)) <- function(e1, e2) {
  gradian(e1) + e2
}

method(`-`, list(gradian, class_numeric)) <- function(e1, e2) {
  e1 - gradian(e2)
}
method(`-`, list(class_numeric, gradian)) <- function(e1, e2) {
  gradian(e1) - e2
}

# as.character ----

# as.character <- new_generic("as.character", dispatch_args = "x")
method(as.character, degree) <- function(x, ..., digits = 1) {
  paste0(signs::signs(round(x@degree, digits)), "\u00B0")
}
method(as.character, radian) <- function(x, ..., digits = 2) {
  rad <- paste0(round(x@turn * 2, digits), "\u03C0")
  rad[rad == "1\u03C0"] <- "\u03C0"
  rad
}
method(as.character, gradian) <- function(x, ..., digits = 2) {
  paste0(round(x@gradian, digits), "grad")
}
method(as.character, turn) <- function(x, ..., digits = 2) {
  paste0(round_probability(x@turn, digits = digits), "\u03C4")
}

method(as.character, angle) <- function(x,
  ...,
  digits = 2,
  type = "degree") {
type <- rlang::arg_match(type, c("degree", "radian", "turn", "gradian"))
switch(
type,
degree = as.character(degree(x@degree), ..., digits = digits),
radian = as.character(radian(x@radian), ..., digits = digits),
turn = as.character(turn(x@turn), ..., digits = digits),
gradian = as.character(gradian(x@gradian), ..., digits = digits)
)
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

method(`+`, list(degree, class_character)) <- function(e1,e2) {
  as.character(e1) + e2
}

