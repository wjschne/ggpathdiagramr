#' @keywords internal
rotate2columnmatrix <- function(x, theta) {
  x_rotated <- x %*%  matrix(c(cos(theta),
                               -sin(theta),
                               sin(theta),
                               cos(theta)),
                             nrow = 2, ncol = 2)
  colnames(x_rotated) <- colnames(x)
  x_rotated
}

#' @keywords internal
allsameclass <- function(l, classname) {
    allsame <- all(sapply(lapply(l, class),
                         function(x)  classname %in% x))
    if (!allsame) {
      "All items must be points."
    }
}

#' @keywords internal
cardinalpoint <- function(x) {
  namedpositions <- c(
    east = 0,
    `east-northeast` = 22.5,
    northeast = 45,
    `north-northeast` = 67.5,
    north = 90,
    `north-northwest` = 112.5,
    northwest = 135,
    `west-northwest` = 157.5,
    west = 180,
    `west-southwest` = 202.5,
    southwest = 225,
    `south-southwest` = 247.5,
    south = 270,
    `south-southeast` = 292.5,
    southeast = 315,
    `east-southeast` = 337.5
  )
  if (!all(x %in% names(namedpositions))) {
    stop("Position must be an angle, numeric, or a one of cardinal points:\nnoorth, east, south, west, northeast, northwest, southwest, southeast, east-northest, north-northeast, north-northwest, west-northwest, west-southwest, south-southwest, south-southeast, east-southeast")
  }
  position <- angle(degree = unname(namedpositions[x]))
  position
}

#' @keywords internal
trimmer <- function(x) {
  notabs <- gsub(x = x, pattern = "\\t", replacement = " ")
  trimws(gsub(x = notabs, pattern = "\\s+", replacement = " "))
}

#' @keywords internal
rounder <- function(x, digits = 2, add = FALSE) {
  if (add) {
    r <- paste0(ifelse(x > 0, " + ", " \u2212 "), formatC(abs(x), digits = digits, format = "fg"))
  } else {
    r <- paste0(ifelse(x > 0, "", "\u2212"), formatC(abs(x), digits = digits, format = "fg"))
  }
  r
}






#' @keywords internal
geom_styles <- function(x, ...) {
  p_style <- x + style(...)
  Filter(function(s) length(s) > 0 , props(p_style))
}
