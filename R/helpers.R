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
