
# Arrow Segment ----
#' arrow_segment class
#'
#' @param p1 point 1
#' @param p2 point 2
#' @param style a style list
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> arguments passed to style object if style is empty
#' @export
arrow_segment <- new_class(
  name = "arrow_segment",
  parent = segment,
  properties = list(
    style = new_property(style_or_style_line_or_style_arrow)
  ),
  constructor = function(p1 = class_missing,
                         p2 = class_missing,
                         style = class_missing,
                         ...) {
    a_style <- style_arrow(arrow_head = the$arrow_head) + style + style_arrow(...)

    d <- tibble::tibble(p1 = c(p1), p2 = c(p2), style = c(a_style))
    if (nrow(d) > 1) {
      return(arrow_segment_list(purrr::pmap(d, arrow_segment)))
    }


    new_object(.parent = segment(p1 = p1, p2 = p2), style = a_style)
  }

)

method(`+`, list(ggplot_class, arrow_segment)) <- function(e1, e2) {
  e1 + as.geom(e2)
}


# Arrow Segment list----
#' arrow_segment_list
#'
#' Make lists of arrow_segments
#' @param .data list of arrow_segments
#' @param p1 point_list of first points (p1)
#' @param p2 point_list of second points (p2)
#' @param line line_list of lines associated with segments
#' @param style style_list of lines associated with segments
#' @export
#' @examples
#' s1 <- segment(point(0,0), point(1,1))
#' s2 <- segment(point(1,1), point(0,1))
#' segment_list(c(s1,s2))
arrow_segment_list <- new_class(
  name = "arrow_segment_list",
  parent = class_list,
  properties = list(
    p1 = new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p1))
      }
    ),
    p2 = new_property(
      point_list,
      getter = function(self) {
        point_list(lapply(self, \(x) x@p2))
      }
    ),
    line = new_property(
      line_list,
      getter = function(self) {
        line_list(lapply(self, \(x) x@line))
      }
    ),
    style = new_property(
      style_list,
      getter = function(self) {
        style_list(lapply(self, \(x) x@style))
      }
    )),
  validator = function(self) {
    allsameclass(self, "arrow_segment")
  }
)

method(`+`, list(ggplot_class, arrow_segment_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}
