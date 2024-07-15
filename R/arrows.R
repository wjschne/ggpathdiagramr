
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
  parent = shape_list,
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

arrow_segment_or_arrow_segment_list <- new_union(arrow_segment, arrow_segment_list)

method(get_tibble, arrow_segment) <- function(x) {
  xs <- c(list(x = x@p1@x,
               xend = x@p2@x,
               y = x@p1@y,
               yend = x@p2@y),
          get_non_empty_props(x@style))

  rlang::inject(tibble::tibble(!!!xs))
}


method(get_tibble_defaults, arrow_segment_list) <- function(x) {
  sp <- style_arrow(
    alpha = replace_na(as.double(ggarrow::GeomArrowSegment$default_aes$alpha), 1),
    arrow_head = replace_na(ggarrow::GeomArrowSegment$default_aes$arrow_head, the$arrow_head),
    arrow_fins = replace_na(ggarrow::GeomArrowSegment$default_aes$arrow_fins, NULL),
    color = replace_na(ggarrow::GeomArrowSegment$default_aes$colour, "black"),
    lineend = "butt",
    linejoin = "round",
    linewidth = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linewidth_head = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linewidth_fins = replace_na(ggarrow::GeomArrowSegment$default_aes$linewidth, 1),
    linetype = replace_na(ggarrow::GeomArrowSegment$default_aes$linetype, 1)
  )

  get_tibble_defaults_helper(x, sp,required_aes = c("x", "y", "xend", "yend"))
}
