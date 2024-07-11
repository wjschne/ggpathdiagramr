

# style----
#' style class
#'
#' @param alpha numeric value for alpha transparency
#' @param angle angle of text
#' @param arrow_fins A 2-column matrix of polygon points
#' @param arrow_head A 2-column matrix of polygon points
#' @param arrow_mid A 2-column matrix of polygon points
#' @param color character string for color
#' @param family font family
#' @param fill character string for fill color
#' @param fontface Can be plain, bold, italic, or bold.italic
#' @param hjust horizontal justification
#' @param lineheight height of line of text
#' @param linejoin height of line of text
#' @param linewidth width of lines
#' @param linetype type of lines
#' @param shape type of shape
#' @param size numeric size
#' @param size.unit How the size aesthetic is interpreted: as points ("pt"), millimetres ("mm"), centimetres ("cm"), inches ("in"), or picas ("pc").
#' @param vjust vertical justification
#' @param linetype type of lines
#' @export
#' @rdname style
style_base <- new_class(
  name = "style_base",
  abstract = TRUE,
  properties = list(
    alpha = class_numeric,
    color = class_character
  )
)

prop_hjust <- new_property(
  numeric_or_character,
  validator = function(value) {
    if (is.character(value)) {
      if (length(value) > 0 && !all(value %in% c("left", "center", "right")))
        stop('vjust must be "left", "center", "right", or a numeric value')
    }
  }
)

prop_vjust = new_property(
  numeric_or_character,
  validator = function(value) {
    if (is.character(value)) {
      if (length(value) > 0 && !all(value %in% c("top", "middle", "bottom")))
        stop('vjust must be "top", "middle", "bottom", or a numeric value')
    }
  }
)


#' @export
#' @rdname style
style <- new_class(
  name = "style",
  parent = style_base,
  properties = list(
    angle = class_numeric,
    arrow = class_any,
    arrow_head = class_numeric,
    arrow_fins = class_numeric,
    arrow_mid = class_numeric,
    family = class_character,
    fill = class_character,
    fontface = class_character,
    hjust = prop_hjust,
    justify = class_numeric,
    length = numeric_or_unit,
    length_head = numeric_or_unit,
    length_fins = numeric_or_unit,
    length_mid  = numeric_or_unit,
    lineend = numeric_or_character,
    lineheight = class_numeric,
    linejoin = class_character,
    linewidth_fins = class_numeric,
    linewidth = class_numeric,
    linetype = numeric_or_character,
    resect = numeric_or_unit,
    resect_fins = numeric_or_unit,
    resect_head = numeric_or_unit,
    shape = numeric_or_character,
    size = class_numeric,
    size.unit = new_property(
      class_character,
      validator = function(value) {

        if (length(value) > 0 && !all(value %in% c("pt", "mm", "cm", "in", "pc")))
          stop('size.unit must be "pt", "mm", "cm", "in", or "pc"')
      }
    ),
    stroke = class_numeric,
    stroke_color = class_character,
    stroke_width = class_character,
    vjust = prop_vjust
  )
)

# style_point class ----
#' @export
#' @rdname style
style_point <- new_class(
  name = "style_point",
  parent = style_base,
  properties = list(
    fill = class_character,
    shape = numeric_or_character,
    size = class_numeric,
    stroke = class_numeric
  ))
# style_line class ----
#' @export
#' @rdname style
style_line <- new_class(
  name = "style_line",
  parent = style_base,
  properties = list(
    lineend = numeric_or_character,
    linejoin = numeric_or_character,
    linewidth = class_numeric,
    linetype = numeric_or_character,
    arrow = class_any
  ))

# style_arrow class ----
#' @export
#' @rdname style
style_arrow <- new_class(
  name = "style_arrow",
  parent = style_line,
  properties = list(
    arrow_fins = class_numeric,
    arrow_head = class_numeric,
    arrow_mid = class_numeric,
    length_head = numeric_or_unit,
    length_fins = numeric_or_unit,
    length_mid  = numeric_or_unit,
    linewidth_fins = class_numeric,
    resect = numeric_or_unit,
    resect_fins = numeric_or_unit,
    resect_head = numeric_or_unit
  ))

# style_polygon class ----
#' @export
#' @rdname style
style_polygon <- new_class(
  name = "style_polygon",
  parent = style_line,
  properties = list(
    fill = class_character,
    rule = numeric_or_character
  ))

# style_label class ----
#' @export
#' @rdname style
style_label <- new_class(
  name = "style_label",
  parent = style_base,
  properties = list(
    angle = class_numeric,
    family = class_character,
    fontface = class_character,
    hjust = prop_hjust,
    label.color = class_character,
    label.padding = class_character,
    label.margin = class_character,
    label.r = class_character,
    label.size = class_character,
    lineheight = class_numeric,
    nudge_x = class_numeric,
    nudge_y = class_numeric,
    size = class_numeric,
    vjust = prop_vjust
  )
)



style_or_style_point <- new_union(style, style_point)
style_or_style_label <- new_union(style, style_label)
style_or_style_line <- new_union(style, style_line)
style_or_style_polygon <- new_union(style, style_polygon)
style_or_style_line_or_style_arrow <- new_union(style, style_line, style_arrow)
style_any <- new_union(style_base, style, style_point, style_line, style_polygon, style_arrow)

method(`+`, list(style_any, style_any)) <- function(e1, e2) {
  for (p in prop_names(e1)) {
    if (prop_exists(e2,p) && length(prop(e2,p))) {
      prop(e1,p) <- prop(e2,p)
    }
  }
  e1
}

method(`+`, list(class_any, style_base)) <- function(e1, e2) {
  e2
}

method(`+`, list(style_base, class_any)) <- function(e1, e2) {
  e1
}


# style list----
#' segment_list
#'
#' Make lists of segments
#' @param .data list of segments
#' @param d data.frame
#' @export
style_list <- new_class(
  name = "style_list",
  parent = class_list,
  # properties = list(
  #   data = new_property(class_data.frame, getter = function(self) {
  #     Filter(function(s) length(s) > 0 , props(self)) |>
  #       purrr::map(\(x) ifelse(
  #         length(x) > 1,
  #         list(x),
  #         x)) |>
  #       tibble::as_tibble_row()
  #   })
  #   ),
  validator = function(self) {
    allsameclass(self, "style_base")
  }
)

method(`+`, list(style_base, style_list)) <- function(e1, e2) {
  style_list(purrr::map(e2, \(sl) e1 + sl))
}

method(`+`, list(style_list, style_base)) <- function(e1, e2) {
  style_list(purrr::map(e1, \(sl) e2 + sl))
}

# style_list

# as_tibble <- new_generic("as_tibble", "object")
# method(as_tibble, style_list) <- function(object) {
#   purrr::map(object, \(o) {
#
#   })
#   Filter(f = function(s) {
#     length(s) > 0
#   }, x = object) |>
#     purrr::map(\(x) {
#       if (length(x) > 1) {
#         return(list(x))
#       } else {
#         return(x)
#       }
#     }) |>
#     tibble::as_tibble_row()
# }
#
#
#
# object  = style_list(c(style(color = "red"),
#                        style(color = "red")))
#
# as_tibble(object)
