

# style----
#' style class
#'
#' @param alpha numeric value for alpha transparency
#' @param arrow_fins A 2-column matrix of polygon points
#' @param arrow_head A 2-column matrix of polygon points
#' @param angle angle of text
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
#' @export
style <- S7::new_class("style", properties = list(
  alpha = S7::class_numeric,
  arrow_head = class_numeric,
  arrow_fins = class_numeric,
  angle = S7::class_numeric,
  color = S7::class_character,
  family = S7::class_character,
  fill = S7::class_character,
  fontface = S7::class_character,
  hjust = S7::new_property(numeric_or_character, validator = function(value) {
    if (is.character(value)) {
      if (!(value %in% c("left", "center", "right"))) stop('vjust must be "left", "center", "right", or a numeric value')
    }
  }),
  lineheight = S7::class_numeric,
  linejoin = S7::class_character,
  linewidth = S7::class_numeric,
  linetype = numeric_or_character,
  shape = numeric_or_character,
  size = S7::class_numeric,
  size.unit = S7::new_property(S7::class_character, default = "pt", validator = function(value) {
    if (!(value %in% c("pt", "mm", "cm", "in", "pc"))) stop('vjust must be "top", "middle", "bottom", or a numeric value')
  }),
  vjust = S7::new_property(numeric_or_character, validator = function(value) {
    if (is.character(value)) {
      if (!(value %in% c("top", "middle", "bottom"))) stop('vjust must be "top", "middle", "bottom", or a numeric value')
    }
  })
)
)

S7::method(`+`, list(style, style)) <- function(e1, e2) {
  for (p in S7::prop_names(e2)) {
    if (length(S7::prop(e2,p))) {
      S7::prop(e1,p) <- S7::prop(e2,p)
    }
  }
  e1
}

S7::method(`+`, list(S7::class_any, style)) <- function(e1, e2) {
  e2
}

S7::method(`+`, list(style, S7::class_any)) <- function(e1, e2) {
  e1
}


