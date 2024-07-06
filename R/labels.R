# label----

#' label class
#'
#' @param p point
#' @param label text label
#' @param style a style list
#' @param ... properties passed to style
#' @export
label <- S7::new_class(
  name = "label",
  parent = xy,
  properties = list(
    p = S7::new_property(point, default = point(0, 0, style = style())),
    label = S7::new_property(S7::class_character, default = ""),
    xy = S7::new_property(
      S7::class_numeric,
      getter = function(self) {
        self@p@xy
      }
    ),
    style = S7::new_property(style, default = style())
  ),
  constructor = function(p = S7::class_missing,
                         label = S7::class_missing,
                         style = S7::class_missing,
                         ...) {
    style <- p@style + style + style(...)
    if (length(label) == 0) {
      label = paste0("(", round(p@x, 2), ",", round(p@y, 2), ")")
    } else if (is.numeric(label)) {
      label <- formatC(label, format = "fg", digits = 2)
    }
    S7::new_object(S7::S7_object(), p = p, label = label, style  = style)
  }
)



S7::method(`-`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p - e2@p, label = paste0(e1@label, " - ", e2@label), style = e1@style + e2@style)
}

S7::method(`+`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p + e2@p, label = paste0(e1@label, " + ", e2@label), style = e1@style + e2@style)
}
S7::method(`+`, list(ggplot_class, label)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

point_or_label <- S7::new_union(point, label)
