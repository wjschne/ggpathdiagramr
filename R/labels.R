# label----

#' label class
#'
#' @param p point
#' @param label text label
#' @param style a style list
#' @param ... properties passed to style
#' @export
label <- new_class(
  name = "label",
  parent = xy,
  properties = list(
    p = new_property(point, default = point(0, 0, style = style_point(shape = 16))),
    label = new_property(class_character, default = ""),
    xy = new_property(
      class_numeric,
      getter = function(self) {
        self@p@xy
      }
    ),
    style = new_property(style_or_style_label, default = style_label(size = 10))
  ),
  constructor = function(p = class_missing,
                         label = class_missing,
                         style = class_missing,
                         ...) {
    if (length(p) == 0) {
      p <- point()
      p_style <- style_point()
    }

    p_style <- p@style
    p_style@size <- numeric()


    l_style <- style_label() + p_style + style + style_label(...)
    if (length(label) == 0) {
      label = paste0("(", signs::signs(round(p@x, 2)), ",", signs::signs(round(p@y, 2)), ")")
    } else if (is.numeric(label)) {
      label <- formatC(label, format = "fg", digits = 2)
    }

    d <- tibble::tibble(p = c(p), label = label, style = c(p_style))
    if (nrow(d) > 1) {
      return(label_list(purrr::pmap(d, label)))
    }

    new_object(S7_object(), p = p, label = label, style  = l_style)
  }
)



method(`-`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p - e2@p, label = paste0(e1@label, " - ", e2@label), style = e1@style + e2@style)
}

method(`+`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p + e2@p, label = paste0(e1@label, " + ", e2@label), style = e1@style + e2@style)
}
method(`+`, list(ggplot_class, label)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

point_or_label <- new_union(point, label)


# Label list----
#' point_list class
#'
#' @export
#' @rdname point
label_list <- new_class(
  name = "label_list",
  parent = class_list,
  properties = list(
    p = new_property(
      point_list,
      getter = function(self) {
        sapply(self, \(x) x@p)
      }
    ),
    label = new_property(
      class_numeric,
      getter = function(self) {
        sapply(self, \(x) x@label)
      }
    ),
    xy = new_property(
      class_numeric,
      getter = function(self) {
        as.matrix(self@p@xy)
      }
    ),
    style = new_property(
      class_list,
      getter = function(self) {
        lapply(self, \(x) x@style)
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "label")
  }
)
label_or_label_list <- new_union(label, label_list)
