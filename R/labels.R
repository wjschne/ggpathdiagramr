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
    label = new_property(class_angle_or_character, default = ""),
    xy = new_property(
      class_numeric,
      getter = function(self) {
        self@p@xy
      }
    ),
    style = new_property(style_or_style_label)
  ),
  constructor = function(p = class_missing,
                         label = class_missing,
                         style = class_missing,
                         ...) {
    if (length(p) == 0) {
      p <- point()
      p_style <- style_point()
    }

    # p <- get_points(x)

    # if (!any(class(p) %in% c("point", "point_list"))) {
    #   p <- point_list(p)
    # }

    if (length(p) > 1) {
      p_style <- style_point()
      p1 <- p
      if (length(label) == 0) label <- NA
    } else {
      p_style <- style_point() + p@style
      p1 <- c(p)

    }






    l_style <- style_label() +
      p_style +
      style_label(size =  numeric()) +
      style +
      style_label(...)


    d <- tibble::tibble(p = p1,
                        label = c(label),
                        style = c(l_style))
    if (nrow(d) > 1) {
      return(label_list(purrr::pmap(d, \(p, label, style) label(p, label, style))))
    }



    if (S7::S7_inherits(p, segment)) {
      l_style@angle <- p@line@angle@degree
      l_style@vjust = ifelse(length(l_style@vjust) == 0 || is.na(l_style@vjust), 0,l_style@vjust)
      l_style <- l_style + style_label(...)
      p <- midpoint(p)
    }

    if (S7::S7_inherits(p, arc)) {
      if (length(label) == 0) {
        label = as.character(degree(p@theta@degree))
      }
      p <- midpoint(p)
      l_style <- polar_just(l_style,
                             p@theta + angle(turn = .5),
                             multiplier = 1.15)
      l_style <- l_style + style_label(...)

    }

    if (length(label) == 0 || ((is.character(label) || is.logical(label)) && is.na(label))) {

      label = paste0("(", signs::signs(round(p@x, 2)), ",",
                     signs::signs(round(p@y, 2)), ")")
    } else if (is.numeric(label)) {
      label <- formatC(label, format = "fg", digits = 2)
    }





    new_object(S7_object(),
               p = p,
               label = as.character(label),
               style  = l_style)
  }
)



method(`-`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p - e2@p, label = paste0(e1@label, " - ", e2@label), style = e1@style + e2@style)
}

method(`+`, list(label, label)) <- function(e1, e2) {
  label(p = e1@p + e2@p, label = paste0(e1@label, " + ", e2@label), style = e1@style + e2@style)
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
        point_list(sapply(self, \(x) x@p))
      }
    ),
    label = new_property(
      class_character,
      getter = function(self) {
        sapply(self, \(x) x@label)
      }
    ),
    style = new_property(
      class_list,
      getter = function(self) {
        style_list(lapply(self, \(x) x@style))
      }
    )
  ),
  validator = function(self) {
    allsameclass(self, "label")
  }
)
label_or_label_list <- new_union(label, label_list)

method(`+`, list(class_ggplot, label_or_label_list)) <- function(e1, e2) {
  e1 + as.geom(e2)
}

method(get_tibble, label) <- function(x) {
  xs <- c(list(x = x@p@x,
             y = x@p@y,
            label = x@label),
          get_non_empty_props(x@style))

  xs <- purrr::map(xs, \(s) ifelse(is.vector(s), s, c(s)))
  xs$polar_just_angle <- NULL
  xs$polar_just_multiplier <- NULL

  rlang::inject(tibble::tibble(!!!xs))
}



method(get_tibble_defaults, label_list) <- function(x) {
  sp <- style_label(
    alpha = 1,
    color = "black",
    angle = 0,
    family = "sans",
    fill = "fill",
    fontface = "plain",
    hjust = .5,
    label.color = "black",
    label.margin = ggplot2::margin(2,2,2,2, "pt"),
    label.padding = ggplot2::margin(1,1,1,1, "pt"),
    label.size = .25,
    lineheight = 1.2,
    nudge_x = 0,
    nudge_y = 0,
    polar_just_angle = angle(degree = 90),
    polar_just_multiplier = 1.2,
    size =  11,
    text.color = "black",
    vjust = .5
  )
  get_tibble_defaults_helper(x, sp, required_aes = c("x", "y", "label"))
}
