


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
#' @param size.unit How the size aesthetic is interpreted: as points ("pt"), millimeters ("mm"), centimeters ("cm"), inches ("in"), or picas ("pc").
#' @param vjust vertical justification
#' @param linetype type of lines
#' @export
#' @rdname style
style_base <- new_class(
  name = "style_base",
  abstract = TRUE,
  properties = list(
    alpha = class_numeric,
    color = class_character_or_logical
  )
)

prop_hjust <- new_property(
  class_numeric_or_character,
  validator = function(value) {
    if (is.character(value)) {
      if (length(value) > 0 && !all(value %in% c("left", "center", "right")))
        stop('vjust must be "left", "center", "right", or a numeric value')
    }
  }
)

prop_vjust = new_property(
  class_numeric_or_character,
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
    angle = class_angle_or_numeric,
    arrow_fins = class_list,
    arrow_head = class_list,
    arrow_mid = class_list,
    family = class_character,
    fill = class_character_or_logical,
    fontface = class_character,
    hjust = prop_hjust,
    justify = class_numeric,
    label.color = class_character_or_logical,
    label.padding = class_list,
    label.margin = class_list,
    label.r = class_any,
    label.size = class_any,
    length = class_any,
    length_head = class_any,
    length_fins = class_any,
    length_mid  = class_any,
    lineend = class_numeric_or_character,
    lineheight = class_numeric,
    linejoin = class_character,
    linewidth_fins = class_numeric,
    linewidth_head = class_numeric,
    linewidth = class_numeric,
    linetype = class_numeric_or_character,
    nudge_x = class_numeric,
    nudge_y = class_numeric,
    polar_just = class_angle_or_numeric,
    resect = class_any,
    resect_fins = class_any,
    resect_head = class_any,
    shape = class_numeric_or_character,
    size = class_any,
    size.unit = new_property(
      class_character,
      validator = function(value) {

        if (length(value) > 0 && !all(value %in% c("pt", "mm", "cm", "in", "pc")))
          stop('size.unit must be "pt", "mm", "cm", "in", or "pc"')
      }
    ),
    stroke = class_numeric,
    stroke_color = class_character_or_logical,
    stroke_width = class_character,
    text.color = class_character_or_logical,
    vjust = prop_vjust
  ),
  constructor = function(alpha = class_missing,
                         angle = class_missing,
                         arrow_head = class_missing,
                         arrow_fins = class_missing,
                         arrow_mid = class_missing,
                         color = class_missing,
                         family = class_missing,
                         fill = class_missing,
                         fontface = class_missing,
                         hjust = class_missing,
                         justify = class_missing,
                         label.color = class_missing,
                         label.margin = class_missing,
                         label.padding = class_missing,
                         label.r = class_missing,
                         label.size = class_missing,
                         length = class_missing,
                         length_head = class_missing,
                         length_fins = class_missing,
                         length_mid  = class_missing,
                         lineend = class_missing,
                         lineheight = class_missing,
                         linejoin = class_missing,
                         linewidth_fins = class_missing,
                         linewidth_head = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         nudge_x = class_missing,
                         nudge_y = class_missing,
                         polar_just = class_missing,
                         resect = class_missing,
                         resect_fins = class_missing,
                         resect_head = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         size.unit = class_missing,
                         stroke = class_missing,
                         stroke_color = class_missing,
                         stroke_width = class_missing,
                         text.color = class_missing,
                         vjust = class_missing,
                         ...) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color
    label.color <- the_style$label.colour %||% label.color
    text.color <- the_style$text.colour %||% text.color
    stroke_color <- the_style$stroke_colour %||% stroke_color


    d <- list(
      alpha = c(alpha),
      angle = c(angle),
      polar_just = c(polar_just),
      arrow_head = list(arrow_head),
      arrow_fins = list(arrow_fins),
      arrow_mid = list(arrow_mid),
      color = c(color),
      family = c(family),
      fill = c(fill),
      fontface = c(fontface),
      hjust = c(hjust),
      justify = c(justify),
      label.color = c(label.color),
      label.padding = ifelse(length(label.padding) > 0,
                             list(label.padding),
                             list()),
      label.margin = ifelse(length(label.margin) > 0,
                            list(label.margin),
                            list()),
      label.r = c(label.r),
      label.size = c(label.size),
      length = c(length),
      length_head = c(length_head),
      length_fins = c(length_fins),
      length_mid = c(length_mid),
      lineend = c(lineend),
      lineheight = c(lineheight),
      linejoin = c(linejoin),
      linewidth_fins = c(linewidth_fins),
      linewidth_head = c(linewidth_head),
      linewidth = c(linewidth),
      linetype = c(linetype),
      nudge_x = c(nudge_x),
      nudge_y = c(nudge_y),
      resect = c(resect),
      resect_fins = c(resect_fins),
      resect_head = c(resect_head),
      shape = c(shape),
      size = c(size),
      size.unit = c(size.unit),
      stroke = c(stroke),
      stroke_color = c(stroke_color),
      stroke_width = c(stroke_width),
      text.color = c(text.color),
      vjust = c(vjust)
    )
    d <- get_non_empty_tibble(d)
    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style)))
    }

    if (length(label.padding) > 0) {
      label.padding <- list(label.padding)
    } else {
      label.padding <- list()
    }
    if (length(label.margin) > 0) {
      label.margin <- list(label.margin)
    } else {
      label.margin <- list()
    }
    if ("angle" %in% class(angle) && length(angle) > 0) angle <- angle@degree

    if (length(arrow_fins) > 0) {
      arrow_fins <- list(arrow_fins)
    } else {
      arrow_fins <- list()
    }
    if (length(arrow_head) > 0) {
      arrow_head <- list(arrow_head)
    } else {
      arrow_head <- list()
    }
    if (length(arrow_mid) > 0) {
      arrow_mid <- list(arrow_mid)
    } else {
      arrow_mid <- list()
    }

    if (S7_inherits(angle, S7_class(angle(0)) )) angle <- angle@degree

    if (length(polar_just) == 1) {
      if (S7_inherits(polar_just, S7_class(angle(0))) || is.numeric(polar_just)) {
        polar_just <- polar(theta = radian(polar_just), r = 1.2)
        
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")      
      polar_just <- class_missing
    }

    new_object(
      S7_object(),
      alpha = alpha,
      color = color,
      angle = angle,
      arrow_head = arrow_head,
      arrow_fins = arrow_fins,
      arrow_mid = arrow_mid,
      family = family,
      fill = fill,
      fontface = fontface,
      hjust = hjust,
      justify = justify,
      label.color = label.color,
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      label.size = label.size,
      length = length,
      length_head = length_head,
      length_fins = length_fins,
      length_mid = length_mid,
      lineend = lineend,
      lineheight = lineheight,
      linejoin = linejoin,
      linewidth_fins = linewidth_fins,
      linewidth_head = linewidth_head,
      linewidth = linewidth,
      linetype = linetype,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      polar_just = polar_just,
      resect = resect,
      resect_fins = resect_fins,
      resect_head = resect_head,
      shape = shape,
      size = size,
      size.unit = size.unit,
      stroke = stroke,
      stroke_color = stroke_color,
      stroke_width = stroke_width,
      text.color = text.color,
      vjust = vjust
    )
  }

)


# style_point class ----
#' @export
#' @rdname style
style_point <- new_class(
  name = "style_point",
  parent = style_base,
  properties = list(
    fill = class_character_or_logical,
    shape = class_numeric_or_character,
    size = class_any,
    stroke = class_numeric
  ),
  constructor = function(alpha = class_missing,
                         color = class_missing,
                         fill = class_missing,
                         shape = class_missing,
                         size = class_missing,
                         stroke = class_missing,
                         ...) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color
    d <- list(
      alpha = c(alpha),
      color = c(color),
      fill = c(fill),
      shape = c(shape),
      size = c(size),
      stroke = c(stroke)
    )

    d <- get_non_empty_tibble(d)
    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style_point)))
    }

    new_object(
      S7_object(),
      alpha = alpha,
      color = color,
      fill = fill,
      shape = shape,
      size = size,
      stroke = stroke
    )
  }
)


# style_line class ----
#' @export
#' @rdname style
style_line <- new_class(
  name = "style_line",
  parent = style_base,
  properties = list(
    lineend = class_numeric_or_character,
    linejoin = class_numeric_or_character,
    linewidth = class_numeric,
    linetype = class_numeric_or_character
  ),
  constructor = function(alpha = class_missing,
                         color = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = class_missing,
                         linetype = class_missing,
                         ...) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color

    d <- list(
      alpha = c(alpha),
      color = c(color),
      lineend = c(lineend),
      linejoin = c(linejoin),
      linewidth = c(linewidth),
      linetype = c(linetype)
    )

    d <- get_non_empty_tibble(d)
    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style_line)))
    }

    new_object(
      S7_object(),
      alpha = alpha,
      color = color,
      lineend = lineend,
      linejoin = linejoin,
      linewidth = linewidth,
      linetype = linetype
    )

  }
)

# style_arrow class ----
#' @export
#' @rdname style
style_arrow <- new_class(
  name = "style_arrow",
  parent = style_line,
  properties = list(
    arrow_fins = class_list,
    arrow_head = class_list,
    arrow_mid = class_list,
    justify = class_numeric,
    length_head = class_any,
    length_fins = class_any,
    length_mid  = class_any,
    linewidth_fins = class_any,
    linewidth_head = class_any,
    resect = class_any,
    resect_fins = class_any,
    resect_head = class_any
  ),
  constructor = function(alpha = class_missing,
                         arrow_fins = class_missing,
                         arrow_head = class_missing,
                         arrow_mid = class_missing,
                         color = class_missing,
                         justify = class_missing,
                         length_fins = class_missing,
                         length_head = class_missing,
                         length_mid = class_missing,
                         lineend = class_missing,
                         linejoin = class_missing,
                         linewidth = class_missing,
                         linewidth_fins = class_missing,
                         linewidth_head = class_missing,
                         linetype = class_missing,
                         resect = class_missing,
                         resect_fins = class_missing,
                         resect_head = class_missing,
                         ...) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color


    d <- list(
      alpha = alpha,
      arrow_fins = ifelse(length(arrow_fins) > 0,
                          list(arrow_fins),
                          list()),
      arrow_head = ifelse(length(arrow_head) > 0,
                          list(arrow_head),
                          list()),
      arrow_mid = ifelse(length(arrow_mid) > 0,
                         list(arrow_mid),
                         list()),
      color = c(color),
      justify = c(justify),
      length_head = c(length_head),
      length_fins = c(length_fins),
      length_mid = c(length_mid),
      lineend = c(lineend),
      linejoin = c(linejoin),
      linewidth = c(linewidth),
      linetype = c(linetype),
      linewidth_fins = c(linewidth_fins),
      linewidth_head = c(linewidth_head),
      resect = c(resect),
      resect_fins = c(resect_fins),
      resect_head = c(resect_head)
    )

    d <- get_non_empty_tibble(d)

    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style_arrow)))
    }

    if (length(arrow_fins) > 0) {
      arrow_fins <- list(arrow_fins)
    } else {
      arrow_fins <- list()
    }
    if (length(arrow_head) > 0) {
      arrow_head <- list(arrow_head)
    } else {
      arrow_head <- list()
    }
    if (length(arrow_mid) > 0) {
      arrow_mid <- list(arrow_mid)
    } else {
      arrow_mid <- list()
    }

    new_object(S7_object(),
               alpha = alpha,
               color = color,
               justify = justify,
               lineend = lineend,
               linejoin = linejoin,
               linewidth = linewidth,
               linetype = linetype,
               arrow_fins = arrow_fins,
               arrow_head = arrow_head,
               arrow_mid = arrow_mid,
               length_head = length_head,
               length_fins = length_fins,
               length_mid = length_mid,
               linewidth_fins = linewidth_fins,
               resect = resect,
               resect_fins = resect_fins,
               resect_head = resect_head)

  }
)

# style_polygon class ----
#' @export
#' @rdname style
style_polygon <- new_class(
  name = "style_polygon",
  parent = style_line,
  properties = list(
    fill = class_character_or_logical,
    rule = class_numeric_or_character
  ),
  constructor = function(
    alpha = class_missing,
    color = class_missing,
    lineend = class_missing,
    linejoin = class_missing,
    linewidth = class_missing,
    linetype = class_missing,
    fill = class_missing,
    rule = class_missing,
    ...) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color

    d <- list(
      alpha = c(alpha),
      color = c(color),
      lineend = c(lineend),
      linejoin = c(linejoin),
      linewidth = c(linewidth),
      linetype = c(linetype),
      fill = c(fill),
      rule = c(rule)
    )

    d <- get_non_empty_tibble(d)
    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style_polygon)))
    }

    new_object(
      S7_object(),
      alpha = alpha,
      color = color,
      lineend = lineend,
      linejoin = linejoin,
      linewidth = linewidth,
      linetype = linetype,
      fill = fill,
      rule = rule)

  })

# style_label class ----
#' @export
#' @rdname style
style_label <- new_class(
  name = "style_label",
  parent = style_base,
  properties = list(
    angle = class_angle_or_numeric,
    family = class_character,
    fill = class_character_or_logical,
    fontface = class_character,
    hjust = prop_hjust,
    label.color = class_character_or_logical,
    label.margin = class_list,
    label.padding = class_list,
    label.r = class_any,
    label.size = class_any,
    lineheight = class_numeric,
    nudge_x = class_numeric,
    nudge_y = class_numeric,
    polar_just = class_angle_or_numeric,
    size = class_any,
    text.color = class_character_or_logical,
    vjust = prop_vjust
  ), constructor = function(
    alpha = class_missing,
    color = class_missing,
    angle = class_missing,
    family = class_missing,
    fill = class_missing,
    fontface = class_missing,
    hjust = class_missing,
    label.color = class_missing,
    label.margin = class_missing,
    label.padding = class_missing,
    label.r = class_missing,
    label.size = class_missing,
    lineheight = class_missing,
    polar_just = class_missing,
    nudge_x = class_missing,
    nudge_y = class_missing,
    size = class_missing,
    text.color = class_missing,
    vjust = class_missing,
    ...
    ) {
    the_style <- rlang::list2(...)
    color <- the_style$colour %||% color
    label.color <- the_style$label.colour %||% label.color
    text.color <- the_style$text.colour %||% text.color

    d <- list(
      alpha = c(alpha),
      color = c(color),
      angle = c(angle),
      family = c(family),
      fill = c(fill),
      fontface = c(fontface),
      hjust = c(hjust),
      label.color = c(label.color),
      label.padding = ifelse(length(label.padding) > 0,
                             list(label.padding),
                             list()),
      label.margin = ifelse(length(label.margin) > 0,
                            list(label.margin),
                            list()),
      label.r = c(label.r),
      label.size = c(label.size),
      lineheight = c(lineheight),
      nudge_x = c(nudge_x),
      nudge_y = c(nudge_y),
      polar_just = c(polar_just),
      size = c(size),
      text.color = c(text.color),
      vjust = c(vjust)
    )
    d <- get_non_empty_tibble(d)
    if (nrow(d) > 1) {
      return(style_list(purrr::pmap(d, style_label)))
    }

    if (length(label.padding) > 0) {
      label.padding <- list(label.padding)
    } else {
      label.padding <- list()
    }
    if (length(label.margin) > 0) {
      label.margin <- list(label.margin)
    } else {
      label.margin <- list()
    }
    if (S7_inherits(angle, S7_class(angle(0)) )) angle <- angle@degree
    if (length(polar_just) == 1) {
      if (S7_inherits(polar_just, S7_class(angle(0)) ) || is.numeric(polar_just)) {
        polar_just <- polar(theta = radian(polar_just), r = 1.2)
        
      }
      hjust <- polar2just(polar_just@theta, polar_just@r, axis = "h")
      vjust <- polar2just(polar_just@theta, polar_just@r, axis = "v")      
      polar_just <- class_missing
    }


    new_object(
      S7_object(),
      alpha = alpha,
      color = color,
      angle = angle,
      family = family,
      fill = fill,
      fontface = fontface,
      hjust = hjust,
      label.color = label.color,
      label.padding = label.padding,
      label.margin = label.margin,
      label.r = label.r,
      label.size = label.size,
      lineheight = lineheight,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      polar_just = polar_just,
      size = size,
      text.color = text.color,
      vjust = vjust )


  }
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
  validator = function(self) {
    allsameclass(self, "style_base")
  }
)

method(`+`, list(style_base, style_list)) <- function(e1, e2) {
  style_list(purrr::map(e2, \(sl) e1 + sl))
}

method(`+`, list(style_list, style_base)) <- function(e1, e2) {
  style_list(purrr::map(e1, \(sl) sl + e2))
}

method(`+`, list(style_list, class_missing)) <- function(e1, e2) {
  e1
}


style_or_style_list <- new_union(style_base, style_list)


method(get_tibble, style_base) <- function(x) {
  xs <- get_non_empty_props_list_fix(x)
  rlang::inject(tibble::tibble(!!!xs))
}

method(get_tibble, style_list) <- function(x) {
  purrr::map_df(x,get_tibble)
}


