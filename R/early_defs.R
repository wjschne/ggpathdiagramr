# classes ----
ggplot_class <- S7::new_S3_class("ggplot")
xy <- S7::new_class("xy", abstract = TRUE)


# unions ----
numeric_or_character <- S7::new_union(S7::class_numeric, S7::class_character)

# internal states ----
the <- new.env(parent = emptyenv())
the$point <- c("alpha", "color", "fill", "shape", "size", "stroke")
the$line <- c("alpha", "color", "stroke", "lineend", "linejoin", "linetype", "linewidth")
the$polygon <- c("alpha", "color", "fill", "linetype", "linewidth")
the$text <- c("alpha", "color", "family", "fontface", "hjust", "size", "size.unit", "vjust")

