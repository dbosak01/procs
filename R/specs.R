
span_spec <- function(label, start, end, level) {

  # Create new structure of class "span_spec"
  x <- structure(list(), class = c("span_spec", "list"))
  x$label = label
  x$start = start
  x$end = end
  x$level = level

  return(x)
}

title_spec <- function(titles, align = 'center',
                       bold = FALSE, font_size = NULL) {

  # Create new structure of class "title_spec"
  x <- structure(list(), class = c("title_spec", "list"))

  x$titles <- titles
  x$align <- align
  x$bold <- bold
  x$font_size <- font_size

  return(x)
}
