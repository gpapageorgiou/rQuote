wrapit <- function(x) {
  paste(stringi::stri_wrap(x, width = 140), collapse = "\n")
}
