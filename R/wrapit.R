wrapit <- function(x) {
  paste(stringi::stri_wrap(x, width = 120), collapse = "\n")
}
