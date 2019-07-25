wrapit <- function(x) {
  paste(stringi::stri_wrap(x, width = 138), collapse = "\n")
}
