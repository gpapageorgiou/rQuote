wrapit <- function(x) {
  paste(stringi::stri_wrap(x, width = 130), collapse = "\n")
}
