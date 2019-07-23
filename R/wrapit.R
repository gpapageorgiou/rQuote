wrapit <- function(x) {
  paste(stri_wrap(x, width = 120), collapse = "\n")
}
