wrapit <- function(x) {
  paste(stri_wrap(x, width = 160), collapse = "\n")
}
