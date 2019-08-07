pagemax <- function(url) {
  max_pages <- html_nodes(read_html(url), 'a')
  max_pages <- max_pages[grep('page=', max_pages)]
  max_pages <- gsub('(.*)[>]([0-9]*)[<](.*)', '\\2', max_pages)
  max_pages <- max_pages[-length(max_pages)]
  max_pages <- as.integer(max_pages)
  max_pages
}
