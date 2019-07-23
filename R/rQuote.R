rQuote <- function(tag = 'science', page_range = 10,
                            cores = detectCores() - 1,
                   OS = c('windows', 'linux', 'macOS'),
                   theme = c('light', 'dark')) {

  # check page range
  if (page_range > 100) {
    stop('Page range should be equal or less than 100')
  }

  if (page_range < 100) {
    pages <- sample(1:100, page_range, replace = FALSE)
  } else {
    pages <- 1:page_range
  }

  # create initial url according to tag input
  url <- paste0('https://www.goodreads.com/quotes/tag/', tag)

  # create list of urls
  urls <- lapply(pages, FUN = function (x, url) paste0(url, '?page=', x), url)
  urls <- do.call(c, urls)

  #
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  res <- foreach(i = seq_along(urls),
                 .packages = c("rvest", 'xml2'),
                 .combine = rbind) %dopar% {
                  quote <- html_session(urls[i]) %>% html_nodes('div.quoteText') %>% html_text()
                  author <- html_session(urls[i]) %>% html_nodes('span.authorOrTitle') %>% html_text()
                  #quote <- gsub('(.*)[ ]["](.*)["][\n](.*)', "\\2", quote)
                  quote <- gsub('(.*)[ ][\u201c](.*)[\u201d][\n](.*)', "\\2", quote)
                  author <- gsub('([,\n|\n]*)([^,])', '\\2', author)
                  #author <- gsub('(.*)[ ]([:upper:].*[:lower:])[,?\n *]', '\\1', author)
                  cbind(quote, author)
                 }
  stopCluster(cl)

  random_row <- sample(nrow(res), 1)
  quote <- res[random_row, 'quote']
  author <- trimws(res[random_row, 'author'])
  author <- paste0("-  ", author)

  linebreaks <- strrep('\n', 4)
  whitespace <- strrep(' ', 100)

  font_add_google('Francois One', 'Francois One')

  showtext_auto()

  cat(quote, "\n", "\n", author)

  theme <- match.arg(theme)
  if (theme == 'light') {
    outplot <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
      theme(panel.background = element_rect(fill = '#f5f5f5')) +
      geom_text(aes(x = 2, y = 2), label = paste("“", wrapit(quote), "”",
                                                 linebreaks,
                                                 whitespace, author),
                size = 6, color = "#363636",
                family = 'Francois One')
  } else {
    outplot <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
      theme(panel.background = element_rect(fill = '#363636')) +
      geom_text(aes(x = 2, y = 2), label = paste("“", wrapit(quote), "”",
                                                 linebreaks,
                                                 whitespace, author),
                size = 6, color = "#f5f5f5" ,
                family = 'Francois One')
  }

  OS <- match.arg(OS)
  switch(OS,
         windows = win.graph(width = 18, height = 9),
         linux = x11(),
         macOS = quartz())

  print(outplot)
}
