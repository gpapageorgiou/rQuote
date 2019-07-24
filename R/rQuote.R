#' rQuote
#' @description  Prints a random quote extracted from the goodreads.com database
#'
#' @param tag A character string specifying the desired tag to look quotes from
#' @param page_range Integer between 0 and 100 specifying the number of goodreads' pages with the specific tag to look quotes from
#' @export

rQuote <- function(tag = 'science', page_range = 1,
                            cores = detectCores() - 1,
                   OS = c('windows', 'linux', 'macOS'),
                   theme = c('light', 'dark'),
                   popup = TRUE,
                   control = NULL, ...) {

  con <- list(width = 48,
              height = 24)
  control <- c(control, list(...))
  namescon <- names(con)
  con[(namescon <- names(control))] <- control

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
                            quote <- rvest::html_session(urls[i]) %>%
                              html_nodes('div.quoteText') %>%
                              html_text()
                            author <- rvest::html_session(urls[i]) %>%
                              html_nodes('span.authorOrTitle') %>%
                              html_text()
                            quote <- gsub('(.*)[ ][\u201c](.*)[\u201d][\n](.*)', "\\2", quote)
                            author <- gsub('([,\n|\n]*)([^,])', '\\2', author)
                            cbind(quote, author)
                 }
  stopCluster(cl)

  random_row <- sample(nrow(res), 1)
  quote <- res[random_row, 'quote']
  author <- trimws(res[random_row, 'author'])
  author <- paste0("-  ", author)

  if(length(quote) == 0) {
    stop("No quote found. Probably the tag you are searching for does not exist. Change tag and try again.")
  }

  if (popup) {
    linebreaks <- strrep('\n', 4)
    whitespace <- strrep(' ', 180)

    font_add_google('Francois One', 'Francois One')
    font_add_google('Fredericka the Great', 'Fredericka the Great')

    showtext_auto()

    theme <- match.arg(theme)
    if (theme == 'light') {
      outplot <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
        theme(panel.background = element_rect(fill = '#f5f5f5')) +
        geom_text(aes(x = 2, y = 2), label = paste("\u201c", wrapit(quote), "\u201d",
                                                   linebreaks,
                                                   whitespace, author),
                  size = 7, color = "#363636",
                  family = 'Francois One') +
        geom_text(aes(x = 3.75, y = 0), label = paste("\u201c"),
                  size = 50, color = '#363636',
                  family = 'Fredericka the Great') +
        geom_text(aes(x = 4, y = 0), label = paste("\u201d"),
                  size = 50, color = '#363636',
                  family = 'Fredericka the Great')
    } else {
      outplot <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
        theme(panel.background = element_rect(fill = '#363636')) +
        geom_text(aes(x = 2, y = 2), label = paste("\u201c", wrapit(quote), "\u201d",
                                                   linebreaks,
                                                   whitespace, author),
                  size = 7, color = "#f5f5f5" ,
                  family = 'Francois One') +
        geom_text(aes(x = 3.75, y = 0), label = paste("\u201c"),
                  size = 50, color = '#f5f5f5',
                  family = 'Fredericka the Great') +
        geom_text(aes(x = 4, y = 0), label = paste("\u201d"),
                  size = 50, color = '#f5f5f5',
                  family = 'Fredericka the Great')
    }

    OS <- match.arg(OS)
    switch(OS,
           windows = win.graph(width = con$width, height = con$height),
           linux = x11(),
           macOS = quartz())

    print(outplot)

    cat(quote, "\n", "\n", author)
  } else {
    cat(quote, "\n", "\n", author)
  }


}
