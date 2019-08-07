#' rQuote
#' @description  Prints a random quote extracted from goodreads.com
#'
#' @param tag A character string specifying the desired tag to look quotes from. Defaults to 'science'.
#' @param cores Integer specifying the number of cores to be used by the function. Defaults to the number of cores detected on the current host minus 1.
#' @param OS Character string specifying the Operating System of the current host. Defaults to windows.
#' @param theme Character string specifying whether the light or the dark theme should be used for the pop-up plot window. Defaults to 'light'.
#' @param popup Logical; if TRUE a pop-up plot window with the quote appears. Defaults to 'TRUE'
#' @param control a list of control values with components: \itemize{
#' \item{page_range: Integer specifying the number of Goodreads' pages to look quotes from. Defaults to 1. Must be greater than zero and less or equal to 100.}
#' \item{width: width of plotting device in inches. Defaults to 48}
#' \item{height: height of plotting device in inches. Defaults to 24}
#' \item{text size: the size of the fonts used to plot the quote in mm. Defaults to 7}
#' }
#'
#' @details Argument \strong{tag} refers to the available user defined tags in goodreads.com.
#' They consist of a single word with lowercase characters (e.g 'politics', 'sports', ...).
#' They do not contain spaces, special characters or uppercase letters. If the user inputs a tag that
#' does not exist in goodreads.com, then no quote will be found and an error message will appear.
#'
#' @author Grigorios Papageorgiou \email{g.papageorgiou@@erasmusmc.nl}
#' @export

rQuote <- function(tag = 'science',
                   cores = detectCores() - 1,
                   OS = c('windows', 'linux', 'macOS'),
                   theme = c('light', 'dark'),
                   popup = TRUE,
                   fonts = 'Francois One',
                   control = NULL, ...) {

  con <- list(page_range = 1,
              width = 48,
              height = 24,
              text_size = 7)
  control <- c(control, list(...))
  namescon <- names(con)
  con[(namescon <- names(control))] <- control

  # check page range
  if (con$page_range > 100) {
    stop('Page range should be equal or less than 100')
  }

  # create initial url according to tag input
  url <- paste0('https://www.goodreads.com/quotes/tag/', tag)

  # find maximum number of pages
  max_pages <- pagemax(url)

  if (length(max_pages) == 0) {
    stop(paste('Unfortunately there are no quotes classified as',
               tag, '.\n', 'Please try another tag and make sure that it is a single word with lowercase characters.'))
  } else {
    max_pages <- max(max_pages)
  }

  if (con$page_range < max_pages) {
    pages <- sample(1:max_pages, con$page_range, replace = FALSE)
  } else if (con$page_range == max_pages) {
    pages <- 1:con$page_range
  } else {
    stop(paste('The maximum number of pages for the tag = ', '"', 'tag', '"', 'is', max_pages, '\n',
               'Try a page range equal or less than', max_pages))
  }

  # create list of urls
  urls <- lapply(pages, FUN = function (x, url) paste0(url, '?page=', x), url)
  urls <- do.call(c, urls)

  #
  cl <- makeCluster(cores)
  registerDoParallel(cl)

  res <- foreach(i = seq_along(urls),
                          .packages = c("rvest", 'xml2'),
                          .combine = rbind) %dopar% {
                            quote <- html_nodes(read_html(urls[i]), 'div.quoteText') %>%
                              html_text()
                            author <- html_nodes(read_html(urls[i]), 'span.authorOrTitle') %>%
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

  theme <- match.arg(theme)

  if (popup) {
    rQuoteMe(quote, author, theme, fonts, OS = OS)
    cat(quote, "\n", "\n", author)
  } else {
    cat(quote, "\n", "\n", author)
  }

}
