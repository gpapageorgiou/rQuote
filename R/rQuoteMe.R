#' rQuoteMe
#' @description  Plots a quote in a graphics device
#'
#' @param quote A character string specifying the quote to plot
#' @param author A character string specifying the author of the quote
#' @param theme A character string specifying whether the light or the dark theme should be used for the pop-up plot window. Defaults to 'light'.
#' @param fonts A character string specifying the google fonts to be used
#' @param OS Character string specifying the Operating System of the current host. Defaults to windows.
#' @param control a list of control values with components: \itemize{
#' \item{width: width of plotting device in inches. Defaults to 48.}
#' \item{height: height of plotting device in inches. Defaults to 24.}
#' \item{text size: the size of the fonts used to plot the quote in mm. Defaults to 7.}
#' }
#'
#' @details
#'
#' @author Grigorios Papageorgiou \email{g.papageorgiou@@erasmusmc.nl}
#' @export

rQuoteMe <- function(quote, author, theme = 'light',
                     fonts = 'Francois One', OS = c('windows', 'linux', 'macOS'),
                     control = list(), ...) {

  con <- list(width = 48,
              height = 24,
              text_size = 7)
  control <- c(control, list(...))
  namescon <- names(con)
  con[(namescon <- names(control))] <- control

  linebreaks <- strrep('\n', 4)
  whitespace <- strrep(' ', 140)

  font_add_google(fonts, fonts)
  font_add_google('Fredericka the Great', 'Fredericka the Great')

  showtext_auto()

  con$text_size <- ifelse(nchar(quote) > 1750, 5, con$text_size)

  if (theme == 'light') {
    rquote <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
      theme(panel.background = element_rect(fill = '#f5f5f5')) +
      geom_text(aes(x = 2, y = 2), label = paste("\u201c", wrapit(quote), "\u201d",
                                                 linebreaks,
                                                 whitespace, author),
                size = con$text_size, color = "#363636",
                family = fonts) +
      geom_text(aes(x = 3.75, y = 0), label = paste("\u201c"),
                size = 50, color = '#363636',
                family = 'Fredericka the Great') +
      geom_text(aes(x = 4, y = 0), label = paste("\u201d"),
                size = 50, color = '#363636',
                family = 'Fredericka the Great')
  } else {
    rquote <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void() +
      theme(panel.background = element_rect(fill = '#363636')) +
      geom_text(aes(x = 2, y = 2), label = paste("\u201c", wrapit(quote), "\u201d",
                                                 linebreaks,
                                                 whitespace, author),
                size = con$text_size, color = "#f5f5f5" ,
                family = fonts) +
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

  print(rquote)
}
