% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rQuote.R
\name{rQuote}
\alias{rQuote}
\title{rQuote}
\usage{
rQuote(tag = "science", theme = c("light", "dark"),
  fonts = "Francois One", popup = TRUE, cores = detectCores() - 1,
  OS = c("windows", "linux", "macOS"), control = NULL, ...)
}
\arguments{
\item{tag}{A character string specifying the desired tag to look quotes from. Defaults to 'science'.}

\item{theme}{Character string specifying whether the light or the dark theme should be used for the pop-up plot window. Defaults to 'light'.}

\item{fonts}{Character string specifying the desired Google fonts to use. Fonts are installed using function \code{\link[sysfonts]{font_add_google}} from package \strong{sysfonts}.}

\item{popup}{Logical; if TRUE a pop-up plot window with the quote appears. Defaults to 'TRUE'.}

\item{cores}{Integer specifying the number of cores to be used by the function. Defaults to the number of cores detected on the current host minus 1.}

\item{OS}{Character string specifying the Operating System of the current host. Defaults to windows.}

\item{control}{a list of control values with components: \itemize{
\item{page_range: Integer specifying the number of Goodreads' pages to look quotes from. Defaults to 1. Must be greater than zero and less or equal to 100.}
\item{width: width of plotting device in inches. Defaults to 48.}
\item{height: height of plotting device in inches. Defaults to 24.}
\item{text_size: the size of the fonts used to plot the quote in mm. Defaults to 7.}
}}
}
\description{
Prints a random quote extracted from goodreads.com
}
\details{
Argument \strong{tag} refers to the available user defined tags in goodreads.com.
They consist of a single word with lowercase characters (e.g 'politics', 'sports', ...).
They do not contain spaces, special characters or uppercase letters. If the user inputs a tag that
does not exist in goodreads.com, then no quote will be found and an error message will appear.
}
\author{
Grigorios Papageorgiou \email{g.papageorgiou@erasmusmc.nl}
}
