# example R options set globally
options(width = 60)

# chunk options set globally

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  out.width = "100%",
  fig.align='center',
  fig.width = 8,
  cache = TRUE
)

screenshot.opts= list(delay = 5)

select <- dplyr::select
addLegend <- leaflet::addLegend

dataJaar <- 2020

# bibliographybib <- bibtex::read.bib("bib/library.bib")
# bibliographybib2 <- bibtex::read.bib("bib/westerscheldewillem.bib")