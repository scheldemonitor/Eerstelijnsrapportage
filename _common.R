# example R options set globally
options(width = 60)

source("r/functions.R")
library(smwfs)

# year for reporting (last data year)
dataJaar <- 2022

# path to frozen datafiles (downloads from Scheldemonitor that are assoiated with report version)
localDatapath <- "" # local

# saving path only works from within Deltares. Saved files come available via datapath. 
datapath <- file.path("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Scheldemonitor", dataJaar + 1)

# chunk options set globally

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  out.width = "100%",
  fig.align='center',
  fig.width = 7,
  cache = TRUE
)

options(bookdown.render.verbose = TRUE)

screenshot.opts= list(delay = 5)

select <- dplyr::select
addLegend <- leaflet::addLegend


# bibliographybib <- bibtex::read.bib("bib/library.bib")

# bibliographybib2 <- bibtex::read.bib("bib/westerscheldewillem.bib")
# bibliographybib2 <- bibtex::read.bib("bib/westerscheldewillem.bib")
