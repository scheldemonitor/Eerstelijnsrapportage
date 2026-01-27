# example R options set globally
options(width = 60)

source("r/functions.R")
library(smwfs)

dataJaar = 2024

refreshData = T

# path to frozen datafiles (downloads from Scheldemonitor that are assoiated with report version)
localDatapath <- "" # local

# saving path only works from within Deltares. Saved files come available via datapath. 
datapath <- file.path("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Scheldemonitor", dataJaar + 1)
savepath <- file.path("p:/11202493--systeemrap-grevelingen/1_data/", "Westerschelde/Scheldemonitor", dataJaar + 1)
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
  cache = FALSE
)

options(bookdown.render.verbose = TRUE)

screenshot.opts= list(delay = 5)

select <- dplyr::select
addLegend <- leaflet::addLegend




bibliographybib <- bibtex::read.bib("bib/westerscheldewillem.bib")
# 
duplicated(names(bibliographybib))
names(bibliographybib)[which(duplicated(names(bibliographybib)))]
# 
# bibtex::write.bib(bibliographybib, "bib/westerschelde.bib")
# # library <- bibtex::read.bib("bib/library.bib")
# # bibtex::write.bib(library, "bib/westerschelde.bib", append = T)


unlink(c(
  "Eerstelijnsrapportage_Westerschelde.aux", "Eerstelijnsrapportage_Westerschelde.toc", "Eerstelijnsrapportage_Westerschelde.out",
  "Eerstelijnsrapportage_Westerschelde.lof", "Eerstelijnsrapportage_Westerschelde.lot", "Eerstelijnsrapportage_Westerschelde.bbl",
  "Eerstelijnsrapportage_Westerschelde.blg", "Eerstelijnsrapportage_Westerschelde.bcf", "Eerstelijnsrapportage_Westerschelde.run.xml"
), force = TRUE)

# Also clear Bookdown cache
unlink("_bookdown_files", recursive = TRUE, force = TRUE)

# Then render again

