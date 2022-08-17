
# Script to directly translate tex to html using pandoc
# Can also be done in commandline pandoc
# This is the quickest way to generate html pages from tex.
# Can styling be added using css?

dir.create("RapportMD")
setwd("Rapport")
getwd()
knitr::pandoc("Eerstelijnsrapportage westerschelde 2018.tex", 'html')

# werkt wel, maar is rommelig. warnings. figuren te groot, geen handige index etc. 


