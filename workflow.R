
update_to = 2025

stopifnot(newversion > existingversion)

update = FALSE

if(update){
  ## workflow 
  # plan
  # script to run complete workflow for yearly update
  
  
  ## update version
  # e.g. set dataJaar
  
  update.year <- function(report_year = 2025){
    dataJaar <- report_year - 1
  }
  
  if(passes) version = version.required
  
  ## update data
  # fetch all data needed for a new report
  
  source("r/refresh_data.R")
  
  # waterstanden
  refresh_waterstanden(datajaar = dataJaar)
  
  # golven
  refresh_golven(
    startjaar = 2018, # only when partly update is done
    datajaar = dataJaar
  )
  
  # oppervlaktewater parameters
  refresh_fysischchemischoppwater(startyear = 1998, endyear = dataJaar) 
  
  # zwevend stof parameters
  fysChemZwevendDataPath <- "Data_FysChem_zwevend.csv"
  fysChemZwevendDataPath2 <- "Data_FysChem_zwevend2.csv"
  
  refresh_fysischchemischzwevendstof(startyear = 1998, endyear = dataJaar, filepath = fysChemZwevendDataPath)
  
  # bodemparameters
  fysChemBodemDataPath <- "Data_FysChem_bodem.csv"
  refresh_fysischchemischbodem(startyear = 1998, endyear = dataJaar, filepath = fysChemBodemDataPath) 
  
  # biotaparameters
  frozendataPath <- "Data_FysChem_biota.csv"
  refresh_fysischchemischbiota(endyear = dataJaar+2, filepath = frozendataPath) # gek.. + 2?
  
  # fytoplanktonparameters
  refresh_fytoplanktondata()
  
  ## check data
  # Check if all expected data are available
  
  ## build html report
  
  bookdown::render_book()
  
  ## visual inspection
  
  ## update narrative manually
  

  ## generate pull request with review

  
}
