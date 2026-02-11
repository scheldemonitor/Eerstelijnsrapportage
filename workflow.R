source("_common.R")
source("r/refresh_data.R")

update = FALSE

if(update){
  ## workflow 
  # plan
  # script to run complete workflow for yearly update
  
  
  ## update version
  # e.g. set dataJaar
  
  
  ## update data
  # fetch all data needed for a new report
  
  
  # waterstanden
  refresh_waterstanden(startjaar = 2023, datajaar = dataJaar)
  
  # golven
  refresh_golven(
    startjaar = 2022, # only when partly update is done
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
