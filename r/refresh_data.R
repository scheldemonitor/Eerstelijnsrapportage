
require(tidyverse)
# refresh locally stored data by downloading from Scheldemonitor

dataIDpath <- "datasetIDs.xlsx"
sheets <- readxl::excel_sheets(dataIDpath)

# storage path of downloaded data
datapath = ""

IDs <- lapply(
  sheets, 
  function(x){
    readxl::read_excel(dataIDpath, sheet = x) 
  }
) %>%
  bind_rows()

# Fysisch-chemisch - oppervlaktewater

refresh_fysischchemischoppwater <- function(){
  Saliniteit <- c(998)
  Temperatuur <- c(1046)
  Zuurstof <- c(1214,1213)
  Chlorofyl_a <- c(238,1800)
  BZV_BOD_CZV <- c(125,178)
  Lichtklimaat <- c(461,495)
  Zwevende_stof <- c(1223)
  Nutrienten <- c(26,2829,1996,528,2370,529,530,531,1843,1019,828,834,866,1010,3217,833,1021,1022,1972)
  Organisch_koolstof <- c(663,674)
  Metalen <- c(133,1737,259,260,268,269,1178,2014,1181,2018,1201,1202,686,687)
  
  parID <- c(Saliniteit,Temperatuur,Zuurstof,Chlorofyl_a,BZV_BOD_CZV,Lichtklimaat,Zwevende_stof,Nutrienten,Organisch_koolstof,Metalen)
  
  #df <- get_y_SMdata(2019, 2020, parID)
  df <- smwfs::getSMdata(startyear = startyear, endyear = endyear, parID = parID)
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write.csv(df, file.path(datapath, 'Data_FysChem_opp_test.csv'))
}


# Fysisch-chemisch - zwevende stof

refresh_fysischchemischzwevendstof <- function(){
  parIDs <- IDs %>%
    filter(grepl(" in zwevend stof", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                        'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                        'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(1998,2020, propname = retrievedcolumns, parID = x)) %>%
    delete.NULLs() %>% 
    # map( ~ mutate(.x, id = as.numeric(id))) %>%
    bind_rows()
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write.csv(df, file.path(datapath, 'Data_FysChem_zwevend.csv'))
}

# Fysisch-chemisch - bodem

refresh_fysischchemischbodem <- function(){
  
parIDs <- IDs %>%
  filter(grepl("in bodem/sediment", Parameternaam, ignore.case = T)) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                          'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                          'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")

df <- lapply(parIDs, function(x) getSMdata(1998,2020, propname = retrievedcolumns, parID = x)) %>%
  delete.NULLs() %>% 
  bind_rows()

df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, file.path(datapath, 'Data_FysChem_bodem.csv'))

}

refresh_fysischchemischbiota <- function(){
  
  parIDs <- IDs %>%
    filter(grepl("biota", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                            'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit', 'class', 'category',
                            'scientificname', 'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(1998,2020, propname = NULL, parID = x)) %>%
    delete.NULLs() %>% 
    bind_rows()
  
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  write.csv(df, file.path(datapath, 'Data_FysChem_biota.csv'))
  
}

