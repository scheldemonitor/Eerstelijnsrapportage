
require(tidyverse)
# refresh locally stored data by downloading from Scheldemonitor

source("r/functions.r")

dataIDpath <- "datasetIDs.xlsx"
sheets <- readxl::excel_sheets(dataIDpath)


IDs <- lapply(
  sheets, 
  function(x){
    readxl::read_excel(dataIDpath, sheet = x) 
  }
) %>%
  bind_rows()

# Hydrodynamiek - waterstanden

refresh_waterstanden <- function(datajaar){
  Waterstand <- c(
    9695,9694, # HW, LW NAP
    10873,10874, # HW, LW MSL
    2438, 2439   # Amplitude, fase
  ) 
  for(jaar in 1998:datajaar){
    df <- smwfs::getSMdata(startyear = jaar, endyear = jaar + 1, parID = c(Waterstand), datasetID = c(476,1527,945))
    write.csv(df, file = file.path(savepath, paste0("Data_Hydro_waterstanden_", jaar,'.csv', sep = "")), row.names = F)
  }
  
  allFiles <- list.files(file.path(savepath), pattern = "Data_Hydro_waterstanden_", full.names = T)
  allFiles <- allFiles[!grepl("all", allFiles)]

  df <- lapply(
    allFiles, function(x) # nameless function. Wat hierna staat wordt uitgevoerd voor elke elemente van allFiles
      read_delim(x, delim = ",", col_types = cols(.default = "c",
                                                  datetime = "T",
                                                  latitude = "d",
                                                  longitude = "d",
                                                  value = "d")) %>%
      select( # kolomnamen van kolommen die je wilt behouden
        stationname,
        latitude,
        longitude,
        datetime,
        parametername,
        class,
        value) #%>%
  ) %>% bind_rows() # alles wordt geplakt
  
  write_delim(df, file.path(savepath, paste0("Data_Hydro_waterstanden_all_", ".csv")), delim = ",")
  rm(df)
  
}

# Hydrodynamiek - golven

refresh_golven <- function(datajaar){
  Golven <- c(
    2599,2601, # Hm0
    1816, 2594, # H3, TH3
    2596,2597,2598 # TM02
  )
  for(jaar in 2014:datajaar){
    df <- smwfs::getSMdata(startyear = jaar, endyear = jaar + 1, parID = c(Golven), datasetID = c(8032))
    write.csv(df, file.path(savepath,paste0("Data_Hydro_golven_", jaar,'.csv')), row.names = F)
  }

  # bewerkingen
  
allFiles <- list.files(file.path(savepath), pattern = "Data_Hydro_golven_", full.names = T)
df <- lapply(
  allFiles, function(x) # nameless function. Wat hierna staat wordt uitgevoerd voor elke elemente van allFiles
    read_delim(x, delim = ",", col_types = cols(.default = "c",
                                                datetime = "T",
                                                latitude = "d",
                                                longitude = "d",
                                                value = "d")) %>%
    select( # kolomnamen van kolommen die je wilt behouden
      stationname,
      latitude,
      longitude,
      datetime,
      parametername,
      value) #%>%
) %>% bind_rows() # alles wordt geplakt

cdf_H3 <- plotCDF(df, "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm")
ggsave(cdf_H3, width = 7, height = 4, filename = "Figuren/cdf_H3.png")
cdf_th3 <- plotCDF(df,"TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in 0.1 s")
ggsave(cdf_th3, width = 8, height = 4, filename =  "Figuren/cdf_TH3.png")

df2 <- df %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(stationname, parametername, year, month) %>% 
  summarize(mean = mean(value), max = max(value), n = n(), latitude = mean(latitude), longitude = mean(longitude)) %>%
  mutate(datum = lubridate::ymd(paste(year, month, "15"))) %>%
  select(stationname,
         latitude,
         longitude,
         parametername,
         datetime = datum,
         value = mean,
         value_max = max,
         datapoints = n) %>%
  filter(stationname %in% trendstations) %>%
  mutate(stationname = factor(stationname, levels = trendstations)) %>%
  mutate(parametername = case_when(
    str_detect(parametername, "TH3") ~ "TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in 0.1 s",
    str_detect(parametername, "H3") ~ "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm",
    str_detect(parametername, "TM02") ~ "TM02: Golfperiode berekend uit het spectrum in 0.1 s",
    str_detect(parametername, "Hm0") ~ "Hm0: Significante golfhoogte uit 10mHz spectrum in cm"))

write_delim(df2, file.path(savepath, paste0("Data_Hydro_golven_all", ".csv")), delim = ",")
rm(df)

}



# Fysisch-chemisch - oppervlaktewater

refresh_fysischchemischoppwater <- function(startyear = 1998, endyear, filepath = "Data_FysChem_opp.csv"){
  Saliniteit <- c(13611) #998
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
  df <- smwfs::get_y_SMdata(startyear = startyear, endyear = endyear, parID = parID)
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  if (nrow(df) != nrow(df %>% select(-FID) %>% distinct())) {
    df <- df %>% select(-FID) %>% distinct()
  }
  write.csv(df, file.path(savepath, filepath), row.names=FALSE)
}


# Fysisch-chemisch - zwevende stof

refresh_fysischchemischzwevendstof <- function(startyear = 1998, endyear, filepath = 'Data_FysChem_zwevend.csv'){
  parIDs <- IDs %>%
    filter(grepl(" in zwevend stof", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                        'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                        'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(startyear,endyear, propname = retrievedcolumns, parID = x)) %>%
    delete.NULLs() %>% 
    # map( ~ mutate(.x, id = as.numeric(id))) %>%
    bind_rows()
  df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  if (nrow(df) != nrow(df %>% select(-FID) %>% distinct())) {
    df <- df %>% select(-FID) %>% distinct()
  }
  write.csv(df, file.path(savepath, filepath), row.names = FALSE)
}

# Fysisch-chemisch - bodem

refresh_fysischchemischbodem <- function(startyear = 1998, endyear, filepath = "Data_FysChem_bodem.csv"){
  
parIDs <- IDs %>%
  filter(grepl("in bodem/sediment", Parameternaam, ignore.case = T)) %>%
  select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
  unlist %>% unname

retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                          'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit',
                          'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")

df <- lapply(parIDs, function(x) getSMdata(startyear, endyear, propname = retrievedcolumns, parID = x)) %>%
  delete.NULLs() %>% 
  bind_rows()

df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
if (nrow(df) != nrow(df %>% select(-FID) %>% distinct())) {
  df <- df %>% select(-FID) %>% distinct()
}
write.csv(df, file.path(savepath, filepath), row.names = F)

}

refresh_fysischchemischbiota <- function(startyear = 1998, endyear, filepath = "Data_FysChem_biota.csv"){
  
  parIDs <- IDs %>%
    filter(grepl("biota", Parameternaam, ignore.case = T)) %>%
    select(`(Parameternr)`) %>% arrange(`(Parameternr)`) %>%
    unlist %>% unname
  
  retrievedcolumns <- paste('latitude', 'longitude', 'depth', 'datetime', 'value', 'standardparameterid',
                            'dataproviderid', 'imisdatasetid', 'parametername', 'parameterunit', 'class', 'category',
                            'scientificname', 'dataprovider', 'stationname', 'unit', 'valuesign', sep = ",")
  
  df <- lapply(parIDs, function(x) getSMdata(startyear, endyear, propname = NULL, parID = x)) %>%
    delete.NULLs() %>% 
    bind_rows()
  
  # df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
  #if (nrow(df) != nrow(df %>% select(-FID) %>% distinct())) {
  #  df <- df %>% select(-FID) %>% distinct()
  #}
  write_csv(df, file = file.path(savepath, filepath))
}



refresh_fytoplanktondata <- function(){
  
  url = "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+imisdatasetid+IN+%28949%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle%2Cdataficheid%2Careaname%2Cdateprecision%2Cstadium%2Cgender%2Cvaluesign%2Cdepth%2Cclassunit%2Cclass%2Cstandardparameterid%2Cparameterunit&outputFormat=csv"

  df.fytoplankton <- read_csv(url)
  if (nrow(df.fytoplankton) != nrow(df.fytoplankton %>% select(-FID) %>% distinct())) {
    df.fytoplankton <- df.fytoplankton %>% select(-FID) %>% distinct()
  }
  write.csv(df.fytoplankton, file.path(savepath, paste0('fytoplankton', dataJaar, '.csv')), row.names = F)
  
  
  }

