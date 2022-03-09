dataset_omschrijving <- "MWTL biological monitoring network Westerschelde: Macrofauna"
dataset_link <- "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+imisdatasetid+IN+%28587%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle&outputFormat=csv"

make_link_with_login <- function(lnk, login='3A0001%3Bloggedin%3A1'){
  gsub(pattern = '3A0001', replacement = login, x = lnk)
}

# install package for Scheldemonitor
local_path_to_smwfs <- "c:\\Users\\ronge_bb\\R\\smwfs\\smwfs_0.1.0.tar.gz"
install.packages(local_path_to_smwfs, repos = NULL, type="source")
require(smwfs)

# Voor het aanpassen van de functie buiten het package om
getSMdata_new <- function(startyear, endyear, parID, datasetID = c(588, 500, 479, 135, 1527, 476, 945)) {
  require(httr)
  require(sf)
  require(tidyverse)
  require(readr)
  if (any(!is.na(datasetID))) {
    print("you are currently searching for data in datasetID")
    print(datasetID)
    print("if you want to search in all datasets, choose datasetID = NA")
    print("if you want to search in other datasets, provide a vector of ID's")
  }
  urllist <- structure(list(scheme = "http", hostname = "geo.vliz.be", 
                            port = NULL, path = "geoserver/wfs/ows", query = list(service = "WFS", 
                                                                                  version = "1.1.0", request = "GetFeature", 
                                                                                  typeName = "Dataportal:abiotic_observations", 
                                                                                  resultType = "results", viewParams = "placeholder", 
                                                                                  #propertyName = "stationname,longitude,latitude,datetime,depth,parametername,valuesign,value,dataprovider,datasettitle", 
                                                                                  outputFormat = "csv"), params = NULL, fragment = NULL, 
                            username = NULL, password = NULL), class = "url")
  viewParams <- paste("where:obs.context+&&+ARRAY[1]+AND+standardparameterid+IN+(", 
                      stringr::str_replace_all(paste(parID, collapse = ","), 
                                               ",", "\\\\,"), ")+", ifelse(any(!is.na(datasetID)), 
                                                                           paste0("AND+imisdatasetid+IN+(", paste(datasetID, 
                                                                                                                  collapse = "\\,"), ")+"), ""), 
                      "AND+((datetime_search+BETWEEN+'", startyear, "-01-01'+AND+'", 
                      endyear, "-12-31'+))", ";context:0001", ";loggedin:1", 
                      sep = "")
  urllist$query$viewParams <- viewParams
  urllist$query$viewParams <- stringr::str_replace_all(urllist$query$viewParams, 
                                                       "\\+", " ")
  downloadURL = httr::build_url(urllist)
  result <- RETRY("GET", url = downloadURL, times = 3) %>% 
    content(., "text") %>% read_csv(guess_max = 1e+05) %>% 
    mutate(value = na_if(value, "999999999999")) %>% 
    mutate(value = as.numeric(value))
  return(result)
}

# Stel jaren in
startyear_hydro = 1950
startyear = 1998
endyear = 2020

# Hydrodynamiek
Waterstand <- c(2832)
Getij <- c()
Golven <- c(1814,1816,1818,1819)
#parID <- c(Waterstand, Getij, Golven)

Test <- c(2438,2439,9695,9694,9696,1822,2437)
Testwaterstand <- c(2832,1225,2587,5385,9679)
Testgolven <- c(1814,1818,1819)
parID <- c(Test, Testwaterstand,Testgolven)

#df <- smwfs::getSMdata(startyear = startyear, endyear = endyear, parID = parID)
for(jaar in 1950:2020){
  df <- getSMdata_new(startyear = jaar, endyear = jaar + 1, parID = parID)
  write.csv(df, paste('Data_Hydro_test',jaar,'.csv', sep = ""))
}

# Fysisch-chemisch - oppervlaktewater
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
write.csv(df, 'Data_FysChem_opp_test.csv')

# Fysisch-chemisch - zwevende stof
Korrelgrootte_zwevend <- c(722,720)
Metalen_zwevend <- c(36,156,256,683,727,739,1198,1845,2016)
Micro_zwevend <- c(567,593,598,880,912,886,904,33,82,102,74,108,264,497,519,629,1891,587,2002,1780)

parID <- c(Korrelgrootte_zwevend, Metalen_zwevend, Micro_zwevend)

df <- getSMdata(1998,2020,parID)
df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, 'Data_FysChem_zwevend_test.csv')

# Fysisch-chemisch - bodem
Korrelgrootte_bodem <- c(721,719,1909,717,666)
Metalen_bodem_fractie <- c(38,158,258,685,729,741,1200)
#Metalen_bodem <- c(42,162,262,689,733,745,1204)
Micro_bodem_fractie <- c(568,594,590,918,913,919,905,1959,34,83,103,75,110,266,499,521,631)
#Micro_bodem <- c(564,589,590,918,1066,919,905,31,85,100,77,109,265,501,520,630)
Overig_bodem <- c(2003,1113,1116)

parID <- c(Korrelgrootte_bodem, Metalen_bodem_fractie, Micro_bodem_fractie, Overig_bodem)

df <- smwfs::getSMdata(startyear = startyear, endyear = endyear, parID = parID)
#df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, 'Data_FysChem_bodem_test.csv')

# Fysisch-chemisch - biota
Metalen_biota_bot <- c(2291,2329,2331,2335,2387)
PCB_biota_bot <- c(2490,2491,2492,2493,2504,2502,2496,2499,2501,2506,2503,2507,2510,2516,2520,2518,2521,2527,2525,2626,2523,2524,2535,2534,2549)
PCB_biota_bot_alles <- c(2198,2199,2201,2202,2217,2214,2206,2210,2212,2219,2216,2220,2226,2231,2237,2234,2239,2248,2245,2246,2242,2244,2259,2257,2362)
PBDE_biota <- c(2510,2512,2513,2514,2515,2533)
PBDE_biota_alles <- c(2224,2227,2228,2229,2230,2255)
HCB_biota <- c(2541,2542,2322,2688)
Metalen_biota_mossel <- c()
PCB_biota_mossel <- c()

parID <- c(Metalen_biota_bot,PCB_biota_bot_alles,PBDE_biota_alles,HCB_biota,Metalen_biota_mossel,PCB_biota_mossel)

#df <- smwfs::getSMoccurenceData(2019,2020,parID)
#df <- getSMdata(1998,2020,parID)
df <- getSMdata_new(1998,2020,parID)
df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, 'Data_FysChem_biota_test.csv')

# Fytoplankton
Autotroof <- c(2268,2472)
Heterotroof <- c()
Mixotroof <- c()

parID <- c(Autotroof,Heterotroof,Mixotroof)

#df <- getSMdata(2016,2020,parID)
df <- getSMdata_new(1998,2020,parID)
df <- df[!df$dataprovider == "8", ] # remove metingen van scan-tochten
write.csv(df, 'Data_Fyto_test.csv')

fytoplankton <- getSMDataset(2000, 2020, 949)
write_delim(fytoplankton, "n:/Projects/1209000/1209394/C. Report - advise/Eerstelijnsrapportage/2021/Data/fytoplankton.csv", delim = ";")
