# Read in data from yearly files
mijnPad = "c:\\Users\\ronge_bb\\R\\eerstelijnSchelde\\download\\yearly"
anderPad = "c:\\Users\\ronge_bb\\R\\eerstelijnSchelde\\download"
mijnPattern = ".csv" # of iets anders wat de files karakteriseert die je wilt samenvoegen

allFiles <- list.files(file.path(mijnPad), pattern = mijnPattern, full.names = T)

lapply(
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
      unit,
      value,
      valuesign,
      class) #%>%
) %>%
  bind_rows() %>% # alles wordt geplakt
  write_delim(file.path(anderPad, paste0("Data_Hydro_1950_2020.csv")), delim = ",")