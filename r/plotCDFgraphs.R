require(tidyverse)
source("r/functions.R")

trendstations <- c("Bath (BAT2)","Hansweert (HAWI)","Cadzand (CAWI)","Cadzand (CADW)","Overloop van Hansweert (OVHW)","Deurloo (DEUR)","Honte (HNTE)","Hoofdplaat (HFPL)","Overloop van Valkenisse (OVVA)","Pas van Terneuzen (PVT)","Wielingen (WIEL)")

frozendatapath <- "n:/Projects/1209000/1209394/C. Report - advise/Eerstelijnsrapportage/2021/Data/Data_Hydro_Golven_1998_2020.csv"

refresh = F
if(refresh) refresh_golven()

df <- read_delim(frozendatapath, delim = ",", guess_max = 200000) %>% 
  filter(stationname %in% trendstations) %>%
  mutate(stationname = factor(stationname, levels = trendstations)) %>%
  mutate(value = case_when(
    str_detect(parametername, "TH3") ~ value / 10,
    str_detect(parametername, "H3") ~ value,
  )) %>%
  mutate(parametername = case_when(
    str_detect(parametername, "TH3") ~ "TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in s",
    str_detect(parametername, "H3") ~ "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm",
))

# CDF plot voor golfhoogte
p1 <- plotCDF(df, "H3: Gemiddelde hoogte van het 1/3 deel hoogste golven in cm")
ggsave(filename="n:/Projects/1209000/1209394/C. Report - advise/Eerstelijnsrapportage/2021/Figuren/cdf_H3.png", plot=p1)

# CDF plot voor golfperiode
p2 <- plotCDF(df, "TH3: Gemiddelde periode van de golven waaruit H3 bepaald is in s")
ggsave(filename="n:/Projects/1209000/1209394/C. Report - advise/Eerstelijnsrapportage/2021/Figuren/cdf_TH3.png", plot=p2)
