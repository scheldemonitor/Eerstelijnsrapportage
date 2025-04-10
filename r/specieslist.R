#Make a new species list using the WoRMS Taxon match tool, the protist table and mixoplankton table 

require(readr)
require(purrr)
require(tidyverse)
require(readxl)

# Load the previous species list
specieslist <- read_tsv(
  file.path('https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Scheldemonitor/2021', "Specieslist_zelfuitgebreid_jh.csv"),
  col_names = F)

names(specieslist) <- c(
  "soortnaam",
  "soortcode",
  "TWN",
  "TWN2",
  "soortnaam2",
  "trofie",
  "groep",
  "groepcode"
)

# Load in the result from the WoRMS Taxon match tool on a missing species list
species <- read_csv("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Specieslist/missingspecies2022_matched.csv")


species <- species %>% mutate(groep = rep(NA, nrow(species))) %>% 
  mutate(groep = case_when(
  Phylum == 'Cyanobacteria' ~ 'Blauwwieren',
  Class == 'Cyanophyceae' ~ "Blauwwieren", 
  Class %in% c('Chlorophyceaea', 'Prasinophyceae', 'Euglenophyceae') ~ 'Groenwieren',
  Class == 'Bacillariophyceae'  ~ 'Diatomeeen',
  Class %in% c('Ellobiophyceae', 'Psammosea', 'Oxyrrhea', 'Pronoctilucea', 'Duboscquellea', 
               'Syndiniophyceae', 'Noctiluciphyceae', 'Dinophyceae') ~ 'Dinoflagellaten',
  Genus == 'Phaeocystis' ~ 'Phaeocystis', 
  ScientificName...2 == 'Gymnodiniales' ~ 'Dinoflagellaten',
  ScientificName...2 == 'Prymnesiales' ~ 'Overig',
  ScientificName...2 =='Leptocylindraceae' ~ 'Diatomeeen', 
  ScientificName...2 == 'Scenedesmaceae' ~ 'Groenwieren', 
  ScientificName...2 == 'Thoracosphaeraceae' ~ 'Dinoflagellaten', 
  ScientificName...2 == 'Plagiotropidaceae' ~ "Diatomeeen", 
  #is.na(AphiaID) | ScientificName...2 == 'Khakista' ~ groep, # keep the current group if AphaID is NA or ScientificName...2 is 'Khakista'
  ScientificName...2 == 'Khakista' ~ 'Diatomeeen',
  TRUE ~ 'Overig'
))
#species_append <- data.frame(matrix(ncol=length(names(specieslist)), nrow=length(nrow(species))))
#names(species_append) <- names(specieslist)
#species_append <- species_append %>% mutate(soortnaam = species$ScientificName...2, 
#                                            groep = species$groep)


species_append <- species %>% filter(!is.na(...1)) %>% 
  select(soortnaam = ScientificName...2, groep = groep)

trofielist <- read_delim("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Specieslist/protisttable.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Quick check whether the trophic levels of the previous species list correspond with trofielist
overeenkomst <- specieslist %>% filter(soortnaam %in% trofielist$ScientificName)

overeenkomst <- left_join(overeenkomst %>% select(soortnaam, trofie), 
                          trofielist %>% select(ScientificName, Trophy), 
                          by = c("soortnaam" = "ScientificName")) %>% 
  mutate_at(c('trofie', 'Trophy'), as.factor) %>% 
  mutate(Trophy = ifelse(Trophy == 'mixoplankton', 'Mixotroof', 
                         ifelse(Trophy=='phytoplankton', 'Autotroof', 
                                ifelse(Trophy=='protozooplankton', 'Heterotroof', Trophy)))) %>% 
  mutate(identical = trofie == Trophy)

(t <- table(overeenkomst$identical)) #About 90% is identical 
overeenkomst %>% filter(identical == FALSE) #of those that are not identical, 95% has been assigned Autotroof instead of Mixotroof


trofielist %>% distinct(Trophy, Family) %>% filter(!is.na(Family)) %>% select(Family)
trofielist %>% distinct(Family) %>% filter(!is.na(Family))
#Whithin a Familiy, differences in trophic level might occur


trofielist %>% distinct(Trophy, Genus) %>% filter(!is.na(Genus)) %>% select(Genus)
trofielist %>% distinct(Genus) %>% filter(!is.na(Genus))
#Within a Genus, differences in trophic level can occur

#We moeten kijken naar genera en families die in trofielist als alleen genus of familie beschreven worden
overeenkomst_genus <- overeenkomst %>%
  mutate(genus = ifelse(nchar(gsub("[^ ]", "", soortnaam)) + 1 == 2, 
                        sapply(strsplit(as.character(soortnaam), " "), `[`, 1), 
                        NA))
overeenkomst_genus <- overeenkomst_genus %>% filter(genus %in% trofielist$ScientificName)
str(overeenkomst_genus)
#apparently, scientificname in trofielist does not contain genera names corresponding with genera included in specieslist
#oh well, this was mostly to check the old specieslist, for the new one it should be easier

species <- species %>% mutate(soortnaam= ScientificName...2)

species_sciName <- species %>% filter(soortnaam %in% trofielist$ScientificName) %>% 
  left_join(trofielist %>% select(ScientificName, Trophy), 
            by=c('soortnaam' = 'ScientificName'))
species_genus <- species %>% filter(soortnaam %in% trofielist$Genus) %>% 
  left_join(trofielist %>% select(Genus, Trophy), 
            by=c('soortnaam' = 'Genus'))
species_family <- species %>% filter(soortnaam %in% trofielist$Family) %>% 
  left_join(trofielist %>% select(Family, Trophy), 
            by=c('soortnaam' = 'Family'))
species_order <- species %>% filter(soortnaam %in% trofielist$Order) %>% 
  left_join(trofielist %>% select(Order, Trophy),
            by=c('soortnaam'='Order'))
species_class <- species %>% filter(soortnaam %in% trofielist$Class) %>% 
  left_join(trofielist %>% select(ScientificName, Class),
            by=c('soortnaam'='Class'))

df_sciName <- left_join(species_append, species %>% filter(soortnaam %in% trofielist$ScientificName) %>% 
            left_join(trofielist %>% select(ScientificName, Trophy), 
                      by=c('soortnaam' = 'ScientificName')) %>% select(soortnaam, Trophy))
df_genus <- left_join(species_append, species %>% filter(soortnaam %in% trofielist$Genus) %>% 
            left_join(trofielist %>% select(Genus, Trophy), 
                      by=c('soortnaam' = 'Genus')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy)
df_fam <- left_join(species_append, species %>% filter(soortnaam %in% trofielist$Family) %>% 
            left_join(trofielist %>% select(Family, Trophy), 
                      by=c('soortnaam' = 'Family')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy)
left_join(species_append, species %>% filter(soortnaam %in% trofielist$Order) %>% 
            left_join(trofielist %>% select(Order, Trophy),
                      by=c('soortnaam'='Order')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy) #This one is longer, within a single order all trophic levels are found

species_append2 <- data.frame(soortnaam = species_append$soortnaam, 
           soortcode = rep(NA, nrow(species_append)), 
           TWN = rep(NA, nrow(species_append)), 
           TWN2 = rep(NA, nrow(species_append)), 
           soortnaam2 = rep(NA, nrow(species_append)), 
           trofie = coalesce(df_sciName$Trophy, 
                             df_genus$Trophy, 
                             df_fam$Trophy), 
           groep = species_append$groep, 
           groepcode = rep(NA, nrow(species_append))) 

# New mixoplankton list

mixotable <- read_excel("https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Specieslist/The%20Mixoplankton%20Database%20-%20MDB%20(20230418).xlsx", 
                        sheet = "MDB - 3Dec2022 ", skip = 3)

species_append3 <- species_append2 %>% mutate(trofie = ifelse(soortnaam %in% mixotable$`Species Name`, 'mixoplankton', trofie))

species_append4 <- species_append3 %>% mutate(trofie = case_when(
  soortnaam == 'Myrionecta rubra' ~ 'protozooplankton',
  soortnaam == 'Gymnodiniales' ~ 'mixoplankton', #Waller&Koreny 2017
  soortnaam == 'Eucampia zoodiacus' ~ 'fytoplankton', #Guiry 2011
  soortnaam == 'Odontella longicruris' ~ 'fytoplankton', #Giury 2011
  soortnaam == 'Prymnesiales' ~ "fytoplankton", #Edvardsen 2011
  soortnaam == 'Delphineis' ~ 'fytoplankton',
  soortnaam == 'Bellerochea horologicalis' ~ 'fytoplankton',
  groep == 'Diatomeeen' ~ 'fytoplankton',
  TRUE ~ trofie
))






new_specieslist <- rbind(specieslist, species_append4) %>%  mutate(trofie = ifelse(trofie == 'Mixotroof', 'mixoplankton', 
                                                           ifelse(trofie=='Autotroof', 'fytoplankton',
                                                                  ifelse(trofie == 'phytoplankton', 'fytoplankton',
                                                                      ifelse(trofie=='Heterotroof', 'protozooplankton', trofie)))))
groepcodes <- new_specieslist %>% distinct(trofie, groep, groepcode) %>% filter(!is.na(groepcode))

new_specieslist <- left_join(new_specieslist, groepcodes, by= c('trofie', 'groep')) %>% 
  rename(groepcode = groepcode.x) %>% select(-groepcode.y)

write.csv(new_specieslist,
          file.path(savepath,'Specieslist_zelfuitgebreid.csv'))


