#Make a new species list using the WoRMS Taxon match tool, the protist table and mixoplankton table 

require(readr)
require(purrr)
require(tidyverse)
require(readxl)
require(xtable)

# Load the  species list in the previous systeemrapportage 
specieslist_old <- read_tsv(
  file.path('https://watersysteemdata.deltares.nl/thredds/fileServer/watersysteemdata/Westerschelde/Scheldemonitor/2021', "Specieslist_zelfuitgebreid_jh.csv"),
  col_names = F)

names(specieslist_old) <- c(
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
missingspecies_matched <- read_csv("P:/11202493--systeemrap-grevelingen/1_data/Westerschelde/Specieslist/missingspecies2022_matched.csv")

# Group the species based on taxonomy
specieslist_new <- missingspecies_matched %>% mutate(groep = rep(NA, nrow(missingspecies_matched))) %>% 
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


species_append <- specieslist_new %>% filter(!is.na(...1)) %>% 
  select(soortnaam = ScientificName...2, groep = groep)

trofielist <- read_delim("P:/11202493--systeemrap-grevelingen/1_data/Westerschelde/Specieslist/protisttable.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% mutate(Trophy = ifelse(Trophy == 'phytoplankton','fytoplankton', Trophy))

# Quick check whether the trophic levels of the previous species list correspond with trofielist
overeenkomst <- specieslist_old %>% filter(soortnaam %in% trofielist$ScientificName)

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

#Regardless of the previous outcomes. the trofielist is newer and likely more accurate than our older information. 
#The values in the old list will be overwritten. Therefore we need to bind the old and the new species lists. 

#For the new specieslist, we first want to add the trophic information available on genus and family level first, before removing columns including taxonomic information. 

specieslist_new <- specieslist_new %>% mutate(soortnaam= ScientificName...2)

#species_sciName <- species %>% filter(soortnaam %in% trofielist$ScientificName) %>% 
#  left_join(trofielist %>% select(ScientificName, Trophy), 
#           by=c('soortnaam' = 'ScientificName'))
species_genus <- specieslist_new %>% filter(soortnaam %in% trofielist$Genus) %>% 
  left_join(trofielist %>% select(Genus, Trophy), 
            by=c('soortnaam' = 'Genus'))
species_family <- specieslist_new %>% filter(soortnaam %in% trofielist$Family) %>% 
  left_join(trofielist %>% select(Family, Trophy), 
            by=c('soortnaam' = 'Family'))
species_order <- specieslist_new %>% filter(soortnaam %in% trofielist$Order) %>% 
  left_join(trofielist %>% select(Order, Trophy),
            by=c('soortnaam'='Order'))
species_class <- specieslist_new %>% filter(soortnaam %in% trofielist$Class) %>% 
  left_join(trofielist %>% select(ScientificName, Class),
            by=c('soortnaam'='Class'))

#df_sciName <- left_join(species_append, species %>% filter(soortnaam %in% trofielist$ScientificName) %>% 
#                          left_join(trofielist %>% select(ScientificName, Trophy), 
#                                    by=c('soortnaam' = 'ScientificName')) %>% select(soortnaam, Trophy))
df_genus <- left_join(species_append, specieslist_new %>% filter(soortnaam %in% trofielist$Genus) %>% 
                        left_join(trofielist %>% select(Genus, Trophy), 
                                  by=c('soortnaam' = 'Genus')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy)
df_fam <- left_join(species_append, specieslist_new %>% filter(soortnaam %in% trofielist$Family) %>% 
                      left_join(trofielist %>% select(Family, Trophy), 
                                by=c('soortnaam' = 'Family')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy)
left_join(species_append, specieslist_new %>% filter(soortnaam %in% trofielist$Order) %>% 
            left_join(trofielist %>% select(Order, Trophy),
                      by=c('soortnaam'='Order')) %>% select(soortnaam, Trophy)) %>% 
  distinct(soortnaam, groep, Trophy) #This one is longer, within a single order GYmnodiniales all trophic levels are found

species_append2 <- data.frame(soortnaam = species_append$soortnaam, 
                              soortcode = rep(NA, nrow(species_append)), 
                              TWN = rep(NA, nrow(species_append)), 
                              TWN2 = rep(NA, nrow(species_append)), 
                              soortnaam2 = rep(NA, nrow(species_append)), 
                              trofie = coalesce(df_genus$Trophy, 
                                                df_fam$Trophy), 
                              groep = species_append$groep, 
                              groepcode = rep(NA, nrow(species_append)))

#' Manual adjustment
species_append3 <- species_append2 %>% mutate(trofie = case_when(
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

specieslist_full <- rbind(specieslist_old, 
                           species_append3) %>% mutate(trofie = ifelse(trofie == 'Mixotroof','mixoplankton',  
                                                                      ifelse(trofie== 'Autotroof','fytoplankton',  
                                                                             ifelse(trofie=='Heterotroof','protozooplankton',  trofie))))

#now we can add the values of trofie based on the species level in trofielist

specieslist_full <- specieslist_full %>%
  left_join(trofielist %>% rename(soortnaam = ScientificName) %>% 
              rename(trofie = Trophy) %>% 
              select(soortnaam, trofie), by = "soortnaam", suffix = c(".df1", ".df2")) %>%
  mutate(trofie = coalesce(trofie.df2, trofie.df1)) %>% #priority given to the trofielist values
  select(-trofie.df1, -trofie.df2) 

#We also heave a mixoplankton database, which is even more recent
mixotable <- read_excel("P:/11202493--systeemrap-grevelingen/1_data/Westerschelde/Specieslist/The Mixoplankton Database - MDB (20230418).xlsx", 
                        sheet = "MDB - 3Dec2022 ", skip = 3)

specieslist_full_final <- specieslist_full %>% mutate(trofie = ifelse(soortnaam %in% mixotable$`Species Name`, 'mixoplankton', trofie))

specieslist_full_final <- specieslist_full_final %>% mutate(groepcode= NA)
groepcodes <- specieslist_old %>% distinct(trofie, groep, groepcode) %>% filter(!is.na(groepcode)) %>% mutate(trofie = ifelse(trofie == 'Mixotroof','mixoplankton',  
                                                                                                                              ifelse(trofie== 'Autotroof','fytoplankton',  
                                                                                                                                     ifelse(trofie=='Heterotroof','protozooplankton',  trofie))))
specieslist_full_final <- left_join(specieslist_full_final, groepcodes, by= c('trofie', 'groep')) %>% 
  rename(groepcode = groepcode.y) %>% select(-groepcode.x) %>% select(names(specieslist_old))

write.csv(specieslist_full_final,
          file.path(savepath,'Specieslist_zelfuitgebreid.csv'))

#print(xtable(specieslist_full_final %>% select(soortnaam, soortcode, TWN, TWN2, trofie, groep) %>% 
#               rename(Naam = soortnaam, Code = soortcode, BTX = TWN, AphiaID = TWN2, Trofie = trofie, Groep = groep)), 
#      type = 'latex', file = 'Figuren/Tabellen/fytoplankton1.tex')
