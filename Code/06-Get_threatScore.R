#### ==== Get threat score value based on IUCN Red List category ==== ####
# Note: done on local computer, using R version 4.1.1
# Calculate threat score value based on IUCN Red List category,
# getting predicted categories for DD species and calculating
# taxa averages for any species still without a threat score

library(tidyverse)
## tidyverse v1.3.1

## === 1. add threat score based on red list category ====
# LC = 2, NT = 4, VU = 8, EN = 16, CR = 32 
aoh2020 <- read_csv("../Data/Processed/ForestSpeciesList_AOH2020.csv") %>% 
  mutate(Class = factor(Class, levels=c("AMPHIBIA", "AVES", "MAMMALIA", "REPTILIA")),
         Category = factor(Category, levels=c("DD", "CR", "EN", "VU", "NT", "LC")),
         Habitat = factor(Habitat),
         PopTrend = factor(PopTrend),
         ForestDependency = factor(ForestDependency),
         total_km2 = rowSums(across(Unprotected_km2:catPIA_km2)),
         PA_km2 = rowSums(across(catIa_km2:catNA_km2)),
         fracPA = PA_km2/total_km2,
         fracIL = catIL_km2/total_km2,
         fracPIA = catPIA_km2/total_km2,
         fracUnPro = Unprotected_km2/total_km2) %>% 
  mutate(ThreatScore = case_when(Category == "LC" ~ 2,
                                 Category == "NT" ~ 4,
                                 Category == "VU" ~ 8,
                                 Category == "EN" ~ 16,
                                 Category == "CR" ~ 32))

## === 2. add threat score for DD species based on literature ====
# scoring as Not Threatened (LC & NT) = 3
# Threatened (VU, EN, CR) = 16
# Imperilled (EN, CR) = 24
# possibly Extinct = 32

ddsp <- aoh2020 %>% 
  filter(Category == "DD",
         !is.na(fracPA)) %>% 
  select(Species, Class) %>% 
  write_csv("../Data/Raw/DD-species/ForestSpeciesList_AOH2020_DDspecies.csv")
# 1108 sp total: AMPHIBIA: 250; AVES: 25; MAMMALIA: 350; REPTILIA: 483

#### Gonzalez-Pliego et al 2019:
# Predicted - A = (LC & NT); B = VU; C = EN & CR; D = possibly extinct
ddAMP <- read_csv("../Data/Raw/DD-species/DD_Amphibia.csv") %>% 
  rename(Assessed = `AS: assessed by the IUCN; DD: data-deficient species`,
         Predicted = `predicted_threat_score_for_DD_species (A = not threatened (LC & NT); B = VU; C = imperilled (EN & CR); D = may be extinct already)`) %>% 
  select(species, Assessed, Predicted) %>% 
  filter(Assessed == "DD") %>% 
  mutate(Predicted = as.factor(Predicted)) %>% 
  # add threat score column. A = 3, B = 8, C = 24, D = 32
  mutate(ThreatScore = ifelse(Predicted == "A", 3, 
                              ifelse(Predicted == "B", 8, 
                                     ifelse(Predicted == "C", 24, 32)))) %>% 
  mutate(species = str_replace(species, "_", " ")) %>% 
  select(species, ThreatScore)
# combine to ddsp
ddsp <- ddsp %>% 
  left_join(ddAMP, by=c("Species"="species"))
summary(filter(ddsp, Class == "AMPHIBIA"))
filter(ddsp, Class == "AMPHIBIA", is.na(ThreatScore))
# 56 amphibia sp still missing data

#### Bland et al 2014:
# Predicted - Threatened (VU, EN, CR); Non threatened (LC, NT)
ddMAM <- read_csv("../Data/Raw/DD-species/DD_Mammals.csv") %>%  
  rename(Species = `Binomial name`) %>% 
  mutate(Prediction = as.factor(Prediction)) %>% 
  # add ThreatScore column. Threatened = 16; Non threatened = 3
  mutate(ThreatScore = ifelse(Prediction == "Threatened", 16, 3)) %>% 
  select(Species, ThreatScore)
# combine to ddsp
ddsp2 <- ddsp %>% 
  left_join(ddMAM, by="Species") %>% 
  mutate(ThreatScore = coalesce(ThreatScore.x, ThreatScore.y)) %>% 
  select(-ThreatScore.x, -ThreatScore.y)
summary(ddsp2 %>% filter(Class == "MAMMALIA")) 
filter(ddsp2, Class == "MAMMALIA", is.na(ThreatScore))
# 182 mammalia sp still missing data

#### Jetz and Freckleton 2015:
# Predicted - Threatened (VU, EN, CR); Non threatened (LC, NT)
ddMAM2 <- read_csv("../Data/Raw/DD-species/DD_all.csv") %>% 
  rename("Species" = `Latin - IUCN`,
         "Predicted" = `Predicted Threat Status`) %>% 
  select(Species, Predicted) %>% 
  mutate(Species = str_replace(Species, "_", " "),
         Predicted = as.factor(Predicted)) %>% 
  # add ThreatScore column. Threatened = 16; Non threatened = 3
  mutate(ThreatScore = ifelse(Predicted == "Threatened", 16, 3)) %>% 
  select(Species, ThreatScore)
# combine to ddsp
ddsp3 <- ddsp2 %>% 
  left_join(ddMAM2, by="Species") 
ddsp3 %>% filter(ThreatScore.x != ThreatScore.y) 
# 14 cases where Jetz & Freckleton disagree with Bland et al
# Species                Class    ThreatScore.x (Bland) ThreatScore.y (Jetz)
# 1 Dacnomys millardi      MAMMALIA             3            16
# 2 Dasyprocta kalinowskii MAMMALIA             3            16
# 3 Echimys saturnus       MAMMALIA             3            16
# 4 Sciurus flammifer      MAMMALIA             3            16
# 5 Sciurus pyrrhinus      MAMMALIA             3            16
# 6 Scotophilus celebensis MAMMALIA            16             3
# 7 Sylvisorex oriundus    MAMMALIA             3            16
# 8 Petaurillus hosei      MAMMALIA            16             3
# 9 Graphiurus surdus      MAMMALIA            16             3
# 10 Exilisciurus exilis    MAMMALIA            16             3
# 11 Lasiurus castaneus     MAMMALIA             3            16
# 12 Maxomys baeodon        MAMMALIA            16             3
# 13 Peromyscus furvus      MAMMALIA             3            16
# 14 Proechimys chrysaeolus MAMMALIA             3            16

# if they disagree, use Jetz & Freckleton's instead since it's more recent
ddsp4 <- ddsp3 %>% 
  mutate(ThreatScore = ifelse(is.na(ThreatScore.y), ThreatScore.x, ThreatScore.y)) %>% 
  select(-ThreatScore.x, -ThreatScore.y)
filter(ddsp4, Class == "MAMMALIA", is.na(ThreatScore))
# 160 mammalia sp still missing data

#### Butchart and Bird 2010: 
# likely true status: not recognised (0), not threatened (3), near threatened (4), threatened (16)
ddBIRD <- read_csv("../Data/Raw/DD-species/DD_birds_final.csv") %>% 
  mutate(Status = as.factor(Status)) %>% 
  # Not threatened: 46 sp, Near threatened: 13 sp, Threatened: 10 sp, Not recognised: 3 sp
  mutate(ThreatScore = case_when(Status == "Not recognised" ~ 0,
                                 Status == "Not threatened" ~ 3,
                                 Status == "Near threatened" ~ 4,
                                 Status == "Threatened" ~ 16)) %>% 
  select(-Status)
# combine to ddsp
ddsp5 <- ddsp4 %>% 
  left_join(ddBIRD, by="Species") %>% 
  mutate(ThreatScore = coalesce(ThreatScore.x, ThreatScore.y)) %>% 
  select(-ThreatScore.x, -ThreatScore.y)
# 6 bird sp still missing predicted threat status

# total 626 NAs remaining

## === 3. calculate taxa-specific predicted threat score ====
# combining with DD status from literature
aoh2020v2 <- aoh2020 %>% 
  left_join(ddsp5, by=c("Species","Class")) %>% 
  mutate(ThreatScore = coalesce(ThreatScore.x, ThreatScore.y)) %>% 
  select(-ThreatScore.x, -ThreatScore.y) %>% 
  filter(!is.na(fracPA))

# calculate taxa-specific global average score
aoh2020v2 %>% 
  select(Class, ThreatScore) %>% 
  filter(!is.na(ThreatScore)) %>% 
  group_by(Class) %>% 
  summarise(meanScore = mean(ThreatScore),
            medianScore = median(ThreatScore),
            n = n())
# Class    meanScore medianScore     n
# 1 AMPHIBIA     11.0            8  1400
# 2 AVES          3.73           2  6392
# 3 MAMMALIA      8.17           3  1565
# 4 REPTILIA      6.12           2  1618

# let's go with mean... 
aoh2020v3 <- aoh2020v2 %>% 
  mutate(ThreatScore = case_when(Class == "AMPHIBIA" & is.na(ThreatScore) ~ 11,
                                 Class == "AVES" & is.na(ThreatScore) ~ 4,
                                 Class == "MAMMALIA" & is.na(ThreatScore) ~ 8,
                                 Class == "REPTILIA" & is.na(ThreatScore) ~ 6,
                                 TRUE ~ ThreatScore)) %>% 
  write_csv("../Data/Raw/ForestSpeciesList_AOH2020_ThreatScore.csv")
