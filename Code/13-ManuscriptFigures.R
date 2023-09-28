#### ============ Make figures for manuscript ============== ####
# Also includes code for analysis of number of species etc. 
# Note: done in local computer, R version 4.1.1

library(tidyverse)
library(patchwork) 
library(prioritizr) # for log-linear interpolation of sufficient AOH
# for Fig 3 map
library(sf) 
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer) 
sf::sf_use_s2(FALSE)
# tidyverse v1.3.1, patchwork v1.1.2, sf v1.0.4, rnaturalearth v0.1.0, rnaturalearthdata v0.1.0, RColorBrewer v1.1.2

## dataset for the 11601 specices for Figs 1 and 2
aoh2020 <- read_csv("./Output/ForestSpeciesList_AOH2020.csv",
                    col_types = cols(Class="f", Category="f", Habitat="f", PopTrend="f", ForestDependency="f")) %>% 
  mutate(Class = factor(Class, levels=c("AMPHIBIA", "AVES", "MAMMALIA", "REPTILIA"),
                        labels=c("Amphibians", "Birds", "Mammals", "Reptiles")),
         Category = factor(Category, levels=c("DD", "CR", "EN", "VU", "NT", "LC")),
         NewCategory = case_when(Category=="DD" ~ "DD",
                                 Category=="CR" ~ "Threatened",
                                 Category=="EN" ~ "Threatened",
                                 Category=="VU" ~ "Threatened",
                                 Category=="NT" ~ "NT/LC",
                                 TRUE ~ "NT/LC"),
         NewCategory = factor(NewCategory, levels=c("Threatened","NT/LC","DD"))) # 11601 obs
summary(aoh2020)    
# Species               Class       Category                Habitat    
# Length:11601       AMPHIBIA:1456   DD:1004   Forest important  :5606  
# Class :character   AVES    :6398   CR: 484   Forest unimportant:2402  
# Mode  :character   MAMMALIA:1725   EN:1026   Forest generalist :3593  
#                    REPTILIA:2022   VU:1003                            
#                                    NT: 977                            
#                                    LC:7107   

## dataset for Within Tropical Forest for the 1135 species AOH for SFigs 1 and 2
aoh2020_within <- read_csv("./Output/ForestSpeciesList_AOH2020_WithinTropFor.csv",
                           col_types = cols(Class="f", Category="f", Habitat="f", PopTrend="f", ForestDependency="f")) %>% 
  mutate(Class = factor(Class, levels=c("AMPHIBIA", "AVES", "MAMMALIA", "REPTILIA"),
                        labels=c("Amphibians", "Birds", "Mammals", "Reptiles")),
         Category = factor(Category, levels=c("DD", "CR", "EN", "VU", "NT", "LC")),
         NewCategory = case_when(Category=="DD" ~ "DD",
                                 Category=="CR" ~ "Threatened",
                                 Category=="EN" ~ "Threatened",
                                 Category=="VU" ~ "Threatened",
                                 Category=="NT" ~ "NT/LC",
                                 TRUE ~ "NT/LC"),
         NewCategory = factor(NewCategory, levels=c("Threatened","NT/LC","DD"))) 
summary(aoh2020_within)
# Species                 Class     Category               Habitat   
# Length:1135        Amphibians:517   DD:317   Forest important  :773  
# Class :character   Birds     :175   CR:203   Forest unimportant:308  
# Mode  :character   Mammals   :179   EN:216   Forest generalist : 54  
#                    Reptiles  :264   VU: 98                           
#                                     NT: 61                           
#                                     LC:240                           

## dataset for observed median value in IPLs for 3 indices for Fig 3 and SFig 4
medDF <- read_csv("./Output/ObservedMedianValue.csv") %>% 
  pivot_wider(names_from=Scenario,
              values_from=Value)
# dataset for observed median value in IPLs for sp within tropical forest for SFig 3
medDF_within <- read_csv("./Output/ObservedCountryRanges_WithinTropFor.csv",
                  col_types = cols(Scenario="f",Country="f",Taxa="f",Area="f",Min="d",TukeyLH="d",Median="d",TukeyUH="d",Max="d")) %>% 
  select(-c(Min, TukeyLH, TukeyUH, Max)) %>% 
  pivot_wider(names_from=Scenario,
              values_from=Median)

## regional information for Fig 4, SFigs 5-9
regionsDF <- data.frame(Country = c("NCL","AUS",
                                    "PAK","CHN","MYS","TWN","LKA","PHL","IDN","KHM","NPL","BGD","VNM","LAO","IND","MMR","THA",
                                    "CMR","NGA","RWA","COD","KEN","UGA","CIV","CAF","TGO","BEN","TZA","GAB","ETH","COG","BDI",
                                    "CRI","DMA","GUY","BRA","SLV","PAN","SUR","BLZ","GUF","HND","PER","BOL","GTM","NIC",
                                    "MEX","ARG","PRY","USA","VEN","COL","ECU"),
                        Region = c(rep("Oceania", 2),
                                   rep("Asia", 15),
                                   rep("Africa", 15),
                                   rep("Americas", 21)))

## dataset for the mean difference between IPL and outside for 3 indices for Fig 4 and SFig 5-9
diffDF <- read_csv("./Output/MeanDifferences.csv",
                   col_types=cols(Scenario='f', Country='f', Taxa='f', Area='f', Value='d', pValue='d')) %>% 
  left_join(regionsDF, by="Country") %>% 
  mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                 pValue < 0.05 & Value < 0 ~ "SigNeg",
                                 TRUE ~ "NotSig"),
         Significant = factor(Significant, levels=c("SigPos", "SigNeg","NotSig")),
         Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas")),
         Scenario = factor(Scenario, levels=c("SR","TS","IR"), labels=c("Species Richness","Extinction vulnerability", "Range-size rarity")))

## an additional plot to label continents for Fig 4 and SFig 5-9
p2 <- tibble(ymin = c(0,2,18,33), ymax = c(2,18,33,54), fill = c("Oceania", "Asia", "Africa", "Americas\n*")) %>% 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
  geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
  scale_y_continuous(breaks = seq(1, 54), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
  guides(fill = "none") +
  theme_void()

#### range overlap analysis ####                                     
## No of species that intersect IPL (IL and/or PIA)
nIPL <- filter(aoh2020, IL_nPixels>0 | PIA_nPixels>0)
# Species               Class      Category                Habitat    
# Length:8874        AMPHIBIA: 700   DD: 628   Forest important  :3977  
# Class :character   AVES    :5567   CR: 159   Forest unimportant:1756  
# Mode  :character   MAMMALIA:1321   EN: 489   Forest generalist :3141  
#                    REPTILIA:1286   VU: 648                            
#                                    NT: 729                            
#                                    LC:6221  
## No of species that intersect PA and/or IPL
nPAIPL <- filter(aoh2020, IL_nPixels>0 | PIA_nPixels>0 | PA_nPixels>0)
# Species               Class      Category                Habitat    
# Length:10965       AMPHIBIA:1218   DD: 847   Forest important  :5194  
# Class :character   AVES    :6235   CR: 373   Forest unimportant:2246  
# Mode  :character   MAMMALIA:1652   EN: 927   Forest generalist :3525  
#                    REPTILIA:1860   VU: 942                            
#                                    NT: 925                            
#                                    LC:6951 
# For these 10965 sp, how many have >50% of AOH outside PA and/or IPL
sumPAIPL <- nPAIPL %>% 
  filter(fracUnPro >= 0.5)
# Species               Class      Category                Habitat    
# Length:6205        AMPHIBIA: 516   DD: 377   Forest important  :2696  
# Class :character   AVES    :3828   CR: 139   Forest unimportant:1217  
# Mode  :character   MAMMALIA: 895   EN: 480   Forest generalist :2292  
#                    REPTILIA: 966   VU: 572                            
#                                    NT: 490                            
#                                    LC:4147            
# For all 11601 sp, how many have >50% of AOH outside PA and/or IPL
nOutside <- filter(aoh2020, fracUnPro >= 0.5)

#        Species                 Class              Category                Habitat         
# Length:6841        Amphibians: 754/1456 (51.8%)   DD: 534   Forest important  :3108    
# Class :character   Birds     :3991/6398 (62.4%)   CR: 250   Forest unimportant:1373    
# Mode  :character   Mammals   : 968/1725 (56.1%)   EN: 579   Forest generalist :2360    
#                    Reptiles  :1128/2022 (55.8%)   VU: 633                              
#                                                   NT: 542                              
#                                                   LC:4303                                              



## Considering variable area sufficient for species
protectionTarget <- aoh2020 %>% 
  dplyr::select(Species, Class, Category, PA_nPixels:total_nPixels) %>% 
  mutate(fracTarget = prioritizr::loglinear_interpolation(x=total_nPixels,
                                                          coordinate_one_x=1000,
                                                          coordinate_one_y=1,
                                                          coordinate_two_x=250000,
                                                          coordinate_two_y=0.1),
         target_nPixels = fracTarget * total_nPixels,
         target_nPixels = case_when(target_nPixels>10000000 ~ 1000000,
                                TRUE ~ target_nPixels)) 
enoughPA <- protectionTarget %>% 
  filter(PA_nPixels+PIA_nPixels >= target_nPixels) # 3633 sp
enoughIPL <- protectionTarget %>% 
  filter(IL_nPixels+PIA_nPixels >= target_nPixels) #4935 sp
inBoth <- inner_join(enoughPA, enoughIPL) # 2823 sp
enoughPAIPL <- protectionTarget %>% 
  filter(PA_nPixels+IL_nPixels+PIA_nPixels >= target_nPixels) # 6361 sp
needIL <- anti_join(enoughPAIPL, enoughPA) # 2728 sp

## ---- Fig 1 & SFig 1: range histograms ----
## violin plot summaries of fraction of overlap for
## A. all species
## B. threatened species only

class20 <- aoh2020 %>% # or aoh2020_within for SFig 1
  mutate(fracIPL = fracIL + fracPIA,
         fracPA = fracPA + fracPIA) %>% 
  dplyr::select(Class, Category, fracIPL, fracPA, fracUnPro) %>% 
  pivot_longer(cols=fracIPL:fracUnPro, names_to="ProType", names_prefix="frac", values_to="FracCovered") %>% 
  mutate(ProType = factor(ProType, levels=c("UnPro", "PA","IPL"),
                          labels=c("None", "PA", "IPL")),
         Class = factor(Class, labels=c("Amphibians", "Birds", "Mammals", "Reptiles")))

class20_thr <- aoh2020 %>% 
  filter(NewCategory == "Threatened") %>% 
  mutate(fracIPL = fracIL + fracPIA,
         fracPA = fracPA + fracPIA) %>% 
  dplyr::select(Class, Category, fracIPL, fracPA, fracUnPro) %>% 
  pivot_longer(cols=fracIPL:fracUnPro, names_to="ProType", names_prefix="frac", values_to="FracCovered") %>% 
  mutate(ProType = factor(ProType, levels=c("UnPro", "PA","IPL"),
                          labels=c("None", "PA", "IPL")),
         Class = factor(Class, labels=c("Amphibians", "Birds", "Mammals", "Reptiles")))

## To see the additionality of ILs only
# class20 <- aoh2020 %>% 
#   dplyr::select(Class, Category, fracIL, fracPA, fracPIA, fracUnPro) %>% 
#   pivot_longer(cols=fracIL:fracUnPro, names_to="ProType", names_prefix="frac", values_to="FracCovered") %>% 
#   mutate(ProType = factor(ProType, levels=c("UnPro","PA","PIA","IL"),
#                           labels=c("None","PA only","PIA only","IL only")),
#          Class = factor(Class, labels=c("Amphibians", "Birds", "Mammals", "Reptiles"))) %>% 
#   group_by(ProType, Class) %>% 
#   summarise(meanFrac = mean(FracCovered),
#             medFrac = median(FracCovered))

# facet wrap by class
p1 <- ggplot(class20, aes(x=ProType, y=FracCovered)) +
  facet_grid(rows=vars(Class)) +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=8, size=2) +
  stat_summary(fun=median, geom="point", shape=20, size=2) +
  ylab('Fraction of Habitat Overlap') +
  xlab('All species') +
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10),
        strip.background = element_blank(),
        strip.text = element_blank())

p2 <- ggplot(class20_thr, aes(x=ProType, y=FracCovered)) +
  facet_grid(rows=vars(Class)) +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=8, size=2) +
  stat_summary(fun=median, geom="point", shape=20, size=2) +
  ylab('Fraction of Habitat Overlap') +
  xlab('Threatened species only') +
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10))
        
fig1 <- p1 + p2 + plot_annotation(tag_levels="a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag=element_text(size=10, face="bold"))
ggsave("../Output/Manuscript/Figure1_2023-09-27.png",
       fig1, width=8, height=4.4, dpi=300)

## ---- Fig 2 & SFig 2: range overlap bars ----
# summary figure of no. of species in
# A. different areas (none, PA or IPL) for each category
# B. different red list category (DD, threatened, NT, LC) for each % of overlap

## get the dataframe for A
# for species whose range fall within IL
sumIL <- aoh2020 %>% # or aoh2020_within for SFig 2
  filter(fracIL==1) %>%
  count(Class, NewCategory) %>% 
  mutate(Area = "IPL only")
# for species whose range fall within PA
sumPA <- aoh2020 %>% 
  filter(fracPA==1) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "PA only")
# for species whose range fall within PIA
sumPIA <- aoh2020 %>% 
  filter(fracPIA==1) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "PIA only")
# for species whose range fall neither within IPL nor PA
sumNONE <- aoh2020 %>% 
  filter(fracUnPro == 1) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "None")
d1 <- bind_rows(sumNONE, sumPA, sumPIA, sumIL) %>% 
  mutate(Area = factor(Area, levels=c("None","PA only","PIA only","IPL only"))) 

thrPalette <- c("#d42020", "#fdbea5","#bababa")
# red, light pink, light grey

## get the dataframe for B
d2 <- aoh2020 %>% 
  dplyr::select(Species, Class, NewCategory, fracIL, fracPIA) %>%
  mutate(fracIPL = fracIL + fracPIA,
         percentHabitat_IPL=case_when(fracIPL==0 ~ 0,
                                      fracIPL<=.2 & fracIPL!=0 ~ 20,
                                      fracIPL>.2 & fracIPL<=.4 ~ 40,
                                      fracIPL>.4 & fracIPL<=.6 ~ 60,
                                      fracIPL>.6 & fracIPL<=.8 ~ 80,
                                      fracIPL>.8 & fracIPL<1 ~ 99,
                                      TRUE ~ 100)) %>% 
  dplyr::select(Class, NewCategory, percentHabitat_IPL) %>% 
  add_count(Class, NewCategory, percentHabitat_IPL, name="nSpecies_IPL") %>% 
  distinct() %>% 
  arrange(Class, NewCategory, percentHabitat_IPL) %>% 
  mutate(percentHabitat_IPL=factor(percentHabitat_IPL,
                                   levels=c(0, 20, 40, 60, 80, 99, 100),
                                   labels=c("0", "<20", "20-40", "40-60", "60-80", "80-99", "100"))) %>% 
  filter(percentHabitat_IPL != 0) 

thrPalette2 <- c("#EFF3FF","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C")
# Taken from RColorBrewer::brewer.pal(6, 'Blues')

p1 <- ggplot(d1) +
  facet_grid(rows=vars(Class), scales="free") +
  geom_col(aes(x=Area, y=n, colour = NewCategory, fill=NewCategory), width=0.4)+ 
  ylab('No. of species') +
  xlab('') +
  labs(fill="IUCN Red List \ncategories") +
  guides(colour="none",
         fill=guide_legend(nrow=2)) +
  scale_fill_manual(values=thrPalette) + 
  scale_colour_manual(values=thrPalette) + 
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.position='bottom',
        strip.background = element_blank(),
        strip.text = element_blank())

p2 <- ggplot(d2) +
  facet_grid(rows=vars(Class), scales="free") +
  geom_col(aes(x=NewCategory, y=nSpecies_IPL, colour = percentHabitat_IPL, fill=percentHabitat_IPL), 
           position=position_stack(reverse=TRUE), width=0.4) +
  ylab('No. of species') +
  xlab('') +
  labs(fill="Habitat overlap \nwith IPLs (%)") +
  guides(fill=guide_legend(reverse=TRUE), colour="none") +
  scale_fill_manual(values=thrPalette2) + 
  scale_colour_manual(values=thrPalette2) + 
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.position='bottom') 
  
# need to make plots a bit wider so legend doesn't get cut off. 
fig2 <- p1 + p2 + plot_annotation(tag_levels="a", tag_prefix="(", tag_suffix=")") &
  theme(plot.tag=element_text(size=10, face="bold"))
ggsave("../Output/Manuscript/Figure2_2023-09-27.png",
       fig2, width=7.2, height=5.8, dpi=300)

## ---- Fig 3 & SFig 3: actual values in IPL----
## map of countries with values of 
## A species richness, B threat score, C inverse range

# get country polygons from natural earth
# NOTE: need to use scale=10 for small countries like DMA to be included
# but ne considers French Guiana (GUF), Guadeloupe (GDP) and Martinique (MTQ)
# under France and the region is also considered Europe T.T
# All three regions are considered one feature. Tried to split
# and re-name to match gadm but was taking too much time. 
# GDP and MTQ are v small Caribbean islands that won't show up
# on global map and have no values here anyway, so will just leave
# them and rename France to French Guiana in this dataset
world <- ne_countries(scale=10,returnclass='sf') %>% 
  select(c(admin, adm0_a3,geounit,gu_a3,name_long,iso_a3,adm0_a3_is,region_un, geometry)) %>% 
  st_make_valid() %>% 
  st_crop(xmin=-115,ymin=-34.12812,xmax=180,ymax=34.98406) %>% 
  mutate(iso_a3 = case_when(adm0_a3=="FRA"~"GUF",
                            TRUE~iso_a3)) %>% 
  left_join(medDF, by=c("iso_a3"="Country")) %>% # or medDF_within for SFig 3
  select(-c(Taxa,Area))

p1 <- ggplot(data=world) +
  geom_sf(aes(fill=SR)) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value="#bababa") +
  labs(fill="Species richness") +
  theme_void() + 
  theme(legend.position='bottom',
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))  + 
  guides(fill=guide_colourbar(title.vjust=1))

p2 <- ggplot(data=world) +
  geom_sf(aes(fill=TS)) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value="#bababa") +
  labs(fill="Extinction vulnerability") +
  theme_void() + 
  theme(legend.position='bottom',
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))  + 
  guides(fill=guide_colourbar(title.vjust=1))

# outliers = 1.5*IQR
outlier_values <- boxplot.stats(medDF$IR)$out
# DMA,NCL,PAK,USA (IR>1.3e-05) but that would change the visualisation
# TZA looks like they've got high range-size rarity then.
# maybe just remove DMA & NCL since they're small islands, not v visible anyway
# (cut-off ~ 5e-05)
# but only DMA is >2sd from mean (7.204476e-05)
p3 <- ggplot(data=filter(world, IR<5e-05|is.na(IR))) +
  theme_void() +
  geom_sf(aes(fill=IR)) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value="#bababa") +
  labs(fill="Range-size rarity") + 
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8, angle=45, hjust=1),
        legend.position='bottom',
        plot.margin=grid::unit(c(0,0,0,0), "mm"))  + 
  guides(fill=guide_colourbar(title.vjust=1))

fig3 <- p1 / p2 / p3 + plot_annotation(tag_levels="a", tag_prefix="(", tag_suffix=")") &
  theme(plot.tag=element_text(size=10, face="bold"))
ggsave("../Output/Manuscript/Figure3_Median_2023-09-27.png",
       fig3, width=6.83, height=5.75, dpi=300)

## ---- Fig 4 - mean difference plots ----
## difference in mean values between IPL and 10 km buffer
## for species richness, threat score and inverse range
## (difference between IPL and 50 km buffer or all outside in SOM)

DF <- diffDF %>% 
  filter(Area=="buff10",
         Taxa=="All") %>% 
  filter(Country!="USA" & Country!="DMA") # skews plotting for TS and IR when included
# mutate(Country = tidytext::reorder_within(Country, Value, within=Index))
# tidytext::reorder_within is pretty neat but doesnt work cos I am not
# trying to include region as a facet_grid, I just want the countries
# to be ordered and shown according to region

# instead try creating a separate column determining order 
# according to value within the regions for each index
new_order <- DF %>% 
  dplyr::group_by(Region, Scenario) %>% 
  do(tibble(al=levels(reorder(interaction(.$Scenario, .$Region, .$Country, drop=TRUE), .$Value)))) %>% 
  pull(al)

DF <- DF %>% 
  mutate(al = factor(interaction(Scenario, Region, Country), levels=new_order))

sigPalette <- c("#EE6677","#4477AA","#BBBBBB")
# red, blue, grey

p1 <- ggplot(DF, aes(x=Value, y=al, colour=Significant)) +
  facet_wrap(~Scenario, scales="free") +
  # tidytext::scale_y_reordered() +
  # geom_segment(aes(x=Min, y=Country, xend=Max, yend=Country), colour="#08519C", size=1, na.rm=TRUE) +
  geom_vline(xintercept=0, linetype=2) +
  geom_point(size=0.8, na.rm=TRUE) +
  theme_classic() +
  scale_colour_manual(values=sigPalette) +
  ylab("") +
  xlab("Difference between mean value in IPL and 10-km buffer zone") +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8)) +
  guides(colour="none") +
  scale_y_discrete(breaks=new_order, labels=gsub("^.*\\.", "", new_order))

fig4 <- p2 + p1 + plot_layout(widths = c(1, 10))

ggsave("../Output/Manuscript/Figure4_2023-09-27.png",
       fig4, width=7.04, height=5.75, dpi=300) 

#### SFig 4: 3 indices scatter plot ####
# median values in IPL
df <- medDF %>% 
  left_join(regionsDF, by="Country") %>% 
  pivot_wider(names_from=Scenario,
              values_from=Value)

sfig <- ggplot(df, aes(x=SR, y=TS, size=IR, colour=Region)) +
  # geom_point() +
  geom_text(aes(label=Country, colour=Region, size=IR), 
            nudge_x = 5, nudge_y=-0.1, check_overlap = TRUE) + # hjust=0, vjust=1.5,
  theme_classic() +
  xlab("Species richness") +
  ylab("Extinction vulnerability") +
  guides(size=guide_legend(title="Range-size rarity")) +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8))
ggsave("../Output/Manuscript/SFigure_3indices_Median_2023-04-03.png",
       sfig, width=7.04, height=5.75, dpi=300)
  
#### SFig 5: difference between IPL & 10km buffer, 50km buffer and all outside ####
  ## with facet_wrap, Scenario facet labels appear on top of Area facet labels instead of on right side
  ## with facet_grid, facet labels in right position but unable to free x-axes so extinction vulnerability
  ## and range-size rarity are not meaningful to read
  ## tried using ggh4x::facet_grid2 which does allow free x-axes and has facet labels in right positions
  ## though they all have variable name, e.g. 'Area: 10-km buffer' or 'Scenario: Species Richness' included  
  ## in the label instead of just variable value which is annoying but because x axis labels for same
  ## index (e.g. species richness) are not the same, makes it difficult to visually compare across the
  ## different areas. Decided to split the three indices to plot and combine using patchwork
  
  ## make df for the three indices
  srDF <- diffDF %>% 
    filter(Taxa=="All",
           Scenario=="Species Richness")  %>% 
    mutate(Area = factor(Area, levels=c("buff10", "buff50", "out"), labels=c("10-km buffer", "50-km buffer", "all outside IPL"))) %>% 
    filter(Country!="USA") # cos plot skews with it
  
  sr_new_order <- srDF %>% 
    dplyr::group_by(Region, Scenario) %>% 
    do(tibble(al=levels(reorder(interaction(.$Scenario, .$Region, .$Country, drop=TRUE), .$Value)))) %>% 
    pull(al)
  
  srDF <- srDF %>% 
    mutate(al = factor(interaction(Scenario, Region, Country), levels=sr_new_order))
  
  tsDF <- diffDF %>% 
    filter(Taxa=="All",
           Scenario=="Extinction vulnerability") %>% 
    mutate(Area = factor(Area, levels=c("buff10", "buff50", "out"), labels=c("10-km buffer", "50-km buffer", "all outside IPL"))) %>% 
    filter(Country!="USA") # cos plot skews with it
  
  ts_new_order <- tsDF %>% 
    dplyr::group_by(Region, Scenario) %>% 
    do(tibble(al=levels(reorder(interaction(.$Scenario, .$Region, .$Country, drop=TRUE), .$Value)))) %>% 
    pull(al)
  
  tsDF <- tsDF %>% 
    mutate(al = factor(interaction(Scenario, Region, Country), levels=ts_new_order))
  
  irDF <- diffDF %>% 
    filter(Taxa=="All",
           Scenario=="Range-size rarity") %>% 
    mutate(Area = factor(Area, levels=c("buff10", "buff50", "out"), labels=c("10-km buffer", "50-km buffer", "all outside IPL"))) %>% 
    filter(Country!="USA") # cos plot skews with it
  
  ir_new_order <- irDF %>% 
    dplyr::group_by(Region, Scenario) %>% 
    do(tibble(al=levels(reorder(interaction(.$Scenario, .$Region, .$Country, drop=TRUE), .$Value)))) %>% 
    pull(al)
  
  irDF <- irDF %>% 
    mutate(al = factor(interaction(Scenario, Region, Country), levels=ir_new_order))
  
  ## plot the three figures
  sigPalette <- c("#EE6677","#4477AA","#BBBBBB")
  # red, blue, grey
  
  sr_p1 <- ggplot(srDF, aes(x=Value, y=al, colour=Significant)) +
    facet_grid(rows=vars(Scenario), cols=vars(Area), scales="fixed") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=7),
          panel.spacing.x = unit(4, "mm")) +
    guides(colour="none") +
    scale_y_discrete(breaks=sr_new_order, labels=gsub("^.*\\.", "", sr_new_order))
  
  ts_p1 <- ggplot(tsDF, aes(x=Value, y=al, colour=Significant)) +
    facet_grid(rows=vars(Scenario), cols=vars(Area), scales="fixed") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=7),
          strip.text.x = element_blank(),
          panel.spacing.x = unit(4, "mm")) +
    guides(colour="none") +
    scale_y_discrete(breaks=ts_new_order, labels=gsub("^.*\\.", "", ts_new_order))
  
  ir_p1 <- ggplot(irDF, aes(x=Value, y=al, colour=Significant)) +
    facet_grid(rows=vars(Scenario), cols=vars(Area), scales="fixed") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("Difference between mean value in IPL and outside IPL") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=7),
          strip.text.x = element_blank(),
          panel.spacing.x = unit(4, "mm")) +
    guides(colour="none") +
    scale_y_discrete(breaks=ir_new_order, labels=gsub("^.*\\.", "", ir_new_order))
  
  sfig2 <- (p2 + sr_p1 + plot_layout(widths = c(1, 10))) / (p2 + ts_p1 + plot_layout(widths = c(1, 10))) / (p2 + ir_p1 + plot_layout(widths = c(1, 10))) 
  
  ggsave("../Output/Manuscript/SFigure_IPLvsOutside_2023-04-03.png",
         sfig2, width=9, height=15.4, dpi=300) 
  
#### SFig 6: species richness difference between IPL & 10km buffer for the 4 taxa ####
  DF <- diffDF %>% 
    filter(Taxa!="All",
           Scenario=="Species Richness") %>% 
    mutate(Country = factor(Country, levels=c("NCL","AUS",
                                              "PAK","MYS","CHN","TWN","LKA","PHL","IDN","KHM",
                                              "NPL","BGD","VNM","LAO","IND","MMR","THA",
                                              "CMR","NGA","RWA","COD","KEN","UGA","CIV","CAF",
                                              "TZA","TGO","BEN","ETH","GAB","COG","BDI",
                                              "CRI","GUF","GUY","BRA","DMA","PAN","SUR",
                                              "SLV","BLZ","HND","PER","BOL","GTM","MEX",
                                              "NIC","ARG","PRY","USA","VEN","COL","ECU")))
  
  p1 <- ggplot(DF, aes(x=Value, y=Country, colour=Significant)) +
    facet_grid(cols=vars(Taxa), scales="free") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("Difference between mean value of species richness in IPL and 10-km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") 
  # make an additional plot to label continents (this inc USA)
  p2b <- tibble(ymin = c(0,2,18,33), ymax = c(2,18,33,54), fill = c("Oceania", "Asia", "Africa", "Americas")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 54), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  sfig_sr <- p2b + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_SR_2023-04-03.png",
         sfig_sr, width=7.04, height=5.75, dpi=300) 
  
#### SFig 7: extinction vulnerability difference between IPL & 10km buffer for the 4 taxa ####
  DF <- diffDF %>% 
    filter(Taxa!="All",
           Scenario=="Extinction vulnerability") %>% 
    mutate(Country = factor(Country, levels=c("NCL","AUS",
                                              "VNM","THA","NPL","IDN","MMR","PHL",
                                              "KHM","PAK","CHN","IND","MYS","BGD",
                                              "LKA","TWN","LAO",
                                              "NGA","COD","TGO","CAF","COG","TZA",
                                              "ETH","GAB","BEN","CMR","BDI","CIV",
                                              "UGA","RWA","KEN",
                                              "USA","ECU","DMA","COL","SLV","BRA",
                                              "MEX","BLZ","PER","GUF","SUR","GUY",
                                              "GTM","BOL","VEN","ARG","HND","CRI",
                                              "NIC","PAN","PRY"))) %>% 
    filter(Country!="USA")
  
  p1 <- ggplot(DF, aes(x=Value, y=Country, colour=Significant)) +
    facet_grid(cols=vars(Taxa), scales="free") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("Difference between mean value of threat score in IPL and 10-km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") 
  
  sfig_ts <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_TS_2023-04-03.png",
         sfig_ts, width=7.04, height=5.75, dpi=300) 
  
#### SFig 8: range-size rarity difference between IPL & 10km buffer for the 4 taxa ####
  DF <- diffDF %>% 
    filter(Taxa!="All",
           Scenario=="Range-size rarity") %>% 
    mutate(Country = factor(Country, levels=c("AUS","NCL",
                                              "TWN","IND","VNM","THA","BGD","LKA",
                                              "MMR","CHN","IDN","LAO","KHM","NPL",
                                              "MYS","PHL","PAK",
                                              "TZA","BEN","TGO","COD","CIV","CAF",
                                              "GAB","COG","CMR","NGA","ETH","KEN",
                                              "UGA","RWA","BDI",
                                              "USA","DMA","ECU","COL","PER","NIC",
                                              "SLV","ARG","PAN","BOL","VEN","MEX",
                                              "BRA","GUF","SUR","BLZ","GUY","PRY",
                                              "HND","GTM","CRI"))) %>% 
    filter(Country!="DMA" & Country !="USA" & Country != "NCL")
  
  p1 <- ggplot(DF, aes(x=Value, y=Country, colour=Significant)) +
    facet_grid(cols=vars(Taxa), scales="free") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=sigPalette) +
    ylab("") +
    xlab("Difference between mean value of inverse range in IPL and 10-km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text.x = element_text(size=8, angle=45, vjust=1, hjust=1)) +
    guides(colour="none") 
  # make an additional plot to label continents (this exc DMA, USA & NCL)
  p2c <- tibble(ymin = c(0,1,17,32), ymax = c(1,17,32,53), fill = c("Oceania*", "Asia", "Africa", "Americas*")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 54), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  
  sfig_ir <- p2c + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_IR_2023-04-03.png",
         sfig_ir, width=7.04, height=5.75, dpi=300) 
  

#### SFig 9: biophysical covars difference between IPL & 10km buffer  ----
                          
covar <- read_csv("./Output/Covar_MeanValues.csv") %>% 
    separate(col=Covar, into=c("Area","Covar"), sep="_") %>% 
    pivot_wider(names_from=Area, values_from=MeanValue) %>% 
    mutate(meanDiff = IPL-buff10,
           Covar = factor(Covar, levels=c("elevation","slope","popDens","travelTime"),
                          labels=c("Elevation","Slope","Population density","Travel time")),
           Country = factor(Country, levels=c("NCL","AUS",
                                              "VNM","TWN","THA","PHL","PAK","NPL","MYS",
                                              "MMR","LKA","LAO","KHM","IND","IDN","CHN","BGD",
                                              "UGA","TZA","TGO","RWA","NGA","KEN","GAB","ETH",
                                              "COG","COD","CMR","CIV","CAF","BEN","BDI",
                                              "VEN","USA","SUR","SLV","PRY","PER","PAN","NIC",
                                              "MEX","HND","GUY","GUF","GTM","ECU","DMA","CRI",
                                              "COL","BRA","BOL","BLZ","ARG")))
  
p1 <- ggplot() +
  facet_wrap(~Covar, scales="free",  nrow=1) +
  geom_vline(xintercept=0, linetype=2) +
  geom_point(data=covar, aes(y=Country, x=meanDiff), size=0.8) +
  theme_classic() + 
  ylab("") +
  xlab("Difference between mean value in IPL and 10 km-buffer area") +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8)) 
  
# make an additional plot to label continents (this inc USA)
p2b <- tibble(ymin = c(0,2,18,33), ymax = c(2,18,33,54), fill = c("Oceania", "Asia", "Africa", "Americas")) %>% 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
  geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
  scale_y_continuous(breaks = seq(1, 54), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
  guides(fill = "none") +
  theme_void()

sfig_covar <- p2b + p1 + plot_layout(widths = c(1, 10))

ggsave("../Output/Manuscript/SFigure_covar_2023-04-03.png",
       sfig_covar, width=8.84, height=5.75, dpi=300) 
