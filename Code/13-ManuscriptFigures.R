#### ============ Make figures for manuscript ============== ####
# Note: done in local computer, R version 4.1.1
# TO BE FINALISED, DEPENDING ON WHETHER WE WANT SPECIES SILHOUETTE
# OR JUST FACET LABELS
# 19 Feb: decided to switch figures 1 and 2
# rphylopic image_data gives SSL certificate problem: 
# certificate has expired error. so for the new
# no of species plot sticking with the facet labels

library(tidyverse)
library(patchwork) 
library(sf) # for map fig 3
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer) 
library(rphylopic) # for animal silhouettes. see https://jacintak.github.io/post/2021-08-01-rphylopic/
library(egg) # to use geom_custom to put different animal silhouettes in each facet
sf::sf_use_s2(FALSE)
# tidyverse v1.3.1, patchwork v1.1.2, sf v1.0.4, rnaturalearth v0.1.0, rnaturalearthdata v0.1.0, RColorBrewer v1.1.2
# rphylopic v0.3.0, egg v0.4.5, 

## main dataset
aoh2020 <- read_csv("../Data/Processed/ForestSpeciesList_AOH2020_ThreatScore_2022-11-16.csv",
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
         NewCategory = factor(NewCategory, levels=c("DD", "Threatened", "NT/LC"))) # 11601 obs


summary(aoh2020)    
# Species               Class       Category                Habitat    
# Length:11601       AMPHIBIA:1456   DD:1004   Forest important  :5606  
# Class :character   AVES    :6398   CR: 484   Forest unimportant:2402  
# Mode  :character   MAMMALIA:1725   EN:1026   Forest generalist :3593  
#                    REPTILIA:2022   VU:1003                            
#                                    NT: 977                            
#                                    LC:7107    

## taxa silhouette (uid from website)
amphibia <- image_data("c387bde8-a9de-4101-928b-e99f2a7a33e5", size=256)[[1]]
aves <- image_data("3a4cdd72-e553-40ad-838e-3b23037b2010", size=256)[[1]]
mammalia <- image_data("87047da1-b40e-4b31-8492-4db262f129f5", size=256)[[1]]
reptilia <- image_data("4210000f-4b54-49e7-8a81-6f36ae567f67", size=256)[[1]]
# using annotation_custom doesnt work; it plots the same image for each facet
# annotation_custom(grid::rasterGrob(amphibia, interpolate=TRUE), xmin=0.3, xmax=0.8, ymin=300, ymax=400) +
# using geom_custom with grid::rasterGrob in grob_fun argument instead
as <- list(amphibia, aves, mammalia, reptilia)
asData <- data.frame(Class=c("Amphibians", "Birds", "Mammals", "Reptiles"), data=I(as),
                     x=0.7, y1=c(800,4000,1000,1000),
                     y2=0.75)

## ---- fig 1 - range histograms ----
## violin plot summaries of fraction of overlap for
## A. all species
## B. threatened species only

class20 <- aoh2020 %>% 
  mutate(fracIPL = fracIL + fracPIA,
         fracPA = fracPA + fracPIA) %>% 
  select(Class, Category, fracIPL, fracPA, fracUnPro) %>% 
  pivot_longer(cols=fracIPL:fracUnPro, names_to="ProType", names_prefix="frac", values_to="FracCovered") %>% 
  mutate(ProType = factor(ProType, levels=c("UnPro", "PA","IPL"),
                          labels=c("None", "PA", "IPL")),
         Class = factor(Class, labels=c("Amphibians", "Birds", "Mammals", "Reptiles")))

class20_thr <- aoh2020 %>% 
  filter(NewCategory == "Threatened") %>% 
  mutate(fracIPL = fracIL + fracPIA,
         fracPA = fracPA + fracPIA) %>% 
  select(Class, Category, fracIPL, fracPA, fracUnPro) %>% 
  pivot_longer(cols=fracIPL:fracUnPro, names_to="ProType", names_prefix="frac", values_to="FracCovered") %>% 
  mutate(ProType = factor(ProType, levels=c("UnPro", "PA","IPL"),
                          labels=c("None", "PA", "IPL")),
         Class = factor(Class, labels=c("Amphibians", "Birds", "Mammals", "Reptiles")))

# change the x value for asData so it fits
# asData$x = 0.5

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
  # geom_custom(data = asData, aes(data=data, x=x, y=y2),
  #             grob_fun = function(x) grid::rasterGrob(x, interpolate = TRUE, width=unit(1,'cm'))) +
  # expand_limits(x=c(0, 3))

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
        # strip.background = element_blank(),
        # strip.text = element_blank())

fig1 <- p1 + p2 + plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=10))
ggsave("../Output/Manuscript/Figure1_2023-03-08.png",
       fig1, width=8, height=4.4, dpi=300)

## ---- fig 2 - range overlap bars ----
# summary figure of no. of species in
# A. different areas (none, PA or IPL) for each category
# B. different red list category (DD, threatened, NT, LC) for each % of overlap

## get the dataframe for A
# for species whose range fall within IL
sumIPL <- aoh2020 %>% 
  filter(fracIL==1) %>% 
  # filter(fracIL != 0) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "IPL only")
# for species whose range fall within PA
sumPA <- aoh2020 %>% 
  filter(fracPA==1) %>% 
  # filter(fracPA != 0) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "PA only")
# for species whose range fall within PIA
sumPIA <- aoh2020 %>% 
  filter(fracPIA==1) %>% 
  # filter(fracPIA != 0) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "PIA only")
# for species whose range fall neither within IPL nor PA
sumNONE <- aoh2020 %>% 
  filter(fracUnPro == 1) %>% 
  count(Class, NewCategory) %>% 
  mutate(Area = "None")
d1 <- bind_rows(sumNONE, sumPA, sumPIA, sumIPL) %>% 
  mutate(Area = factor(Area, levels=c("None","PA only","PIA only","IPL only"))) 

# thrPalette <- c("#67000d","#fc7050","#404040")
# dark red, dark pink, dark grey
thrPalette <- c("#d42020", "#fdbea5","#404040")
# red, light pink, dark grey

## get the dataframe for B
d2 <- aoh2020 %>% 
  select(Species, Class, NewCategory, fracIL, fracPIA) %>%
  mutate(fracIPL = fracIL + fracPIA,
         percentHabitat_IPL=case_when(fracIPL==0 ~ 0,
                                      fracIPL<=.2 & fracIPL!=0 ~ 20,
                                      fracIPL>.2 & fracIPL<=.4 ~ 40,
                                      fracIPL>.4 & fracIPL<=.6 ~ 60,
                                      fracIPL>.6 & fracIPL<=.8 ~ 80,
                                      fracIPL>.8 & fracIPL<1 ~ 99,
                                      TRUE ~ 100)) %>% 
  select(Class, NewCategory, percentHabitat_IPL) %>% 
  add_count(Class, NewCategory, percentHabitat_IPL, name="nSpecies_IPL") %>% 
  distinct() %>% 
  arrange(Class, NewCategory, percentHabitat_IPL) %>% 
  mutate(percentHabitat_IPL=factor(percentHabitat_IPL,
                                   levels=c(0, 20, 40, 60, 80, 99, 100),
                                   labels=c("0", "<20", "20-40", "40-60", "60-80", "80-99", "100"))) %>% 
  filter(percentHabitat_IPL != 0) 

thrPalette2 <- c("#404040","#bababa","#fdbea5","#fc7050","#d42020","#67000d")
# dark grey,  light grey, med pink, dark pink, red, dark red

p1 <- ggplot(d1) +
  facet_grid(rows=vars(Class), scales="free") +
  geom_col(aes(x=Area, y=n, colour = NewCategory, fill=NewCategory), width=0.4)+ 
  # position=position_stack(reverse=TRUE)) +
  ylab('No. of species') +
  xlab('') +
  labs(fill="IUCN Red List \ncategories") +
  guides(colour="none") +
  scale_fill_manual(values=thrPalette) + # scale_fill_grey()
  scale_colour_manual(values=thrPalette) + # scale_colour_grey()
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.position='bottom') #+
  # geom_custom(data = asData, aes(data=data, x=x, y=y1),
  #             grob_fun = function(x) grid::rasterGrob(x, interpolate = TRUE, width=unit(1,'cm')))

p2 <- ggplot(d2) +
  facet_grid(rows=vars(Class), scales="free") +
  geom_col(aes(x=NewCategory, y=nSpecies_IPL, colour = percentHabitat_IPL, fill=percentHabitat_IPL), 
           position=position_stack(reverse=TRUE), width=0.4) +
  ylab('No. of species') +
  xlab('') +
  labs(fill="Habitat overlap \nwith IPLs (%)") +
  guides(fill=guide_legend(reverse=TRUE), colour="none") +
  scale_fill_manual(values=thrPalette2) + # scale_fill_grey()
  scale_colour_manual(values=thrPalette2) + # scale_colour_grey()
  theme_classic() +
  theme(axis.title=element_text(size=11),
        axis.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        legend.position='bottom') 
  
# need to make plots a bit wider so legend doesn't get cut off. 
fig2 <- p1 + p2 + plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=10))
ggsave("../Output/Manuscript/Figure2_2023-03-20.png",
       fig2, width=7.2, height=5.8, dpi=300)



## ---- fig 3 - actual values in IPL----
## map of countries with values of 
## A species richness, B threat score, C inverse range

# mean/median value in IPL (going with median values)
obsMeanDF <- read.csv("../Data/Processed/CountryData/ObservedMeanValue.csv") %>% 
  pivot_wider(names_from=Scenario,
              values_from=Value) %>% 
  select(-c(Taxa,Area))
obsMedDF <- read.csv("../Data/Processed/CountryData/ObservedMedianValue_2023-02-10.csv") %>% 
  pivot_wider(names_from=Scenario,
              values_from=Value) %>% 
  select(-c(Taxa,Area))
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
  left_join(obsMedDF, by=c("iso_a3"="Country"))

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

# for IR, FJI, DMA & NCL >>>> rest (cut-off ~ 5e-05)
# but only FJI and DMA is >2sd from mean (7.204476e-05)
p3 <- ggplot(data=filter(world, IR<0.00005|is.na(IR))) +
  theme_void() +
  geom_sf(aes(fill=IR)) +
  scale_fill_distiller(palette='YlOrRd', direction=1, na.value="#bababa") +
  labs(fill="Range-size rarity") + 
  theme(legend.title=element_text(size=10),
        legend.text=element_text(size=8, angle=45, hjust=1),
        legend.position='bottom',
        plot.margin=grid::unit(c(0,0,0,0), "mm"))  + 
  guides(fill=guide_colourbar(title.vjust=1))

fig3 <- p1 / p2 / p3 + plot_annotation(tag_levels="A") &
  theme(plot.tag=element_text(size=10))
ggsave("../Output/Manuscript/Figure3_Median_2023-02-15.png",
       fig3, width=6.83, height=5.75, dpi=300)

#### scatter plot for SOM ####
obsMeanDF <- read.csv("../Data/Processed/CountryData/ObservedMeanValue.csv")
obsMedDF <- read.csv("../Data/Processed/CountryData/ObservedMedianValue_2023-02-10.csv")
regionsDF <- data.frame(Country = c("SLB","NCL","AUS","FJI",
                                  "PAK","CHN","MYS","TWN","LKA","PHL","IDN","KHM","NPL","BGD","VNM","LAO","IND","MMR","THA",
                                  "CMR","NGA","RWA","COD","KEN","UGA","CIV","CAF","TGO","BEN","TZA","GAB","ETH","COG","BDI",
                                  "CRI","DMA","GUY","BRA","SLV","PAN","SUR","BLZ","GUF","HND","PER","BOL","GTM","NIC","MEX","ARG","PRY","USA","VEN","COL","ECU"),
                      Region = c(rep("Oceania", 4),
                                 rep("Asia", 15),
                                 rep("Africa", 15),
                                 rep("Americas", 21)))
df <- obsMedDF %>% 
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
ggsave("../Output/Manuscript/SFigure_3indices_Median_2023-02-14.png",
       sfig, width=7.04, height=5.75, dpi=300)

## ---- fig 4 - mean difference scatterplots ----
## difference in mean values between IPL and 10 km buffer
## for species richness, threat score and inverse range
## (difference between IPL and 50 km buffer or all outside in SOM)

regionsDF <- data.frame(Country = c("SLB","NCL","AUS","FJI",
                                  "PAK","CHN","MYS","TWN","LKA","PHL","IDN","KHM","NPL","BGD","VNM","LAO","IND","MMR","THA",
                                  "CMR","NGA","RWA","COD","KEN","UGA","CIV","CAF","TGO","BEN","TZA","GAB","ETH","COG","BDI",
                                  "CRI","DMA","GUY","BRA","SLV","PAN","SUR","BLZ","GUF","HND","PER","BOL","GTM","NIC",
                                  "MEX","ARG","PRY","USA","VEN","COL","ECU"),
                      Region = c(rep("Oceania", 4),
                                 rep("Asia", 15),
                                 rep("Africa", 15),
                                 rep("Americas", 21)))
DF <- read_csv("../Data/Processed/CountryData/ObservedMeanDifferences_WithPValue.csv") %>% 
  filter(Area=="buff10",
         Taxa=="All") %>% 
  left_join(regionsDF, by="Country") %>% 
  mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                 pValue < 0.05 & Value < 0 ~ "SigNev",
                                 TRUE ~ "NotSig"),
         Significant = factor(Significant, levels=c("SigPos", "SigNev","NotSig")),
         Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas")),
         Scenario = factor(Scenario, levels=c("SR","TS","IR"), labels=c("Species Richness","Extinction vulnerability", "Range-size rarity"))) %>% 
  filter(Country!="USA") # skews plotting for TS and IR when included
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

  colPalette2 <- c("#EE6677","#4477AA","#BBBBBB")
  # red, blue, grey
  
  p1 <- ggplot(DF, aes(x=Value, y=al, colour=Significant)) +
    facet_wrap(~Scenario, scales="free") +
    # tidytext::scale_y_reordered() +
    # geom_segment(aes(x=Min, y=Country, xend=Max, yend=Country), colour="#08519C", size=1, na.rm=TRUE) +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=colPalette2) +
    ylab("") +
    xlab("Difference between mean value in IPL and 10 km buffer zone") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") +
    scale_y_discrete(breaks=new_order, labels=gsub("^.*\\.", "", new_order))
  
  # make an additional plot to label continents
  p2 <- tibble(ymin = c(0,4,20,35), ymax = c(4,20,35,56), fill = c("Oceania", "Asia", "Africa", "Americas\n*")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 55), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  
  fig4 <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/Figure4_2023-02-13.png",
         fig4, width=7.04, height=5.75, dpi=300) 
  
## for buff50 and out for SOM
  DF <- read_csv("../Data/Processed/CountryData/ObservedMeanDifferences_WithPValue.csv") %>% 
    filter(Taxa!="All",
           Area=="out") %>% 
    left_join(regions, by="Country") %>% 
    mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                   pValue < 0.05 & Value < 0 ~ "SigNev",
                                   TRUE ~ "NotSig"),
           Significant = factor(Significant, levels=c("SigPos", "SigNev","NotSig")),
           Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas"))) %>% 
    filter(Country!="USA") # cos plot skews with it

  p1 <- ggplot(DF, aes(x=Value, y=al, colour=Significant)) +
    facet_wrap(~Scenario, scales="free") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=colPalette2) +
    ylab("") +
    xlab("Difference between mean value in IPL and outside IPL") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") +
    scale_y_discrete(breaks=new_order, labels=gsub("^.*\\.", "", new_order))

  sfig4 <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_IPLvsOutside_2023-02-13.png",
         sfig4, width=7.04, height=5.75, dpi=300) 
  
  ## ---- sfig sep taxa ----  
  #### Species richnes ####
  DF <- read_csv("../Data/Processed/CountryData/ObservedMeanDifferences_WithPValue.csv") %>% 
    filter(Taxa!="All",
           Scenario=="SR") %>% 
    left_join(regions, by="Country") %>% 
    mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                   pValue < 0.05 & Value < 0 ~ "SigNev",
                                   TRUE ~ "NotSig"),
           Significant = factor(Significant, levels=c("SigPos", "SigNev","NotSig")),
           Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas")),
           Country = factor(Country, levels=c("SLB","NCL","AUS","FJI",
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
    scale_colour_manual(values=colPalette2) +
    ylab("") +
    xlab("Difference between mean value of species richness in IPL and 10 km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") 
  # make an additional plot to label continents
  p2 <- tibble(ymin = c(0,4,20,35), ymax = c(4,20,35,56), fill = c("Oceania", "Asia", "Africa", "Americas")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 55), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  sfig_sr <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_SR_2023-02-13.png",
         sfig_sr, width=7.04, height=5.75, dpi=300) 
  
  #### threat score ####
  DF <- read_csv("../Data/Processed/CountryData/ObservedMeanDifferences_WithPValue.csv") %>% 
    filter(Taxa!="All",
           Scenario=="TS") %>% 
    left_join(regions, by="Country") %>% 
    mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                   pValue < 0.05 & Value < 0 ~ "SigNev",
                                   TRUE ~ "NotSig"),
           Significant = factor(Significant, levels=c("SigPos", "SigNev","NotSig")),
           Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas")),
           Country = factor(Country, levels=c("FJI","SLB","NCL","AUS",
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
    scale_colour_manual(values=colPalette2) +
    ylab("") +
    xlab("Difference between mean value of threat score in IPL and 10 km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") 
  # make an additional plot to label continents
  p2 <- tibble(ymin = c(0,4,20,35), ymax = c(4,20,35,56), fill = c("Oceania", "Asia", "Africa", "Americas\n*")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 55), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  sfig_ts <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_TS_2023-02-13.png",
         sfig_ts, width=7.04, height=5.75, dpi=300) 
  
  
  
  
  #### range size rarity ####
  DF <- read_csv("../Data/Processed/CountryData/ObservedMeanDifferences_WithPValue.csv") %>% 
    filter(Taxa!="All",
           Scenario=="IR") %>% 
    left_join(regions, by="Country") %>% 
    mutate(Significant = case_when(pValue < 0.05 & Value > 0 ~ "SigPos",
                                   pValue < 0.05 & Value < 0 ~ "SigNev",
                                   TRUE ~ "NotSig"),
           Significant = factor(Significant, levels=c("SigPos", "SigNev","NotSig")),
           Region = factor(Region, levels=c("Oceania","Asia","Africa","Americas")),
           Country = factor(Country, levels=c("NCL","SLB","AUS","FJI",
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
    filter(Country!="USA")
  
  p1 <- ggplot(DF, aes(x=Value, y=Country, colour=Significant)) +
    facet_grid(cols=vars(Taxa), scales="free") +
    geom_vline(xintercept=0, linetype=2) +
    geom_point(size=0.8, na.rm=TRUE) +
    theme_classic() +
    scale_colour_manual(values=colPalette2) +
    ylab("") +
    xlab("Difference between mean value of inverse range in IPL and 10 km buffer area") +
    theme(axis.title = element_text(size=10),
          axis.text = element_text(size=8)) +
    guides(colour="none") 
  # make an additional plot to label continents
  p2 <- tibble(ymin = c(0,4,20,35), ymax = c(4,20,35,56), fill = c("Oceania", "Asia", "Africa", "Americas\n*")) %>% 
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
    scale_y_continuous(breaks = seq(1, 55), expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
    guides(fill = "none") +
    theme_void()
  sfig_ir <- p2 + p1 + plot_layout(widths = c(1, 10))
  
  ggsave("../Output/Manuscript/SFigure_SepTaxa_IR_2023-02-13.png",
         sfig_ir, width=7.04, height=5.75, dpi=300) 
  
  
  
  
## ---- fig 5? SFig? biophysical covars ----
                          
covar <- read.csv("../Data/Processed/CountryData/Covar_MeanValues.csv") %>% 
    separate(col=Covar, into=c("Area","Covar"), sep="_") %>% 
    mutate(Covar = factor(Covar, labels=c("Elevation", "Population density","Slope","Travel time")),
           Area = factor(Area, labels=c("10 km buffer", "IPL")),
           Country = factor(Country, levels=c("SLB","NCL","FJI","AUS",
                                              "VNM","TWN","THA","PHL","PAK","NPL","MYS","MMR","LKA","LAO","KHM","IND","IDN","CHN","BGD",
                                              "UGA","TZA","TGO","RWA","NGA","KEN","GAB","ETH","COG","COD","CMR","CIV","CAF","BEN","BDI",
                                              "VEN","USA","SUR","SLV","PRY","PER","PAN","NIC","MEX","HND","GUY","GUF","GTM","ECU","DMA","CRI","COL","BRA","BOL","BLZ","ARG"))) 
  
test <- covar %>% 
  filter(!Country %in% c("SLB","NCL","FJI"))
p1 <- ggplot() +
  facet_wrap(~Covar, scales="free",  nrow=1) +
  geom_point(data=test, aes(y=Country, x=MeanValue, colour=Area), alpha=0.4) +
  theme_classic()
p2 <- tibble(ymin = c(0,4,20,35), ymax = c(4,20,35,56), fill = c("Oceania", "Asia", "Africa", "Americas")) %>% 
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.5, ymin = ymin, ymax = ymax, fill = fill)) +
  geom_text(aes(x = 0.25, y = (ymin  + ymax) / 2, label = fill), angle = 0, size=3) +
  scale_y_continuous(breaks = seq(1, 55), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(breaks = c(0), expand = expansion(mult = c(0, 0))) +
  guides(fill = "none") +
  theme_void()

fig5 <- p2 + p1 + plot_layout(widths = c(1, 10))

ggsave("../Output/Manuscript/Figure5_2023-03-08.png",
       fig5, width=8.84, height=5.75, dpi=300) 
