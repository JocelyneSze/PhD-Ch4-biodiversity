# PhD-Ch4-biodiversity
This paper forms chapter 4 of my PhD thesis. It uses IUCN Red List/BirdLife International distributional range files for amphibians, birds, mammals, and reptiles that are dependent on tropical forests, Global Forest Watch data on forest cover in 2020, UNEP-WCMC & IUCN World Database of Protected Areas, and Garnett et al. (2018) Indigenous Peoples' Lands datasets to identify tropical forest-dependent vertebrates' Areas of Habitat in 2020 and their overlaps with PAs and IPLs. It uses the AOH data to create maps of species richness, extinction vulnerability, and range-size rarity to identify countries where IPLs have high values of the above metrics, and to compare these metrics inside and outside of IPLs. 

This repository has a DOI: 10.5281/zenodo.8385939

Paper abstract: 
Indigenous Peoples are long-term custodians of their lands, but only recently has their contributions to conservation been recognised in biodiversity policy and practice. Tropical forest loss and degradation are lower in Indigenous lands than unprotected areas, yet the role of Indigenous Peoples’ Lands (IPLs) in biodiversity protection conservation has not been properly assessed from regional to global scalesis poorly understood. Using species distribution ranges of 11,872 tropical forest-dependent vertebrates to create Area of Habitat maps, we identified the overlap of these species ranges with IPLs and then compared values inside and outside of IPLs for species richness, extinction vulnerability, and range-size rarity. Of assessed vertebrates, 76.8% had range overlaps with IPLs, on average overlapping ~ 25% of range, and at least 120 species were found only within IPLs.  Species richness within IPLs was highest in South America, while IPLs in Southeast Asia and Oceania islands had highest extinction vulnerability, and IPLs in Dominica and New Caledonia within small island nations in the Caribbean and Oceania were important for range-size rarity. Most countries in the Americas had higher species richness within IPLs than outside, whereas most countries in Asia had lower extinction vulnerability scores inside IPLs and range-size rarity varied minimally inside and outside IPLsmore countries in Africa and Asia had slightly higher range-size rarity in IPLs apart from small island countries. Our findings suggest that IPLs provide critical support for tropical forest-dependent vertebrates, highlighting the need for greater inclusion of Indigenous Peoples in conservation target-setting and programme implementation

Files are organised as follows:

Code folder: contains the code used for cleaning, processing, and analysing the data. 

Output folder: contains six csv files for plotting figures. 

ForestSpeciesList_AOH2020.csv = the 11872 tropical forest-dependent species and their 2020 Area of Habitat overlap with PAs, IPLs, overlapping areas of both, and unprotected areas. 

ObservedMedianValues.csv = the country-level median values for species richness, extinction vulnerability, and range-size rarity for IPLs. 

ObservedCountryRanges.csv = the country-level minimum, Tukey lower hinge, median, Tukey upper hinge, and maximum values for species richness, extinction vulnerability, and range-size rarity for IPLs. 

ObservedCountryRanges_WithinTropFor.csv = the country-level minimum, Tukey lower hinge, median, Tukey upper hinge, and maximum values for species richness, extinction vulnerability, and range-size rarity for IPLs, only for species whose entire range fall within tropical forest extents (supplementary analysis). 

MeanDifferences.csv = the country-level difference between mean values for species richness, extinction vulnerability, and range-size rarity between IPLs and buffer zone areas. 

Covar_MeanValues.csv = the country-level mean values for elevation, slope, population density, and travel time in IPLs and 10-km buffer zones
