# Software and code

## Data collection

Canopy foliage phenology data were extracted from the MCD12Q2 V6 Land Cover Dynamics product (a.k.a. MODIS Global Vegetation Phenology product) via Google Earth Engine (https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2#description). 

## Data analysis

Data were analysed in the open source statistical software R (version 4.0). We used packages climwin v.1.2.3 (https://cran.r-project.org/web/packages/climwin/index.html), dplR v.1.0.2, and bootRes v1.2.4, rstanarm v.2.21.1, and functions from Rdendrom (https://github.com/seanmcm/RDendrom/). We used climpact software v.1.2.8 (see www.climpact-sci.org). All custom code is available through the EcoClimlab GitHub repository (https://github.com/EcoClimLab/growth_phenology) and archived in Zenodo (DOI: [TBD]).

# Data

The datasets generated and analysed during the current study are available via GitHub in the growth_phenology repository of the ForestGEO Ecosystems & Climate Lab @ SCBI, (https://github.com/EcoClimLab/growth_phenology) and archived in Zenodo (DOI [TBD]). Master versions of the dendrometer band data are available for SCBI via GitHub in the Dendrobands repository of the Smithsonian Conservation Biology Institute ForestGEO plot (https://github.com/SCBI-ForestGEO/Dendrobands), which is archived in Zenodo (DOI 10.5281/zenodo.5551143), and for Harvard Forest via the Harvard Forest Data Archive (https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF149). Weather data for SCBI were obtained from the ForestGEO Climate Data Portal v1.0 (https://github.com/forestgeo/Climate/tree/master/Climate_Data/Met_Stations/SCBI), which is archived in Zenodo (DOI: 10.5281/zenodo.3958215), and the National Center for Environmental Information (NCEI) weather station located in Front Royal, Virginia (https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00443229/detail), downloaded on [DATE]. Weather data for Harvard Forest are available through the Harvard Forest Data Archive (https://harvardforest.fas.harvard.edu/harvard-forest-data-archive) (https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF001 AND https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF000). Climate data were obtained from CRU v.4.04 via the ForestGEO Climate Data Portal v1.0 (https://github.com/forestgeo/Climate/tree/master/Climate_Data/CRU), which is archived in Zenodo (DOI: 10.5281/zenodo.3958215). The Standardised Precipitation-Evapotranspiration Index was obtained from the ForestGEO Climate Data Portal v1.0 (https://github.com/forestgeo/Climate/tree/master/Climate_Data/SPEI), which is archived in Zenodo (DOI: 10.5281/zenodo.3958215). Canopy foliage phenology data were extracted from the MCD12Q2 V6 Land Cover Dynamics product (a.k.a. MODIS Global Vegetation Phenology product) via Google Earth Engine (https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2#description). Original tree cores are archived at the institutions of various members of the author team and will be made available upon reasonable request. 

# Ecological, evolutionary & environmental sciences study design

## Study description

Using dendrometer band measurements from 463 trees across two forests, we show that warmer spring temperatures shifted the woody growth of deciduous trees earlier but had no consistent effect on peak growing season length, maximum daily growth rates, or annual growth. The latter finding was confirmed on the centennial scale by 207 tree-ring chronologies from 108 forests across eastern North America, where annual growth was far more sensitive to temperatures during the peak growing season than in the spring.

## Research sample

The data used here were dendrometer band measurements from the Smithsonian Conservation Biology Institute (SCBI) and Harvard Forest. Tree-ring chronologies were created using cores from these two sites along with an additional 106 sites.  

## Sampling strategy

Dendrometer bands:

At SCBI, dendrometer bands were installed on indiviuals of the two most dominant (in terms of contributions to woody productivity) ring-porous species (*Quercus alba* and *Quercus rubra*) and the two most dominant diffuse-porous species (*Liriodendron tulipifera* and *Fagus grandifolia*). Sampling was weighted towards large individuals. Banded trees were randomly located throughout the 25.6 ha ForestGEO plot, with some additional individuals within two 15 m radius plots in which all trees were banded. Details are available in Extended Data Table 1 and the SCBI-ForestGEO dendrobands repository (https://github.com/SCBI-ForestGEO/Dendrobands). 

At Harvard Forest, dendrometer bands were placed on trees in 36 circular, 10-m radius plots located randomly along eight 500-m transects that extended from the EMS eddy-flux tower (detailed in D’Orangeville et al., 2021; https://doi.org/10.1093/treephys/tpab101). We analyzed data for eight diffuse- and three ring-porous species (see Extended Data Table 1). 

Tree cores:

At SCBI, we sampled the 12 species contributing most to woody productivity, sampling live and dead individuals randomly located throughout the plot (detailed in Helcoski et al. 2019; https://doi.org/10.1111/nph.15906). At Harvard Forest, all trees were cored within size-stratefied circular plots (detailed in Dye et al. 2016; https://doi.org/10.1002/ecs2.1454).

The tree-ring records from our focal sites were complemented with a much larger collection of tree-ring chronologies spanning 106 deciduous and mixed forest sites in Eastern North America (see Maxwell et al. 2020; https://doi.org/10.5194/cp-16-1901-2020).


## Data collection

Dendrometer bands:


Metal dendrometer bands were installed on a total of 941 trees within the SCBI and Harvard Forest ForestGEO plots. They were placed at ~1.4m above the base of the tree. Bands were measured with a digital caliper approximately every 1-2 weeks within the growing season from 2011-2020 at SCBI and 1998-2003 at Harvard Forest. The number of bands measured at each site fluctuated slightly as trees were added or dropped from the census (e.g., because of tree mortality). Across years, the number of bands sampled averaged 129 (range: 91-138) at SCBI and 717 (range: 700-755) at Harvard Forest.  In total, our analysis included 2459 tree-years (Extended Data Table 1).

Measurements were timed to begin before the beginning of spring growth and to continue through the cessation of growth in the fall. At SCBI, the median start date was April 14, which was adjusted forward when early leaf-out of understory vegetation was observed, with the earliest start date being March 30 (in 2020). Measurements were continued through to fall leaf senescence, with the median end date being October 17 and the latest end date November 26 (2012). Timing of measurements at Harvard Forest were similar, with the median start date of April 23 and median end date of October 30. 1998 was an anomalous year where initial measurements were taken on January 5, but not taken again until April 15. The latest end date was November 11, 2002.

Leaf phenology:

Canopy foliage phenology data for the years 2001-2018 were extracted for SCBI and Harvard Forest from the MCD12Q2 V6 Land Cover Dynamics product (a.k.a. MODIS Global Vegetation Phenology product) via Google Earth Engine. Extracted pixels were those containing the NEON tower at each site.

Weather data: 

Climate data corresponding to the measurement periods were obtained from local weather stations at each focal site. For SCBI, weather data were obtained from a meteorological tower adjacent to the ForestGEO plot, via the ForestGEO Climate Data Portal v1.0 (https://forestgeo.github.io/Climate/). The R package climpact (see www.climpact-sci.org) was used to plot temperatures for visual inspection and to identify readings that were >3 standard deviations away from yearly means, which were labeled as outliers and removed from the dataset. Gaps in the SCBI meteorological tower data were subsequently filled using temperature readings obtained from a National Center for Environmental Information (NCEI) weather station located in Front Royal, Virginia (https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00443229/detail). Daily temperature records for Harvard Forest, which had already been gap-filled based on other local records, were obtained from the Harvard Forest weather station. For each site, we used records of daily maximum (T_max) and minimum temperatures (T_min).

Tree cores: 

All tree cores had been previously collected, cross-dated, and measured using standard collection and processing methodologies. Details are provided in the original publications cited in the manuscript.

## Timing and spatial scale

Dendrometer band measurements were taken in the nearby forestGEO plots with no interruption in collection from 2011-2020 at SCBI and from 1998-2003 at Harvard Forest. Tree cores were taken opportunistically at 108 sites across the eastern North America, with chronologies spanning from 1901-2016.
