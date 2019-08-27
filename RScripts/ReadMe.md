### Directory and description of scripts provided
*Scripts are categorized by the figure they correspond to in the manuscript*

**Figure 1: Human population distribution relative to water availability trends**\
`PopulationDistribution.R`
> Takes the Rodell et al. (2018) source data of trends in freshwater availability, the World Resources Institue Aqueduct (2019 release) baseline water stress mapping (provided at HydroBASINS level 6), and the 2015 gridded world population (GWPv4) to determine the global distribution of the human population relative to trendsin water availability, classified by water stress. Also determines the population weighted mean water availability trend per water stress class (replaces stand-alone script WaterStressMean.R, which has been archived).

**Figure 2: Human activities and considerations assessed relative to trends in water availability**\
`CroplandDensity.R`
> Uses the Rodell et al. (2018) source data, the Ramankutty et al. (2008) Global Agricultural Lands dataset, and EarthStat's CropAllocation to Food, Feed, Nonfood dataset to determine the range of emerging water availability trends per cropland land use density, supplemented with the percent of global kilocalorie production occuring per cropland land use density.

`EconomicRegionGDP.R`
> Uses Rodell et al. (2018) source data, the MAgPIE classification of world regions, and Kummu et al. (2018) global gridded GDP (PPP), to produce density plots of GDP relative to TWS trends in each of the ten world regions.

`LandUse.R`
> Uses Rodell et al. (2018) source data and the ESA CCI LC project land use data to plot boxplots of the distribution of TWS trends within seven broad land uses.

`Biodiversity.R`
> Uses Rodell et al. (2018) source data, and BiodiversityMapping.org's amphibian species richness dataset to plot boxplots of the distribution of TWS trends within ten percentile-based ranges of species richness.

**Figure 3: Water security vulnerability**\
`GRACEmodified_WRIStatus.R`
> Uses the Rodell et al. (2018) source data to coarsely modify the WRI Aqueduct Water Risk Atlas (2019 release) mapping of water stress and riverine flooding risk.
>> **Data product:** 'GRACEmod_FLOODS.tif' \
>> **Data product:** 'GRACEmod_STRESS.tif'

`VulnerabilityFilter.R`
> Derives the two sub-indicators that create the vulnerability filter; and then combines the two sub-indicators to create the vulnerability filter. Uses the 2018 Corruptions Perceptions Index, GNI per capita, and protected area density (WDPA data) to derive the *Institutional Resilience Capacity* indicator, and the GWPv4 population count, Kummu et al.'s (2018) gridded GDP (PPP), and BiodiversityMapping.org's amphibian species richness to derive the *Sustainability Pillar Prevalence* indicator.
>> **Data product:** 'Resilience.ind.tif' \
>> **Data product:** 'Prevalence.ind.tif' \
>> **Data product:** 'Vulnerability.filter.tif'

`WaterSecurityVulnerability.R`
> Uses three data products (from above) to create global maps of water security vulnerability. Passes both `GRACEmod_FLOODS.tif` and `GRACEmod_STRESS.tif` through the `Vulnerability.filter.tif`.
>> **Data product:** 'WSVI_FLOOD.tif' \
>> **Data product:** 'WSVI_STRESS.tif'

`WSV_stats.R`
> Determines the distribution of water security vulnerability (WSV) in each of the 10 MAgPIE world regions.

**Figure 4: Virtual Water Trade**\
`VirtualWaterTrade.R`
> Uses  Hoekstra and Mekonnen's (2012) net national virtual water import flows and the the Rodell et al. (2018) source data to plot national net virtual water import per nation against net national trends in water storage. 

**Supporting Information**\
`SI_TernaryPlots.R`
> Uses the same data inputs as PopulationDistribution.R, but categorizes the percent of the population within each water stress class as being situated in either: wetting, drying, or stable conditions. 
