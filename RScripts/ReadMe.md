### Directory and description of scripts provided
1. PopulationDistribution.R
   - Takes the Rodell et al. (2018) source data of emerging trends in freshwater availability, the World Resources Institue HydroBASIN scale water stress mapping, and the GWPv4 2015 gridded population of the world to determine the global distribution of the human population relative to emerging trends in water availability and water stress classes, simultaneously. 
2. WaterStressMean.R
    - Uses the same input as ```PopulationDistribution.R``` to determine the population-weighted mean emerging trend in freshwater availability in each water stress class.
3. CroplandDensity.R
    - Uses the Rodell et al. (2018) source data, the Ramankutty et al. (2008) Global Agricultural Lands dataset, and EarthStat's CropAllocation to Food, Feed, Nonfood dataset to determine the range of emerging water availability trends per cropland land use density, and to supplement with the percent of global kilocalorie production occuring per cropland land use density as well.
4. WorldRegionGDP.R
    - Uses the Rodell et al. (2018) source data, the MAgPIE model's definition of the 10 economic regions of the world, and the Kummu et al. (2018) global gridded GDP (PPP) datasets to determine the GDP (PPP) distribution per emerging water availability trend in each of the 10 economic regions. 
5. WaterSecurityVulnerabilityIndex.R
    - Uses the GWPv4 population, Kummu et al. (2018) GDP (PPP), and BiodiversityMapping.com's amphibian species richness dataset to create an indicator of the prevalence of the three primary sustainability pillars.
    - Uses the Transparency International Corruption Perceptions Index, the World Bank's data on national wealth per capia, and the World Database of Protected Areas to create an indicator of insitutional resilience capacity.
    - Combines the sustainability pillar and resilience capacity indicators (described above) to create a vulnerability filter (available on repository).
    - Uses the Rodell et al. (2018) source data to categorically modify the World Resources Institue mapping of water stress and flood occurence.
    - Passes this modified categorical global map of water stress and flood occurance through the derived vulnerability filter to determine the global water security vulnerability (available on repository). 
6. VirtualWaterTrade.R
    - Uses the Rodell et al. (2018) source data and the Hoekstra et a. (2012) virtual water flows dataset to plot national net virtual water import per nation against net national trends in water storage. 
