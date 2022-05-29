# Where do we find the highest concentrations of livestock in Kenya?

## A set of visualizations looking at livestock distribution in Kenya.

#### References/Data Sources
1) 2019 Kenya Population and Housing Census
2) Shelmith Kariuki (2020). rKenyaCensus: 2019 Kenya Population and Housing Census Results. R package version 0.0.2.

### 1) What is the population of various types of livestock in Kenya?

A brief summary of the total livestock population (raw numbers) in Kenya.

#### [Code](https://github.com/wokech/livestock_kenya/blob/main/R_scripts/livestock_kenya_national.R)
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_national/treemap_livestock_national.png)

### 2) Where are the pastoral livestock found?
#### a) How many pastoralist livestock are found in your county?
Notes
1) Here we define pastoralist livestock as indigenous cattle, sheep, and goats. 
2) Goats were not classified as exotic dairy or indigenous

#### [Code](https://github.com/wokech/livestock_kenya/blob/main/R_scripts/livestock_kenya_county_pasto.R)
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_county_pasto/all_counties_livestock_pasto_barplot_map.png)

#### b) How many pastoralist livestock are there for every farming household in the county?

#### [Code](https://github.com/wokech/livestock_kenya/blob/main/R_scripts/livestock_kenya_county_pasto.R)
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_county_pasto/all_counties_livestock_pasto_barplot_map_household.png)

### 4) Where are you most likely to see indigenous cattle, sheep, and goats?
#### Here, we look at the subcounties with the highest number of goats, sheep, and indigenous cows

#### [Code](https://github.com/wokech/livestock_kenya/blob/main/R_scripts/livestock_kenya_subcounty_pasto.R)

### Pastoral livestock
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_subcounty_pasto/subcounty_top10_pasto.png)

### Indigenous cattle
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_subcounty_pasto/subcounty_top10_indi_cow.png)
### Sheep
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_subcounty_pasto/subcounty_top10_sheep.png)
### Goats
![alt text](https://github.com/wokech/livestock_kenya/blob/main/images/livestock_kenya_subcounty_pasto/subcounty_top10_goats.png)
