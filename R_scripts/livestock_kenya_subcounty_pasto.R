# What is the distribution of pastoralist livestock in Kenya
# By @willyokech
# Data: rKenyaCensus

#1) Load the required packages

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(janitor)

### To filter out the FOREST data
### filter_all(any_vars(grepl("FOREST", .)))
###

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Load the required data

df_livestock <- V4_T2.24

View(df_livestock)

# Table 1 for County and SubCounty Analysis
table_1_pasto <- df_livestock[2:393,]
View(table_1_pasto)

glimpse(table_1_pasto)

table_1_pasto <- table_1_pasto %>%
  clean_names()

table_1_pasto_select <- table_1_pasto %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) %>%
  mutate(total_pasto_livestock = sheep + goats + indigenous_cattle) %>%
  mutate(ind_cattle_farm_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_farm_household = round(goats/farming)) %>%
  mutate(sheep_farm_household = round(sheep/farming)) %>%
  mutate(total_pasto_farm_household = round(total_pasto_livestock/farming))

View(table_1_pasto_select)
glimpse(table_1_pasto_select)

table_1_pasto_select_subcounty <- table_1_pasto_select %>%
  filter(admin_area == "SubCounty")

# Top 10 subcounties for sheep, goats, and indigenous cows combined

table_1_pasto_select_subcounty_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, total_pasto_livestock) %>%
  unite("county_sub", county, sub_county, sep = ", ") %>%
  arrange(desc(total_pasto_livestock)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_top10)

# Top 10 subcounties for sheep

table_1_pasto_select_subcounty_sheep_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, sheep) %>%
  unite("county_sub", county, sub_county, sep = ", ") %>%
  arrange(desc(sheep)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_sheep_top10)

# Top 10 subcounties for goats

table_1_pasto_select_subcounty_goats_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, goats) %>%
  unite("county_sub", county, sub_county, sep = ", ") %>%
  arrange(desc(goats)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_subcounty_goats_top10)

# Top 10 subcounties for indigenous cows

table_1_pasto_select_county_indi_cow_top10 <- table_1_pasto_select_subcounty %>%
  select(county, sub_county, admin_area, indigenous_cattle) %>%
  unite("county_sub", county, sub_county, sep = ", ") %>%
  arrange(desc(indigenous_cattle)) %>%
  ungroup %>%
  slice(1:10)

View(table_1_pasto_select_county_indi_cow_top10)

