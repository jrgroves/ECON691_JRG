#https://www.kaggle.com/datasets/NUFORC/ufo-sightings?resource=download



rm(list = ls())

library(tidyverse)
library(tidycensus)
library(sf)


#Objective 1-A
ufo <- read.csv(file = "./Data/scrubbed.csv", header = TRUE, as.is = TRUE)


#Census Maps with State Lines

cen.stat <- cen.map %>%
  select(-c(variable, estimate, moe)) %>%
  arrange(GEOID) %>%
  filter(GEOID != "02",
         GEOID != "72",
         GEOID != "15")

ggplot(cen.stat) +
  geom_sf() +
  theme_bw()



ufo.us <- ufo %>%
  filter(country == "us") %>%
  select(-comments) %>%
  mutate(date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"),
         year = year(date),
         decade = year - year %% 10) %>%
  filter(decade > 1959)









cen.map <- get_acs(geography = "state", 
                   survey = "acs5",
                   variables = "B01003_001E", 
                   year = 2020,
                   geometry = TRUE)


ufo.map <- ufo.us %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=st_crs(cen.map))



ggplot(ufo.map) +
  geom_sf(aes(color = factor(decade))) + 
  scale_color_viridis_d(option = "C")
