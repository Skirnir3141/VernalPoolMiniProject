rm(list = ls())
setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

library(dplyr)
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)

# Import vectors
ma <- sf::st_read("./outline25k/OUTLINE25K_POLY.shp")
ma.towns <- sf::st_read("./townssurvey_shp/TOWNSSURVEY_POLY.shp")
verified.pools <- sf::st_read("./GISDATA_CVP_PT/GISDATA_CVP_PTPoint.shp")
potential.pools <- sf::st_read("./pvp/PVP_PT.shp")

# Let's plot...wow, looks like there are a lot of pools
terra::plot(sf::st_geometry(ma.towns), col = "white")
terra::plot(
  sf::st_geometry(sf::st_zm(verified.pools)),
  add = TRUE,
  col = "blue",
  pch = 20,
  cex = .5)

# Add vernal pool counts to towns vector
ma.towns$verified.pools <- lengths(
  sf::st_intersects(
    x = ma.towns,
    y = sf::st_zm(verified.pools)))

# Looks like the vast majority of towns have no pools...but, can that be true?
hist(ma.towns$verified.pools)
ma.towns %>%
  group_by(verified.pools) %>%
  summarise(n = n()) %>%
  arrange(verified.pools)


