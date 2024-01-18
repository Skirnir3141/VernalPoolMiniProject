rm(list = ls())
setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

library(dplyr)
library(tidyverse)
library(terra)
library(sf)
library(rgeos)

# Import vectors
ma <- sf::st_read("./outline25k/OUTLINE25K_POLY.shp")
ma.towns <- sf::st_read("./townssurvey_shp/TOWNSSURVEY_POLY.shp")
verified.pools <- sf::st_read("./GISDATA_CVP_PT/GISDATA_CVP_PTPoint.shp")
potential.pools <- sf::st_read("./pvp/PVP_PT.shp")

# Let's plot...wow, looks like there are a lot of pools!
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
ma.towns$potential.pools <- lengths(
  sf::st_intersects(
    x = ma.towns,
    y = sf::st_zm(potential.pools)))

# Looks like the vast majority of towns have no pools...but, can that be true?
hist(ma.towns$verified.pools)
ma.towns %>%
  dplyr::group_by(verified.pools) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(verified.pools)

# Oh wait, maybe these are mostly small towns, especially in urban areas?
terra::plot(
  sf::st_geometry(ma.towns[ma.towns$verified.pools == 0, ]),
  col = "grey")

# They are way way smaller than the towns with pools in them.
mean(ma.towns[ma.towns$verified.pools == 0, ]$SHAPE_AREA)
mean(ma.towns[ma.towns$verified.pools > 0, ]$SHAPE_AREA)
dplyr::arrange(ma.towns[ma.towns$verified.pools == 0, ], SHAPE_AREA)

# But, something is weird...there are 1239 rows in the municipalities sf, but
# per Wikipedia there are only 292 towns and 59 cities in Massachusetts.
nrow(ma.towns)

# Looks like there are many rows for some towns
ma.towns %>%
  dplyr::group_by(TOWN) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# I think what's going on is just that some of these towns have complicated
# geos with many islands and such...so, require many polygons.  Okay, cool,
# we're good.
print(ma.towns[ma.towns$TOWN == "WESTPORT", ], n = 100)
terra::plot(ma.towns[ma.towns$TOWN == "WESTPORT", ][1], col = "grey")

# Looking at just towns with pools, looks like there are plenty with multiple
# polygons
ma.towns[ma.towns$TOWN %in% ma.towns[ma.towns$verified.pools > 0, ]$TOWN, ] %>%
  dplyr::group_by(TOWN) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# Looks like town metadata like population is the same across rows, but area
# and my vernal pool count are distinct to each row. So, okay, fair enough, I
# can't just group at the town level to get derived stats.
ma.towns[ma.towns$TOWN == "WESTPORT", ]

# Cool, so, here are some stats on pool density by area and population density.
ma.towns.agg <- sf::st_drop_geometry(
  ma.towns[ma.towns$TOWN %in% ma.towns[ma.towns$verified.pools > 0, ]$TOWN, ]) %>%
  dplyr::group_by(TOWN, POP2010) %>%
  dplyr::summarise(pools = sum(verified.pools), sm = sum(SQUARE_MIL)) %>%
  dplyr::mutate(ppsm = pools / sm, pppd = pools / (POP2010 /  sm))
dplyr::arrange(ma.towns.agg, desc(ppsm))
dplyr::arrange(ma.towns.agg, desc(pppd))

# Let's add these to the main towns sfc object and plot them
# Pretty interesting, vernal pool density by area and by population density are
# pretty different, although you do get some overlap...wth is going on in
# Hubbardston??
ma.towns <- left_join(
  ma.towns, ma.towns.agg[, c("TOWN", "ppsm", "pppd")],
  by = "TOWN")
terra::plot(ma.towns["ppsm"])
terra::plot(ma.towns["pppd"])

# Next step will have to be looking at land use...where is pool density high
# along with high development?  Which pools fall within MA DEP wetlands and so
# have a higher likelihood of havivng WPA protection?
#
# Also, may want to look at potential vernal pools rather than verified.  If a
# pool is already verified, it is (conceptually) protected.  Should I look at
# high absolute or relative potential vernal pools?
#
# Maybe the project is advising people on where to focus on verifying additoinal
# pools?

# Might be fun to plot Hudson and Berlin
terra::plot(
  sf::st_geometry(
    ma.towns[ma.towns$TOWN %in% c("HUDSON", "BERLIN"), ]["TOWN"]),
  col = "white")
terra::plot(
  sf::st_geometry(sf::st_zm(verified.pools)),
  add = TRUE,
  col = "blue",
  pch = 20,
  cex = .5)
terra::plot(
  sf::st_geometry(sf::st_zm(potential.pools)),
  add = TRUE,
  col = "black",
  pch = 2,
  cex = .5)

# Let's import the wetland objects and plot it vs pools for Hudson
wetlands <- sf::st_read("./wetlandsdep/WETLANDSDEP_POLY.shp")
hudson.wetlands <- wetlands[
  unlist(
  sf::st_intersects(
    x = ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"],
    y = wetlands)), ]
hudson.cvp <- verified.pools[
  unlist(
    sf::st_intersects(
      x = ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"],
      y = sf::st_geometry(sf::st_zm(verified.pools))
  )), ]
hudson.pvp <- potential.pools[
  unlist(
    sf::st_intersects(
      x = ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"],
      y = sf::st_geometry(sf::st_zm(potential.pools))
    )), ]
terra::plot(
  sf::st_geometry(ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"]),
  col = "white")
terra::plot(
  sf::st_geometry(hudson.wetlands),
  add = TRUE,
  col = "blue")
terra::plot(
  sf::st_geometry(sf::st_zm(hudson.cvp)),
  add = TRUE,
  col = "green",
  pch = 17,
  cex = .5)
terra::plot(
  sf::st_geometry(sf::st_zm(hudson.pvp)),
  add = TRUE,
  col = "red",
  pch = 4,
  cex = .5)

# Okay, now let's flag potential pools that are inside wetlands.  If one is
# within a DEP wetland, then it at least potentially has WPA protection.  I'll 
# keep focusing on Hudson pools just to make this tractable.

hudson.pvp$within.wetland <- lengths(
  sf::st_intersects(hudson.pvp, hudson.wetlands))
dev.off()
terra::plot(
  sf::st_geometry(ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"]),
  col = "white")
terra::plot(
  sf::st_geometry(hudson.wetlands),
  add = TRUE,
  col = "blue")
terra::plot(
  sf::st_geometry(sf::st_zm(hudson.pvp[hudson.pvp$within.wetland == 0, ])),
  add = TRUE,
  col = "green",
  pch = 17,
  cex = .5)

# Seems to have worked well.  Okay, now, for potential pools that are not in
# wetlands, let's calculate the distance to the nearest wetland object.

# First let's just test a method of extracting the nearest feature and see if
# it looks like it's working.
example.point <- hudson.pvp[hudson.pvp$within.wetland == 0, ][1, ]
example.point.nw <- wetlands[
  sf::st_nearest_feature(
    hudson.pvp[hudson.pvp$within.wetland == 0, ][1, ],
    wetlands), ]
terra::plot(
  sf::st_geometry(ma.towns[ma.towns$TOWN %in% c("HUDSON"), ]["TOWN"]),
  col = "white")
terra::plot(
  sf::st_geometry(example.point.nw),
  add = TRUE,
  col = "blue")
terra::plot(
  sf::st_geometry(example.point),
  add = TRUE,
  col = "green",
  pch = 17,
  cex = .5)

# Cool, looks reasonable.  So, now I need to calculate the distance from the
# unverified pool to the nearest feature. Unfortunately, I think it seems to be
# simpler to do this with a for loop because apply doesn't play nicely with the
# fact that the geometry column is a list.  There's probably a clever way to fix
# that, but I think this works.
NearestFeatureDistance <- function(x, y) {
  nw <- wetlands[sf::st_nearest_feature(y, wetlands), ]
  ifelse(
    x == 1,
    z <- NA,
    z <- sf::st_distance(y, nw))
  return(z)
}
dist <- list()
for (i in seq_along(hudson.pvp[[1]])) {
  dist[i] <- NearestFeatureDistance(
    hudson.pvp[i, ]$within.wetland,
    hudson.pvp[i, ]$geometry)
}
dist
hudson.pvp$dtn <- unlist(dist)
hudson.pvp

# Cool, looks like that worked.  If a pool is within 100 feet of a wetland, then
# it can be protected.  The pool points are just points.  Actual boundary of the
# pool water will ebb and flow and is hard to track for obvious reasons (it's a
# vernal pool, duh).  So, if we say that any pool point within 100 feet of a
# wetland shape is potentially high value, that should be a conservative
# estimate.  So, any pool point that is within a wetland or within 100 feet of a
# wetland is deemed high value.  Cool.  Now we can apply this to all of the
# potential vernal pools.


potential.pools$within.wetland <- lengths(
  sf::st_intersects(potential.pools, wetlands))
dist <- list()
for (i in seq_along(potential.pools[[1]])) {
  dist[i] <- NearestFeatureDistance(
    potential.pools[i, ]$within.wetland,
    potential.pools[i, ]$geometry)
}
