rm(list = ls())
setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

library(dplyr)
library(tidyverse)
library(terra)
library(sf)

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
  dplyr::group_by(TOWN, POP2000) %>%
  dplyr::summarise(pools = sum(verified.pools), sm = sum(SQUARE_MIL)) %>%
  dplyr::mutate(ppsm = pools / sm, pppd = pools / (POP2000 /  sm))
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

# Looks like ~2/3 of the potential pools are inside a wetland polygon.
potential.pools$within.wetland <- lengths(
  sf::st_intersects(potential.pools, wetlands))
potential.pools.ww <- potential.pools[potential.pools$within.wetland == 1, ]
potential.pools.ww$dtp <- 0
potential.pools.nww <- potential.pools[potential.pools$within.wetland == 0, ]

# Ideally, I'd like to calculate the distance from the pools not in a polygon
# to the nearest  polygon.  Unfortunately, although the code is straightforward,
# the fact that there are so many (200k!) wetland polygons makes this really
# computationally intensive (i.e., slow). I've tried a variety of ways to solve
# this.  For example, excluding certain wetland types, excluding parts of the
# state with few potential vernal pools, and exploring different ways to make my
# loop more efficient.  Unfortunately, all of the common wetland types are
# widely distributed, as are the potential pools, meaning that there isn't an
# obvious block to exclude. Ways of making my loops more efficient didn't work.

# So, let's randomly sample to see what the typical distance from a wetland is
# for pools not in a wetland.  Maybe we can decide to exclude or include all.

ppnw.sample <- potential.pools.nww[sample(nrow(potential.pools.nww), 100), ]
dist <- vector("list", length = nrow(ppnw.sample))
for (i in 1:nrow(ppnw.sample)) {
  dist[i] <- min(
    sf::st_distance(
      sf::st_geometry(ppnw.sample)[i],
      sf::st_geometry(wetlands)))
}
mean(unlist(dist))
median(unlist(dist))
quantile(unlist(dist), probs = c(.1, .25, .5, .75, .9))

# Unfortunately, over half of the pools not near a wetland are within 100 feet.
# Filtering by pool proximity to a wetland is only going to remove about 15% of
# records.  Not a ton.  That doesn't make things tractable. Let's see if we can
# focus on some towns instead.

ma.towns.agg <- sf::st_drop_geometry(
  ma.towns[ma.towns$TOWN %in% ma.towns[ma.towns$potential.pools > 0, ]$TOWN, ]) %>%
  dplyr::group_by(TOWN, POP2000) %>%
  dplyr::summarise(pools = sum(potential.pools), sm = sum(SQUARE_MIL)) %>%
  dplyr::mutate(
    pools,
    perc_pools = pools / sum(ma.towns$potential.pools),
    poppsm = POP2000 / sm,
    ppsm = pools / sm,
    pppd = pools / (POP2000 /  sm))

ma.towns.agg
dplyr::arrange(ma.towns.agg, desc(poppsm))
dplyr::arrange(ma.towns.agg, desc(perc_pools))

# Unfortunately, it doesn't look like there are towns with mega high pool
# distribution.  Maybe it's time to concede defeat and just focus on part of the
# state.

# Screw it, we're just gonna do one county.


county.agg <- sf::st_drop_geometry(ma.towns) %>%
  dplyr::group_by(FIPS_COUNT) %>%
  dplyr::summarise(
    pools = sum(potential.pools),
    sm = sum(SQUARE_MIL),
    pop = sum(POP2000)) %>%
  dplyr::mutate(
    pools,
    perc_pools = pools / sum(ma.towns$potential.pools),
    poppsm = pop / sm,
    ppsm = pools / sm,
    pppd = pools / (pop /  sm))
dplyr::arrange(county.agg, desc(pppd))
dplyr::arrange(county.agg, desc(ppsm))


dist <- vector("list", length = nrow(worcester.pp.nww))
start_time <- Sys.time()
for (i in 1:nrow(worcester.pp.nww)) {
  dist[i] <- min(
    sf::st_distance(
      sf::st_geometry(worcester.pp.nww)[i],
      sf::st_geometry(worcester.wetlands)))
}
end_time <- Sys.time()
end_time - start_time
worcester.pp.nww$dtw <- dist
worcester.pp <- dplyr::arrange(rbind(worcester.pp.nww, worcester.pp.ww), PVP_NUMBER)



# Get town vectors. Get pools and wetlands that intersect with towns.
cm.towns <- ma.towns[
  ma.towns$TOWN %in% c(
    "HUDSON"), ]
cm.vp <- verified.pools[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(sf::st_zm(verified.pools))
    ))), ]
cm.pp <- potential.pools[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(sf::st_zm(potential.pools))
    ))), ]
cm.wetlands <- wetlands[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(wetlands)
    ))), ]

# Get index of tiles that intersect with towns. Convert to MA projection. Load
# tiles, compile into one object, convert to projection.
tile.index <- sf::st_read("./landcover_use_index_poly/LANDCOVER_USE_INDEX_POLY.shp")
tile.index <- sf::st_transform(tile.index, 26986)
needed.tiles <- tile.index[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(tile.index)
    ))), ]
needed.tiles <- needed.tiles[needed.tiles$TILENAME %in% unique(needed.tiles$TILENAME), ]$TILENAME
tiles.l <- vector("list", length = length(needed.tiles))
for (i in 1:length(needed.tiles)) {
  shp <- sf::st_read(
    paste("./tiles/LCLU_", needed.tiles[i], "/LCLU_", needed.tiles[i], ".shp", sep = ""))
  tiles.l[[i]] <- shp[c("COVERCODE", "USEGENCODE")]
}
cm.tiles <- do.call(rbind, tiles.l)
cm.tiles <- sf::st_transform(cm.tiles, 26986)

# Check whether a potential pool overlaps with a wetland. If it does, set its
# distance to wetland equal to zero.
cm.pp$within.wetland <- lengths(
  sf::st_intersects(cm.pp, cm.wetlands))
cm.pp.ww <- cm.pp[cm.pp$within.wetland == 1, ]
cm.pp.nww <- cm.pp[cm.pp$within.wetland == 0, ]
cm.pp.ww$dtw <- 0

# If it doesn't intersect with a wetland, calculate the distance to the nearest
# wetland.
dist <- vector("list", length = nrow(cm.pp.nww))
for (i in 1:nrow(cm.pp.nww)) {
  dist[i] <- min(
    sf::st_distance(
      sf::st_geometry(cm.pp.nww)[i],
      sf::st_geometry(cm.wetlands)))
}
cm.pp.nww$dtw <- dist
cm.pp <- rbind(cm.pp.nww, cm.pp.ww)

# Check if pools overlap with an impervious feature. If they do, set their
# distance to the nearest feature equal to zero.
cm.pp$within.feature <- lengths(
  sf::st_intersects(
    sf::st_geometry(cm.pp),
    sf::st_geometry(
      cm.tiles[
        cm.tiles$COVERCODE %in% c(2) |
          cm.tiles$USEGENCODE %in% c(7, 33, 4, 20, 10, 8, 12, 11, 55), ])))
cm.pp.wf <- cm.pp[cm.pp$within.feature == 1, ]
cm.pp.wf$dtf <- 0
cm.pp.nwf <- cm.pp[cm.pp$within.feature == 0, ]

dist2 <- vector("list", length = nrow(cm.pp.nwf))
for (i in 1:nrow(cm.pp.nwf)) {
  dist2[i] <- min(
    sf::st_distance(
      sf::st_geometry(cm.pp.nwf)[i],
      sf::st_geometry(
        cm.tiles[
          cm.tiles$COVERCODE %in% c(2) |
            cm.tiles$USEGENCODE %in% c(7, 33, 4, 20, 10, 8, 12, 11, 55), ])))
}
cm.pp.nwf$dtf <- dist2
cm.pp <- rbind(cm.pp.nwf, cm.pp.wf)

cm.pp$priority <- apply(
  cm.pp[c("dtw", "dtf")],
  1,
  function(x) ifelse(
    x[1] <= 30 & x[2] <= 150,
    1,
    ifelse(
      x[1] <= 30 & x[2] > 150,
      2,
      3)))
cm.pp$pch <- apply(
  cm.pp["priority"],
  1,
  function(x) ifelse(
    x[1] == 1,
    16,
    ifelse(
      x[1] == 2,
      17,
      15)))
cm.pp$col <- apply(
  cm.pp["priority"],
  1,
  function(x) ifelse(
    x[1] == 1,
    "green",
    ifelse(
      x[1] == 2,
      "brown",
      "red")))

cm.pp

terra::plot(
  sf::st_geometry(cm.towns["TOWN"]),
  col = "white")
terra::plot(
  sf::st_geometry(cm.wetlands),
  add = TRUE,
  col = "blue")
terra::plot(
  sf::st_geometry(sf::st_zm(cm.pp)),
  add = TRUE,
  col = cm.pp$col,
  pch = cm.pp$pch,
  cex = 1)
terra::plot(
  sf::st_geometry(
    cm.tiles[
      cm.tiles$COVERCODE %in% c(2) |
        cm.tiles$USEGENCODE %in% c(7, 33, 4, 20, 10, 8, 12, 11, 55), ]),
  add = TRUE,
  col = "grey")




print(
  sf::st_drop_geometry(R09C11) %>%
    group_by(COVERNAME, COVERCODE, USEGENNAME,  USEGENCODE) %>%
    summarise(n = n()) %>%
    arrange(COVERNAME, USEGENNAME),
  n = 200)

print(
  sf::st_drop_geometry(R09C11) %>%
    group_by(USEGENNAME,  USEGENCODE) %>%
    summarise(n = n()) %>%
    arrange(USEGENNAME),
  n = 200)

# Useful 
potential.pools$dtw <- apply(
  potential.pools["within.wetland"],
  1,
  function(x) if(x[1] == 1) {0})