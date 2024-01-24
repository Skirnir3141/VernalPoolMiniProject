rm(list = ls())
setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

library(dplyr)
library(terra)
library(sf)
library(units)
library(scales)

setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

# Import vectors MA towns, certified pools, potential pools, and wetlands.
ma.towns <- sf::st_read("./townssurvey_shp/TOWNSSURVEY_POLY.shp")
certified.pools <- sf::st_read("./GISDATA_CVP_PT/GISDATA_CVP_PTPoint.shp")
potential.pools <- sf::st_read("./pvp/PVP_PT.shp")
wetlands <- sf::st_read("./wetlandsdep/WETLANDSDEP_POLY.shp")

# Create vectors for just the towns of interest.
cm.towns <- ma.towns[
  ma.towns$TOWN %in% c(
    "HUDSON"), ]
cm.cp <- certified.pools[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(sf::st_zm(certified.pools))
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

# Import land use tiles that intersect with towns. To do this, import tile
# indexes, convert to MA projection, get IDs of tiles overlapping with towns,
# create a list to hold tiles, import tiles by name, bind tiles together, and
# convert to MA projection.
tile.index <- sf::st_read(
  "./landcover_use_index_poly/LANDCOVER_USE_INDEX_POLY.shp")
tile.index <- sf::st_transform(tile.index, 26986)
needed.tiles <- tile.index[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(tile.index)
    ))), ]$TILENAME
tiles.l <- vector("list", length = length(needed.tiles))
for (i in 1:length(needed.tiles)) {
  shp <- sf::st_read(
    paste("./tiles/LCLU_",
          needed.tiles[i],
          "/LCLU_",
          needed.tiles[i],
          ".shp",
          sep = ""))
  tiles.l[[i]] <- shp[c("COVERCODE", "USEGENCODE")]
}
cm.tiles <- do.call(rbind, tiles.l)
cm.tiles <- sf::st_transform(cm.tiles, 26986)

# Filter tiles to include only polygons intersecting withtowns.
cm.tiles <- cm.tiles[unlist(
  unique(
    sf::st_intersects(
      x = sf::st_geometry(cm.towns),
      y = sf::st_geometry(cm.tiles)
    ))), ]

# Create an object including only polygons describing terrain that could
# indicate a threat to pools.  These are: COVERCODE 2 = Impervious, USEGENCODE 4
# = Industrial, USEGENCODE 7 = Agriculture, USEGENCODE 8 = Recreation,
# USEGENCODE 10 = Mixed use, primarily residential, USEGENCODE 11 = Residential
# - single family, USEGENCODE 12 = Residential - multi-family, USEGENCODE 20 =
# Mixed use, other, USEGENCODE 30 = Mixed use, primarily commercial, USEGENCODE
# 55 = Right-of-way.
cm.tiles.f <- cm.tiles[
  cm.tiles$COVERCODE %in% c(2) |
    cm.tiles$USEGENCODE %in% c(4, 7, 8, 10, 11, 12, 20, 30, 55), ]

# Check whether a potential pool overlaps with a wetland. If it does, set its
# distance to the nearest wetland equal to zero. If it doesn't, calculate its
# distance to the nearest wetland in meters
cm.pp$within.wetland <- lengths(
  sf::st_intersects(cm.pp, cm.wetlands))
cm.pp.ww <- cm.pp[cm.pp$within.wetland == 1, ]
cm.pp.ww$dtw <- 0
cm.pp.nww <- cm.pp[cm.pp$within.wetland == 0, ]
dist <- vector("list", length = nrow(cm.pp.nww))
for (i in 1:nrow(cm.pp.nww)) {
  dist[i] <- units::set_units(
    min(
      sf::st_distance(
        sf::st_geometry(cm.pp.nww)[i],
        sf::st_geometry(cm.wetlands))),
    'ft')
}
cm.pp.nww$dtw <- dist
cm.pp <- rbind(cm.pp.nww, cm.pp.ww)

# Check if pools overlap with an impervious feature. If they do, set their
# distance to the nearest impervious feature equal to zero. If they don't,
# calculate their distance to the nearest impervious feature.
cm.pp$within.feature <- lengths(
  sf::st_intersects(
    sf::st_geometry(cm.pp),
    sf::st_geometry(cm.tiles.f)))
cm.pp.wf <- cm.pp[cm.pp$within.feature == 1, ]
cm.pp.wf$dtf <- 0
cm.pp.nwf <- cm.pp[cm.pp$within.feature == 0, ]
dist2 <- vector("list", length = nrow(cm.pp.nwf))
for (i in 1:nrow(cm.pp.nwf)) {
  dist2[i] <- units::set_units(
    min(
      sf::st_distance(
        sf::st_geometry(cm.pp.nwf)[i],
        sf::st_geometry(cm.tiles.f))),
    'ft')
}
cm.pp.nwf$dtf <- dist2
cm.pp <- rbind(cm.pp.nwf, cm.pp.wf)

# For each potential pool, calculate the distance to the nearest certified pool.
dist3 <- vector("list", length = nrow(cm.pp))
for (i in 1:nrow(cm.pp)) {
  dist3[i] <- units::set_units(
    min(
      sf::st_distance(
        sf::st_geometry(cm.pp)[i],
        sf::st_zm(sf::st_geometry(cm.cp)))),
    'ft')
}
cm.pp$dtcp <- dist3

# Create an object that drops potential poos that are more than 150 feet from a
# wetland. This accounts for the 100 foot rule and +/- 15 foot imprecision in
# point generation.
# TODO: figure out precision of wetland objects and adjust
cm.pp.f <- cm.pp[cm.pp$dtw <= 150, ]

# Create a distance matrix for filtered potential pools and compute a
# hierarchical cluster analysis on it using an 
dist.matrix <- sf::st_distance(cm.pp.f, cm.pp.f)
cm.pp.f.clust <- hclust(as.dist(dist.matrix), method = "average")
fviz_dend(cm.pp.f.clust)


wss <- function(d) {
  sum(scale(d, scale = FALSE)^2) 
}
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i) #cuts the cluster cluster based a number specified by i
  spl <- split(x, cl) # splits the dataset into the number of groups according to cl 
  wss <- sum(sapply(spl, wss)) # calculates sum of squares for each cluster/group 
  wss # extracts the within group sum of squares 
}
res <- sapply(
  seq.int(1, nrow(dist.matrix)),
  wrap,
  hc = cm.pp.f.clust,
  x = dist.matrix) # calculates the within group/cluster sum of squares starting at 1 to number of rows in the dataframe, and uses the prespecified functions wrap. 
plot(seq_along(res), res, type = "b", pch = 19, xlab="Number of Clusters", ylab="Within-Cluster Sum of Squares", xlim=c(0, 30))
