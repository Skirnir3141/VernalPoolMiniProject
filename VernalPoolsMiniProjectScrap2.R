rm(list = ls())
setwd("C:/Users/Michael Jordan/Desktop/VernalPoolMiniProject")

library(dplyr)
library(terra)
library(sf)
library(units)
library(scales)
library(factoextra)

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
    cm.tiles$USEGENCODE %in% c(4, 10, 11, 12, 30, 55), ]


# This code agglomerates sets of overlapping features. For potential pools
# within an impervious feature, I want to understand the distance to the edge of
# the overall impervious matrix, not just the specific feature the pool may be
# inside (imagine the pool being inside a small building on top of a parking lot
# ...I want the distance to the parking lot edge, not the building edge).
#
# Get indexes of every feature with each feature intersects.
test <- st_intersects(cm.tiles.f, cm.tiles.f)

# Create an object to hold the intersections for every feature
res <- vector("list", length = length(test))

# This creates groups of overlapping features. Imagine the case where many
# features overlap, but no feature in the group overlaps with every other
# feature in the group. In this case, intersecting produces various subsets of
# the group. This combines those subsets.
for (i in 1:length(test)) {
  x <- test[[i]] # Get a set of intersecting features
  for (j in 1:length(test[-i])) { # Now look at all other sets 
    if(any(x %in% test[-i][[j]])) { # For all other sets, check if they overlap
      x <- sort(unique(c(x, test[-i][[j]]))) # If so, add them to the set
    }  
  }
  res[[i]] <- x # Save results
}

# Remove duplicates
res <- res[!duplicated(res)]

# Union each distinct group into one polygon geometry set, combine, and convert
# back to an sfc.
res2 <- vector("list", length = length(res))
for (i in 1:length(res2)) {
  res2[[i]] <- st_union(s[res[[i]], ])
}

s2 <- st_sf(data.frame(geometry = do.call(rbind, res2)))



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

# Clustering evaluation functions
wss <- function(d) {
  sum(scale(d, scale = FALSE)^2) 
}
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss)) 
  wss
}

# One way of doing clustering
library(cluster)

test <-  data.frame(st_coordinates(st_cast(cm.pp.f$geometry,"MULTIPOINT")))[, c("X", "Y")]
dist.matrix <- daisy(test, stand = TRUE, metric = "euclidean")
cm.pp.f.clust <- hclust(dist.matrix, method = "average")
fviz_dend(cm.pp.f.clust)
res <- sapply(
  seq.int(1, nrow(dist.matrix)),
  wrap,
  hc = cm.pp.f.clust,
  x = dist.matrix) 
plot(
  seq_along(res),
  res,
  type = "b",
  pch = 19,
  xlab="Number of Clusters",
  ylab="Within-Cluster Sum of Squares",
  xlim=c(0, 41))

# Another way of doing clustering
dist.matrix <- sf::st_distance(cm.pp.f, cm.pp.f)
cm.pp.f.clust <- hclust(as.dist(dist.matrix), method = "single")
fviz_dend(cm.pp.f.clust)
# This is in meters, but 402 is about 1/4 of a mile
cm.pp.f$clust <- cutree(cm.pp.f.clust, h = 402)



cluster.prios <- st_drop_geometry(cm.pp.f) %>%
  group_by(clust) %>%
  summarise(
    n = n(),
    avg.dtf = round((mean(unlist(dtf))) / 10)) %>%
  mutate(
    rank.n = dense_rank(desc(n)),
    rank.dtf = dense_rank(avg.dtf),
    prio = dense_rank(dense_rank(desc(n)) + dense_rank(avg.dtf)) / 2)
cluster.prios

cm.pp.f <- left_join(cm.pp.f, cluster.prios[c("clust", "prio")], by = "clust")


test <- sf::st_read(
  paste("./tiles/LCLU_",
        needed.tiles[1],
        "/LCLU_",
        needed.tiles[1],
        ".shp",
        sep = ""))

test.l <- vector("list", length = length(needed.tiles))
for (i in 1:length(needed.tiles)) {
  shp <- sf::st_read(
    paste("./tiles/LCLU_",
          needed.tiles[i],
          "/LCLU_",
          needed.tiles[i],
          ".shp",
          sep = ""))
  test.l[[i]] <- shp[c("COVERCODE", "COVERNAME", "USEGENCODE", "USEGENNAME", "SHAPE_AREA")]
}
test.tiles <- do.call(rbind, test.l)

dev.off()
terra::plot(
  sf::st_geometry(cm.towns),
  col = "white",
  main = "Potential Pool Heatmap")
terra::plot(
  sf::st_geometry(cm.pp),
  add = TRUE,
  col = "lightblue")
terra::plot(
  sf::st_geometry(cm.cp),
  add = TRUE,
  col = "black")

dev.off()
terra::plot(
  cm.pp.f["prio"],
  main = "test",
  legend = FALSE,
  breaks = 1:5,
  pal = rev(heat.colors(4)),
  pch = 20,
  cex = 2)
legend(
  "bottom",
  legend = c(1, 2, 3, 4, 5),
  fill = rev(heat.colors(5)),
  bty = "n")

dev.off()
# Set graphical parameters
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))

cm.towns <- cbind(cm.towns, st_coordinates(st_centroid(cm.towns)))
# Add town centroid for label plotting. Plot potential pools versus wetlands in
# all towns.

test1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = cm.towns, fill = "white") +
  ggplot2::geom_sf(data = cm.wetlands, fill = "blue") +
  ggplot2::geom_sf(data = cm.pp.f, aes(color = prio), pch = 20, cex = 5) +
  ggplot2::scale_color_gradient(
    low="darkred", high="yellow", trans = "reverse", name = "Pool\nPrio") + 
  ggplot2::geom_label(
    data = cm.towns, aes(X, Y, label = TOWN), size = 4, fontface = "bold") +
  ggplot2::theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    legend.margin = margin( 0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0))

# Plot potential pools versus wetlands, impervious features, and pools removed
# from analysis for a zoomed in area of Hudson.
ext <- c(xmin = 196687.2, ymin = 903000, xmax = 202250.2, ymax = 906000)
cm.pp.del <- cm.pp[cm.pp$dtw > 150, ]
cm.pp.del$del <- ""
ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf::st_crop(cm.towns, ext), fill = "white") +
  ggplot2::geom_sf(data = sf::st_crop(cm.wetlands, ext), fill = "blue") +
  ggplot2::geom_sf(data = sf::st_crop(cm.tiles.f, ext), fill = "grey") +
  ggplot2::geom_sf(
    data = sf::st_crop(cm.pp.f, ext), aes(color = prio), pch = 20, cex = 5) +
  ggplot2::scale_color_gradient(
    low="darkred", high="yellow", trans = "reverse", guide = "none") + 
  ggplot2::geom_sf(
    data = sf::st_crop(cm.pp.del, ext), aes(fill = del), pch = 13, cex = 5) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Excluded\nPool")) +
  ggplot2::theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    legend.margin = margin( 0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0))
cowplot::plot_grid(test1, test2, labels = NULL)


```{r results_table, echo=FALSE, fig.align='center', error=FALSE, warning=FALSE, message=FALSE}

table.output <- dplyr::arrange(
  cluster.prios[c("town", "prio", "n", "avg.dtf", "centroid")],
  prio)[1:5, ]
colnames(table.output) <- c(
  "Town", "Priority", "No. of Pools", "Avg Distance To Impervious Feature (10m)",
  "Cluster GIS Coords")
knitr::kable(
  table.output,
  align = "c",
  caption = "Cluster Geo Coords & Ranking")
```


ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf::st_crop(cm.towns, ext2), fill = "white") +
  #ggplot2::geom_sf(data = sf::st_crop(cm.wetlands, ext2), fill = "lightblue") +
  ##ggplot2::geom_sf(data = sf::st_crop(cm.tiles.f, ext2), fill = "lightgrey") +
  ggplot2::geom_sf(
    data = sf::st_crop(cm.pp.f, ext2), aes(color = prio), pch = 20, cex = 5) +
  ggplot2::scale_color_gradient(
    low="darkred", high="yellow", trans = "reverse", guide = "none") + 
  ggplot2::geom_sf(
    data = sf::st_crop(cm.ep, ext2),
    aes(shape = factor(type)), size = 2) +
  scale_shape_manual(values = c(17, 13)) +
  ggplot2::theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    legend.margin = margin( 0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_blank(),
    legend.position = "top")
