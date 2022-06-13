library(pct)
library(stplanr)
library(sf)
library(tidyverse)
library(osmdata)
library(odjitter)
library(osrm)
library(stringr)
library(tmap)
library(leaflet)
library(htmlwidgets)
library(mapview)

##define coordinate systems
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"

#load("out/all_routes.RData")
#load("../bath_anpr/all_dat.RData")

load("out/pct_od.RData")
load("out/jittered_routes.RData")
load("out/validation_dat.RData")

## pick out example zone
zone_A <- filter(zones_in, geo_code == "E01014479")
## extract centre point
zone_A_cent <- st_centroid(zone_A)
## extracts roads
zone_rds_A <- st_intersection(rdz, zone_A)
## pick out example second zone
zone_B <- filter(zones_in, geo_code == "E01014482")
## extrract centre point
zone_B_cent <- st_centroid(zone_B)
## roads for zone B
zone_rds_B <- st_intersection(rdz, zone_B) %>% 
  select(osm_id)

## show buildings as example of where people live, but also as possiblity for jittering
buildings_in <- buildings[zone_A,]
plot(buildings_in["osm_id"])
## define lat lon of zone centre
lon <- st_coordinates(zone_A_cent)[,1]
lat <- st_coordinates(zone_A_cent)[,2]

## specify a palette
pal_B <- colorFactor("viridis", domain = buildings_in$building, reverse = FALSE)

m <- leaflet() %>% 
  setView(lon, lat, zoom = 16) %>% 
  addProviderTiles('CartoDB.Positron')

m <- m %>% addPolygons(data = buildings_in, color = 'black', weight = 1, fillColor = ~pal_B(buildings_in$building),
                       opacity = 1.0, fillOpacity = 1,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "buildings")

m <- m %>% addPolygons(data = zone_A, color = 'black', weight = 1, fillColor = "grey",
                       opacity = 1.0, fillOpacity = 0.2,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "zone")

m <- m %>% addCircleMarkers(data = zone_A_cent, color = "red", weight = 2, group = "zone_centre")

m <- m %>% addPolylines(data = zone_rds_A, color = "purple", weight = 2, group = "roads")

m <- m %>% addLegend("bottomleft", pal=pal_B, values=buildings_in$building, opacity=1, title = "Type",
                     group = "Buildings")

m <- m %>% addLayersControl(overlayGroups = c("buildings", "roads", "zone_centre", "zone"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright")


m

## save as html
withr::with_dir('./', saveWidget(m, file = "Buildings.html"))
#mapshot(m, file = "buildings.png")

## get polygon for bath, reduce by 200m so it only gets zones within boundary
bath <- osmdata::getbb("Bath", format_out = "sf_polygon") %>% 
  st_transform(ukgrid) %>% 
  st_buffer(-200) %>% 
  st_transform(latlong)

## intersect zones
bath_zones <- zones_in[bath,]
## filter LSOA data for zones
LSOA_small <- filter(LSOA_in, geo_code1 %in% c("E01014479", "E01014482") & geo_code2 == c("E01014479", "E01014482"))
## filter zones inside
zones_small <- filter(zones_in, geo_code == "E01014482" | geo_code == "E01014479")
## define centroid of zones 
zones_cent <- st_centroid(zones_small)
## desire line between two example zones
desire_lines_small = stplanr::od2line(flow = LSOA_small,zones = zones_small)
## use osrm to estimate route between zone centres
route_small <- osrmRoute(src = zones_cent[1,], dst = zones_cent[2,],
                   overview = "full", returnclass = "sf")
## define lat lon of bath centre
lon <- st_coordinates(st_centroid(bath[1,]))[1]
lat <- st_coordinates(st_centroid(bath[1,]))[2]

m <- leaflet() %>% 
  setView(lon, lat, zoom = 14) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE))

m <- m %>% addPolygons(data = zones_small, color = 'green', weight = 2, fillColor = "green",
                       opacity = 1.0, fillOpacity = 0.3,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "zones small")

m <- m %>% addPolygons(data = bath_zones, color = 'black', weight = 1, fillColor = "grey",
                       opacity = 1.0, fillOpacity = 0.2,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "bath zones")

m <- m %>% addCircleMarkers(data = zones_cent, color = "red", weight = 2, group = "zone_centre")

m <- m %>% addPolylines(data = desire_lines_small, color = "purple", weight = 2, group = "desire line")

m <- m %>% addPolylines(data = route_small, color = "orange", weight = 2, group = "route centre")

m <- m %>% addLayersControl(overlayGroups = c("zones small", "bath zones", "zone_centre", "desire line", "route centre"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright")


m
 
bath_cent <- st_centroid(bath[1,])

ur <- unique(routes_all$ID)



site_coords <- st_coordinates(all_sites)

bb <- st_as_sfc(st_bbox(c(xmin = min(site_coords[,1]), xmax = max(site_coords[,1]), ymax = max(site_coords[,2]), ymin = min(site_coords[,2])), crs = st_crs(4326)))

bb_buff <- bb %>% 
  st_transform(ukgrid) %>% 
  st_buffer(1000) %>% 
  st_transform(latlong)

## snap the atcs locations to the roads
rdz_noresi <- rdz[bb_buff,] %>% 
  select(osm_id, geometry, highway) %>% 
  filter(highway %in% c("tertiary", "primary", "trunk")) %>% 
  st_transform(ukgrid) %>% 
  st_line_sample(n = 20) %>% 
  st_cast("POINT") %>% 
st_transform(latlong) %>% 
  st_as_sf()

rdz_resi <- rdz[bb_buff,] %>% 
  select(osm_id, geometry, highway) %>% 
  filter(highway %in% c("residential")) %>% 
  st_transform(ukgrid) %>% 
  st_line_sample(n = 20) %>%  
  st_cast("POINT") %>% 
  st_transform(latlong) %>% 
  st_as_sf()

sites_2 <- filter(all_sites, site_ID %in% c("20170104", "20170106"))
sites_7 <- filter(all_sites, !site_ID %in% c("20170104", "20170106"))

atcs1 <- rdz_noresi[st_nearest_feature(sites_7, rdz_noresi),] %>% 
  mutate(site_ID = sites_7$site_ID)
atcs2 <- rdz_resi[st_nearest_feature(sites_2, rdz_resi),] %>% 
  mutate(site_ID = sites_2$site_ID)

atc_snap <- rbind(atcs1, atcs2)

routes_trim <- st_intersection(routes_all, bb_buff)

overline_routes <- list()
overline_trimmed <- list()
atc_vars <- list()
rt <- ur[2]
for (rt in ur){

  dr <- filter(routes_all, ID == rt)
  
  r <- overline(sl = dr, attrib = "car_driver", ncores = 4) %>% 
    mutate(ID = rt)
  
  overline_routes[[rt]] <- r
  
  r_trim <- st_intersection(r, bb_buff)
  
  overline_trimmed[[rt]] <- r_trim
  
  atcs <- r_trim[st_nearest_feature(atc_snap, r_trim),] %>% 
    mutate(site_ID = str_sub(atc_snap$site_ID, 7,-1)) %>% 
    st_set_geometry(NULL)

  atc_vars[[rt]] <- atcs
  
  }

ol_routes <- do.call(rbind, overline_routes)
ol_trimmed <- do.call(rbind, overline_trimmed)
all_atcs <- do.call(rbind, atc_vars)

me_pal <- c("#0000b3", "#0000eb", "#1d00ff", "#4a00ff", "#7600ff", "#a211ee", "#cf2ed1", "#fb4ab5", 
            "#ff6798", "#ff837c", "#ff9f60", "#ffbc43", "#ffd827", "#fff50a")

max_cars <- max(ol_routes$car_driver)
max_cars_jittered <- max(jitters_all$car_driver)

bks_hr <- seq(from = 0, to = max(ol_routes$car_driver), by = max_cars/NROW(me_pal))
bks_od <- seq(from = 0, to = max(jitters_all$car_driver), by = max_cars_jittered/NROW(me_pal))



# tmaptools::palette_explorer()
tm0 <- tm_shape(jitters_all) +
  tm_lines(palette = me_pal, breaks = bks_od,
           lwd = 0.1,
           scale = 20,
           title.lwd = "Number of vehicles",
           alpha = 0.6,
           col = "car_driver",
           title = "Flow (number of vehicles)"
  ) +
  tm_facets(along = "ID", free.coords = FALSE)+
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

tmap_animation(tm0, filename = "out/plots/OD_raw.gif", delay = 100)

# tmaptools::palette_explorer()
tm1 <- tm_shape(ol_routes) +
  tm_lines(palette = me_pal, breaks = bks_hr,
           lwd = "car_driver",
           scale = 20,
           title.lwd = "Number of vehicles",
           alpha = 0.6,
           col = "car_driver",
           title = "Flow (number of vehicles)"
  ) +
  tm_facets(along = "ID", free.coords = FALSE)+
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

tmap_animation(tm1, filename = "out/plots/OD_jitter.gif", delay = 100)

tm2 <- tm_shape(ol_trimmed) +
  tm_lines(palette = me_pal, breaks = bks_hr,
           lwd = "car_driver",
           scale = 20,
           title.lwd = "Number of vehicles",
           alpha = 0.6,
           col = "car_driver",
           title = "Flow (number of vehicles)"
  ) +
  tm_facets(along = "ID", free.coords = FALSE)+
  tm_layout(frame = FALSE, legend.outside = TRUE)+
  tm_scale_bar(position = c("left", "bottom"))

tmap_animation(tm2, filename = "out/plots/OD_jitter_trimmed.gif", delay = 100)


##define approx point interval in metres
approx_interval <- 5
v <- "E01"
vz <- unique(zone_rds_B$osm_id)

vgt_split <- list()
for (v in vz){
  tryCatch({
    df <- zone_rds_B %>% filter(osm_id == v) %>% st_transform(ukgrid) %>% mutate(lngths = as.numeric(st_length(geometry)))
    
    n_pts <- df$lngths/approx_interval
    df1 <- st_line_sample(df, sample = 0)
    df2 <- st_line_sample(df, n = n_pts)
    df3 <- st_line_sample(df, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df <- rbind(df1, df2, df3)
    df <- transmute(df, Source.name = v, X, Y)
    vgt_split[[v]] <- df
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

vgt <- do.call(rbind, vgt_split)

B_pts <- st_as_sf(vgt, coords = c("X", "Y"), crs = ukgrid)
B_pts$ID <- rownames(B_pts)
B_pts$zone <- "zone_B"

vz <- unique(zone_rds_A$osm_id)

vgt_split <- list()
for (v in vz){
  tryCatch({
    df <- zone_rds_A %>% filter(osm_id == v) %>% st_transform(ukgrid) %>% mutate(lngths = as.numeric(st_length(geometry)))
    
    n_pts <- df$lngths/approx_interval
    df1 <- st_line_sample(df, sample = 0)
    df2 <- st_line_sample(df, n = n_pts)
    df3 <- st_line_sample(df, sample = 1)
    df1 <- data.frame(st_coordinates(df1))
    df2 <- data.frame(st_coordinates(df2))
    df3 <- data.frame(st_coordinates(df3))
    df <- rbind(df1, df2, df3)
    df <- transmute(df, Source.name = v, X, Y)
    vgt_split[[v]] <- df
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

vgt <- do.call(rbind, vgt_split)

A_pts <- st_as_sf(vgt, coords = c("X", "Y"), crs = ukgrid)
A_pts$ID <- rownames(A_pts)
A_pts$zone <- "zone_A"

pts <- rbind(A_pts, B_pts)

## examples of opposite jounneys

mapview(A_pts)+B_pts

route_pairs <- data.frame(start = c("121120357.6",
                                    "671912551.2",
                                    "671912551.2",
                                    "510800084.56"),
                          end = c("510800084.56", 
                                  "89126716.40", 
                                  "121120357.6", 
                                  "89126716.40"))

routez <- list()
starts <- list()
ends <- list()
for (r in seq(1:NROW(route_pairs))){
  
  rp <- route_pairs[r,]
  
  sp <- pts %>% filter(ID == rp$start) %>% st_transform(latlong)
  ep <- pts %>% filter(ID == rp$end) %>% st_transform(latlong)
  
  route <- osrmRoute(src = sp, dst = ep,
                     overview = "full", returnclass = "sf")
  
  nam <- as.character(r)
  routez[[nam]] <- route
  starts[[nam]] <- sp
  ends[[nam]] <- ep
  
}

all_routes <- do.call(rbind, routez)
all_starts <- do.call(rbind, starts)
all_ends <- do.call(rbind, ends)

m <- leaflet() %>% 
  setView(lon, lat, zoom = 14) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE))

m <- m %>% addPolygons(data = zones_small, color = 'green', weight = 2, fillColor = "green",
                       opacity = 1.0, fillOpacity = 0.3,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "zones small")

m <- m %>% addPolygons(data = bath_zones, color = 'black', weight = 1, fillColor = "grey",
                       opacity = 1.0, fillOpacity = 0.2,
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = FALSE), group = "bath zones")

m <- m %>% addCircleMarkers(data = zones_cent, color = "red", weight = 2, group = "route centre")

m <- m %>% addPolylines(data = route_small, color = "purple", weight = 2, group = "route centre")

m <- m %>% addPolylines(data = all_routes[1,], color = "purple", weight = 2, group = "route 1")

m <- m %>% addCircleMarkers(data = all_starts[1,], color = "green", weight = 2, group = "route 1")

m <- m %>% addCircleMarkers(data = all_ends[1,], color = "red", weight = 2, group = "route 1")

m <- m %>% addPolylines(data = all_routes[2,], color = "purple", weight = 2, group = "route 2")

m <- m %>% addCircleMarkers(data = all_starts[2,], color = "green", weight = 2, group = "route 2")

m <- m %>% addCircleMarkers(data = all_ends[2,], color = "red", weight = 2, group = "route 2")

m <- m %>% addPolylines(data = all_routes[3,], color = "purple", weight = 2, group = "route 3")

m <- m %>% addCircleMarkers(data = all_starts[3,], color = "green", weight = 2, group = "route 3")

m <- m %>% addCircleMarkers(data = all_ends[3,], color = "red", weight = 2, group = "route 3")

m <- m %>% addPolylines(data = all_routes[4,], color = "purple", weight = 2, group = "route 4")

m <- m %>% addCircleMarkers(data = all_starts[4,], color = "green", weight = 2, group = "route 4")

m <- m %>% addCircleMarkers(data = all_ends[4,], color = "red", weight = 2, group = "route 4")

m <- m %>% addLayersControl(overlayGroups = c("zones small", "bath zones"),
                            baseGroups = c("route 1", "route 2", "route 3", "route 4", "route centre"),
                            options = layersControlOptions(collapsed = FALSE), position = "topright")


m

withr::with_dir('./', saveWidget(m, file = "route_options.html"))

library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
g1 <- ggplot(all_atcs, aes(x=site_ID, y=car_driver, fill=site_ID)) + 
  geom_bar(stat='identity') +
  theme_bw()+
  # gganimate specific bits:
  transition_states(
    ID,
    transition_length = 1,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:
animate(g1, fps = 4)
anim_save("out/plots/jitter_atc.gif")

gg_pal <- c("#f7766c", "#d29200", "#92aa00", "#00ba37", "#00c19e", 
            "#03b8e3", "#619cff", "#da72fb", "#fe61c2")

tm3 <- tm_shape(ol_trimmed) +
  tm_lines(palette = me_pal, breaks = bks_hr,
           lwd = "car_driver",
           scale = 20,
           title.lwd = "Number of vehicles",
           alpha = 0.6,
           col = "car_driver",
           title = "Flow (number of vehicles)"
  ) +
  tm_facets(along = "ID", free.coords = FALSE)+
  tm_layout(frame = FALSE, legend.outside = TRUE)+  
  tm_scale_bar(position = c("left", "bottom"))+
  tm_shape(atc_snap)+
  tm_dots(size = 1, col = "site_ID", palette = gg_pal)

tmap_animation(tm3, filename = "out/plots/OD_jitter_trimmed_pts.gif", delay = 100)
