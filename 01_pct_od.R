library(pct)
library(stplanr)
library(sf)
library(tidyverse)
library(osmdata)
library(osrm)

##define coordinate systems
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"

##define area of interest
x_min <- -2.5494499206
x_max <- -2.303910017
y_min <- 51.320346
y_max <- 51.42509

##create a data frame to setup polygon generation
df <- data.frame(X = c(x_min, x_max, x_max, x_min), 
                 Y = c(y_max, y_max, y_min, y_min))

##generate a polygon of the area
vgt_area <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = latlong) %>%
  dplyr::summarise(data = st_combine(geometry)) %>%
  st_cast("POLYGON") 

## get census data zones from the pct
pct_regions <- pct::pct_regions

## trim to area of interest
pct_zones <- pct_regions[vgt_area,]

## loop through each zone and get OD data
all_zones <- list()
all_LSOA <- list()

for (p in pct_zones$region_name){

zones_all <- get_pct_zones(region = p)

LSOA <- read.csv(paste0("https://npttile.vs.mythic-beasts.com/npct-data/pct-outputs-regional-notR/commute/lsoa/", p, "/od_attributes.csv"))

zones_clean <- zones_all[st_is_valid(zones_all),] %>% 
  filter(geo_code %in% LSOA$geo_code1)

LSOA <- LSOA %>% filter(geo_code1 %in% zones_clean$geo_code & geo_code2 %in% zones_clean$geo_code) %>% 
  select(geo_code1, geo_code2, all, bicycle, foot, car_driver, car_passenger, motorbike, train_tube, bus, taxi_other)

all_zones[[p]] <- zones_clean
all_LSOA[[p]] <- LSOA

}

## combine all zone geo and data
LSOA_all <- do.call(rbind, all_LSOA)
zones_all <- do.call(rbind, all_zones)

## trim zones to area of interest
zones_in <- zones_all[vgt_area,]

## get coordinates of zone area 
zone_coords <- st_coordinates(zones_in)

##create min x and y of camera area
x_min <- min(zone_coords[,1])
x_max <- max(zone_coords[,1])
y_min <- min(zone_coords[,2])
y_max <- max(zone_coords[,2])

##create a data frame to setup polygon generation
df <- data.frame(X = c(x_min, x_max, x_max, x_min), 
                 Y = c(y_max, y_max, y_min, y_min))

##generate a polygon of the area
zone_area <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = latlong) %>%
  dplyr::summarise(data = st_combine(geometry)) %>%
  st_cast("POLYGON") 

##get osm road geometry for camera area
x <- opq(bbox = zone_area) %>% 
  add_osm_feature(key = c('highway')) %>% osmdata_sf()

## extract line geometry for the roads
rdz <- x$osm_lines

##download building data from OSM
x <- opq(bbox = zone_area) %>%
  add_osm_feature(key = c('building')) %>%
  osmdata_sf()

##extract building polygons
buildings <- x$osm_polygons %>% 
  select(osm_id, building, geometry)

## filter LSOA data
LSOA_in <- LSOA_all %>% filter(geo_code1 %in% zones_in$geo_code & geo_code2 %in% zones_in$geo_code) %>% 
  mutate(car_driver = as.numeric(car_driver))

## generate desire lines for OD data
desire_lines = stplanr::od2line(flow = LSOA_in,zones = zones_in)

desire_lines_car <- select(desire_lines, car_driver)
  
  #od_jittered <- distinct(od_jittered, geo_code1, geo_code2, .keep_all = TRUE)
  
  desire_lines_car$n_line <- sprintf("%04d", seq(1:NROW(desire_lines_car)))
  desire_lines_car <- filter(desire_lines_car, car_driver > 0)
  
  routes <- list()
  for(o in desire_lines_car$n_line){
    
    l <- desire_lines_car %>% filter(n_line == o) %>% st_transform(ukgrid)
    
    sp <- l %>% st_line_sample(sample = 0) %>% st_transform(latlong) %>% st_as_sf()
    ep <- l %>% st_line_sample(sample = 1) %>% st_transform(latlong) %>% st_as_sf()
    
    if(NROW(sp)>0 & NROW(ep)>0){
      
      
      success <- FALSE
      while(!success){
        tryCatch({
          route <- osrmRoute(src = sp, dst = ep,
                             overview = "full", returnclass = "sf", )
          
          success <- NROW(route) > 0
          
          nam <- paste0(o)
          #show progress
          print(nam)
          # update GUI console
          flush.console()
        },error=function(e){
          Sys.sleep(5)
        },finally={})
        
      }
      
      st_geometry(l) <- NULL
      
      route <- cbind(route, l)
      
      routes[[nam]] <- route
      
    } else {
      nam <- paste0(r, "_", o)
      if(NROW(sp)<1){
        
        missing_sp[[nam]] <- r_df$o
        print(r)
      } else {
        
        missing_ep[[nam]] <- r_df$d
        print(r)
      }
    }
    
  }

## combine outputs (takes a while)
routes_all <- do.call(rbind, routes)

r <- overline(sl = routes_all, attrib = "car_driver", ncores = 4)

## save outputs
save(zones_in, rdz, r, vgt_area, buildings, desire_lines, LSOA_in, file = "out/pct_od.RData")
