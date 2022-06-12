## this script jitters the OD matrices. Took about 48 hours on my laptop!

library(odjitter) 
library(sf)
library(tidyverse)
library(osrm)

##define coordinate systems
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"

load("out/pct_od.RData")
load("../bath_anpr/all_dat.RData")
runs <- seq(1:20)

jitters <- list()
routes <- list()
missing_sp <- list()
missing_ep <- list()

for (r in runs){
  
  od_jittered = jitter(
    od = LSOA_in,
    zones = zones_in,
    subpoints = rdz,
    disaggregation_threshold = 5)
  
  jitters[[r]] <- od_jittered
  
  #od_jittered <- distinct(od_jittered, geo_code1, geo_code2, .keep_all = TRUE)
  
  od_jittered$n_line <- sprintf("%04d", seq(1:NROW(od_jittered)))
  
  for(o in od_jittered$n_line){
    
    l <- od_jittered %>% filter(n_line == o) %>% st_transform(ukgrid)
    
    sp <- l %>% st_line_sample(sample = 0) %>% st_transform(latlong) %>% st_as_sf()
    ep <- l %>% st_line_sample(sample = 1) %>% st_transform(latlong) %>% st_as_sf()
    
    if(NROW(sp)>0 & NROW(ep)>0){
      
      
      success <- FALSE
      while(!success){
        tryCatch({
          route <- osrmRoute(src = sp, dst = ep,
                             overview = "full", returnclass = "sf")
          
          success <- NROW(route) > 0
          
          nam <- paste0(r, "_", o)
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
  
}

## combine outputs (takes a while)
routes_all <- do.call(rbind, routes)
jitters_all <- do.call(rbind, jitters)

routes_all$ID <- sprintf("%02d", rep(1:NROW(jitters), each=NROW(od_jittered)))
jitters_all$ID <- sprintf("%02d", rep(1:NROW(jitters), each=NROW(od_jittered)))

## save for 03_outputs.R
save(routes_all, jitters_all, file = "out/jittered_routes.RData")
