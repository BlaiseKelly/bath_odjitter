## loads in ATC data

library(tidyverse)
library(openxlsx)
library(reshape2)
library(lubridate)
library(sf)

##define coordinate systems
latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"

## read in data
xlsxs <- list.files("data/ATCs/", pattern = ".xlsx", full.names = TRUE)

## loop through files and combine data
atc_dat <- list()
sites <- list()
x <- xlsxs[3]
for (x in xlsxs){

atc <- read.xlsx(x, sheet = 1, startRow = 4)[1:24,2:8]

atc_info <- colnames(read.xlsx(x, sheet = 1, rows = 1))

site_ID <- atc_info[2]

coords <- colsplit(atc_info[6], ",", c("x", "y"))

site_nam <- data.frame(site_ID = atc_info[2], x = coords$x, y = coords$y) %>% 
  st_as_sf(coords = c("x", "y"), crs = ukgrid) %>%
  st_transform(latlong)

atc_down1 <- atc %>% 
  mutate(hour = as.character(seq(1:NROW(atc)))) %>% 
  melt("hour") %>% 
  mutate(variable = as.numeric(as.character(variable))) %>% 
  mutate(day = as.Date(variable, origin = "1899-12-30")) %>%
  mutate(date = ymd_h(paste0(day, "_", hour))) %>% 
  mutate(site_ID = atc_info[2]) %>% 
  select(site_ID, date, trips = value)

atc2 <- read.xlsx(x, sheet = 2, startRow = 4)[1:24,2:8]

atc_down2 <- atc2 %>% 
  mutate(hour = as.character(seq(1:NROW(atc)))) %>% 
  melt("hour") %>% 
  mutate(variable = as.numeric(as.character(variable))) %>% 
  mutate(day = as.Date(variable, origin = "1899-12-30")) %>%
  mutate(date = ymd_h(paste0(day, "_", hour))) %>% 
  mutate(site_ID = atc_info[2]) %>% 
  select(site_ID, date, trips = value)

atc_down_all <- rbind(atc_down1, atc_down2)

atc_dat[[site_ID]] <- atc_down_all
sites[[site_ID]] <- site_nam

}

## combine data from all sites
all_dat <- do.call(rbind, atc_dat)
## combine geometry from all sites
all_sites <- do.call(rbind, sites)

## save data for use in other scripts
save(all_dat, all_sites, file = "out/validation_dat.RData")

