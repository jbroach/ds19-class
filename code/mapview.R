# Playing with mapping
library(dplyr)
#install.packages("sp")
#install.packages("sf")
library(sf)
#install.packages("mapview")
library(mapview)

biketown <- read.csv("data/biketown-2018-trips.csv",
                     stringsAsFactors = F)
head(biketown)

hubs_start_sf <- biketown %>%
  group_by(StartHub) %>%
  summarise(lat = mean(StartLatitude), lng = mean(StartLongitude),
            starts = n()) %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("lng", "lat"), 
           crs = 4326, agr = "constant")

mapview(hubs_start_sf, zcol = "starts") # if basemap won't load in RStudio
                                        # click "show in new window" button
                                        #  in viewer pane (just right of sweep)
mapview(hubs_start_sf, cex = "starts", legend = T)
mapview(hubs_start_sf, zcol = "starts", cex = "starts")

hubs_end <- biketown %>%
  group_by(EndHub) %>%
  summarise(lat = mean(EndLatitude), lng = mean(EndLongitude),
            ends = n())

hubs_end_sf <- hubs_end %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("lng", "lat"), 
           crs = 4326, agr = "constant")

mapview(hubs_end_sf, zcol = "ends", cex = "ends")

hubs_ratio_sf <- inner_join(hubs_start_sf, hubs_end,
                            by = c("StartHub" = "EndHub")) %>%
  mutate(starts_to_ends = starts / ends, ends_to_starts = ends / starts)

summary(hubs_ratio_sf)

mapview(hubs_ratio_sf, zcol = "starts_to_ends", cex = "starts_to_ends")
mapview(hubs_ratio_sf, zcol = "ends_to_starts", cex = "ends_to_starts")

m1 <- mapview(hubs_ratio_sf, zcol = "starts_to_ends", 
              cex = "starts_to_ends", legend = F)
m2 <- mapview(hubs_ratio_sf, zcol = "ends_to_starts", 
              cex = "ends_to_starts", legend = F)

sync(m1, m2)


