library(sf)
library(mapview)
library(leaflet)
library(sp)
library(raster)
library(data.table)
library(jsonlite)
library(hablar)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(councildown)
# library(geojsonsf)

source(here::here("code", "util.R"))


#creating all datasets and separating them out into subsets,
#to be  joined by either spatial orattribute methods
#according to whether or not they have longitudinal data

park_shapes <- st_read("data/original_data/Parks_Properties/geo_export_11cd0a61-e45b-447b-99f0-d4ccbde643ce.shp") # reads in spatial object
# st_transform(park_shapes,'+proj=longlat +datum=WGS84')
# st_crs(park_shapes) <-4326
keeps <- c("acres", "borough",'class','communityb','jurisdicti','name311', 'signname', 'subcategor',
           'typecatego','waterfront', 'gispropnum')
park_shapes<-park_shapes[keeps]
#mapview(park_shapes)
#summary(park_shapes)
#str(park_shapes)


basketball <- fromJSON('./data/original_data/DPR_Basketball_001.json') %>% as.data.frame()
# basketball <- basketball[c('Name','Accessible','lat','lon')]
basketball <-rename_all(basketball, function(x) paste0('bball_',x))
basketball[71,'bball_lon'] <- -73.7922


bball_nolat <- subset(basketball,is.na(bball_lat))
bball_nolat <- bball_nolat[c('bball_Name', 'bball_Accessible')]
bball_nolat$name311 = bball_nolat$bball_Name

bball_lat <- subset(basketball,!is.na(bball_lat))
bball_lat <- transform(bball_lat, bball_lat = as.numeric(bball_lat),
                       bball_lon = as.numeric(bball_lon))
bball_lat <- st_as_sf(bball_lat, coords = c("bball_lon", "bball_lat"), crs='+proj=longlat +datum=WGS84')
st_crs(bball_lat) <- 4326
bball_lat$bball_geo2 <- bball_lat$geometry
bball_lat <- bball_lat %>%
  mutate(bball_Accessible = case_when(bball_Accessible == "N" ~ "No",
                                      bball_Accessible == "Y" ~ "Yes",
                                      TRUE ~ NA_character_),
         popup = pmap_chr(list(bball_Name, bball_Location, bball_Num_of_Courts, bball_Accessible),
                          ~caption_template(header_template(..1, "Basketball court", ..2), body_template(`Number of courts` = ..3, Accessible = ..4))))





#
#
# bocce <- fromJSON('data/original_data/DPR_Bocce_001.json') %>% as.data.frame()
# bocce <- bocce[c('Name','Accessible','lat','lon')]
# #all bocce courts have lat and lon, therefore all may be spatially joined
# bocce <- transform(bocce, lat = as.numeric(lat),
#                   lon = as.numeric(lon))
#
# bocce <- st_as_sf(bocce, coords = c('lon','lat'))
# bocce <-rename_all(bocce, function(x) paste0('bocce_',x))
# st_crs(bocce) <- 4326
#
#
#






# cricket <- fromJSON('data/original_data/DPR_Cricket_001.json') %>% as.data.frame()
# cricket <- cricket[c('Name','lat','lon')]
# cricket <-rename_all(cricket, function(x) paste0('crick_',x))
#
# cricket_nolat <- subset(cricket,is.na(crick_lat))
# cricket_nolat <- cricket_nolat[c('crick_Name')]
# cricket_nolat$name311 <- cricket_nolat$crick_Name
# cricket_lat <- subset(cricket,!is.na(crick_lat))
#
# cricket_lat <- transform(cricket_lat, crick_lat = as.numeric(crick_lat),
#                          crick_lon = as.numeric(crick_lon))
# cricket_lat <- st_as_sf(cricket_lat, coords = c('crick_lon','crick_lat'))
# st_crs(cricket_lat) <- 4326
#





handball <- fromJSON('data/original_data/DPR_Handball_001.json') %>% as.data.frame()
# handball <- handball[c('Name','Num_of_Courts', 'lat','lon')]
handball <-rename_all(handball, function(x) paste0('handb_',x))

handball_nolat <- subset(handball,is.na(handb_lat))
handball_nolat <- handball_nolat[c('handb_Name','handb_Num_of_Courts')]
handball_nolat$name311 <- handball_nolat$handb_Name
handball_lat <- subset(handball,!is.na(handb_lat))

handball_lat <- transform(handball_lat, handb_lat = as.numeric(handb_lat),
                          handb_lon = as.numeric(handb_lon))
handball_lat <- st_as_sf(handball_lat, coords = c('handb_lon','handb_lat'))
st_crs(handball_lat) <- 4326

handball_lat <- handball_lat %>%
  mutate(popup = pmap_chr(list(handb_Name, handb_Location, handb_Num_of_Courts),
                          ~caption_template(header_template(..1, ..2), body_template(`Number of courts` = ..3))))


indoor_pools <- fromJSON("data/original_data/DPR_Pools_indoor_001.json")
outdoor_pools <- fromJSON("data/original_data/DPR_Pools_outdoor_001.json")
pools <- indoor_pools %>%
  rename(Type = Pools_indoor_Type) %>%
  bind_rows(outdoor_pools %>% rename(Type = Pools_outdoor_Type)) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  mutate(Accessible = case_when(Accessible == "N" ~ "No",
                                Accessible == "Y" ~ "Yes",
                                TRUE ~ NA_character_),
         popup = pmap_chr(list(Name, Location, Phone, Setting, Size, Accessible),
                          ~caption_template(header_template(..1, "Pool", ..2, ..3),
                                            body_template(`Indoor/Outdoor` = ..4, Size = ..5, Accessible = ..6))))




tracks <- fromJSON('data/original_data/DPR_RunningTracks_001.json') %>% as.data.frame()
# tracks <- tracks[c('Name','Size', 'RunningTracks_Type','lat','lon')]
tracks <-rename_all(tracks, function(x) paste0('tracks_',x))

tracks_nolat <- subset(tracks,is.na(tracks_lat))
tracks_nolat <- tracks_nolat[c('tracks_Name', 'tracks_Size', 'tracks_RunningTracks_Type')]
tracks_nolat$name311 <- tracks_nolat$tracks_Name
tracks_lat <- subset(tracks,!is.na(tracks_lat))

tracks_lat <- transform(tracks_lat, tracks_lat = as.numeric(tracks_lat),
                        tracks_lon = as.numeric(tracks_lon))
tracks_lat <- st_as_sf(tracks_lat, coords = c('tracks_lon','tracks_lat'))
st_crs(tracks_lat) <- 4326

tracks_lat <- tracks_lat %>%
  mutate(popup = pmap_chr(list(tracks_Name, tracks_Location, tracks_RunningTracks_Type, tracks_Size),
                          ~caption_template(header_template(..1, "Running track", ..2), body_template(Type = ..3, Size = ..4))))






# tennis <- fromJSON('data/original_data/DPR_Tennis_001.json') %>% as.data.frame()
# tennis <- tennis[c('Name','Courts', 'Indoor_Outdoor', 'Accessible','lat','lon')]
# tennis <-rename_all(tennis, function(x) paste0('tennis_',x))
# tennis[73,'tennis_lon'] <- -73.7361
#
#
# tennis_nolat <- subset(tennis,is.na(tennis_lat))
# tennis_nolat <- tennis_nolat[c('tennis_Name','tennis_Courts', 'tennis_Indoor_Outdoor', 'tennis_Accessible')]
# tennis_nolat$name311 <- tennis_nolat$tennis_Name
# tennis_lat <- subset(tennis,!is.na(tennis_lat))
#
# tennis_lat <- transform(tennis_lat, tennis_lat = as.numeric(tennis_lat),
#                         tennis_lon = as.numeric(tennis_lon))
# tennis_lat <- st_as_sf(tennis_lat, coords = c('tennis_lon','tennis_lat'))
# st_crs(tennis_lat) <- 4326
#


play_areas <- st_read("data/original_data/Play Areas/geo_export_c03428b3-2818-41eb-a86b-e798978499a0.shp", stringsAsFactors = FALSE) %>%
  # st_transform("+proj=longlat +ellps=WGS84 +no_defs") %>%
  group_by(gispropnum, borough, park_name) %>%
  summarize() %>%
  mutate(popup = map_chr(park_name, ~caption_template(header_template(., "Playground"), body = NULL)))
# st_crs(play_areas) <-4326


bbq <- fromJSON('data/original_data/DPR_Barbecue_001.json') %>% as.data.frame()
# bbq <- bbq['Name']
# bbq$name311 <- bbq$Name
bbq <- bbq %>%
  inner_join(park_shapes, by = c("Prop_ID" = "gispropnum")) %>%
  st_as_sf() %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  mutate(popup = map_chr(Name, ~caption_template(header_template(.), body = NULL)))



concessions_raw <- fromJSON("data/original_data/DPR_Concessions_001.json", flatten = TRUE)

concessions <- concessions_raw %>%
  filter(type %in% c("Restaurant", "Snack Bar", "Food Cart")) %>%
  # as_tibble() %>%
  mutate_at(vars(contains(".")), ~map(., function(x) as_tibble(x))) %>%
  mutate_at(vars(emails.email, phones.phone, websites.website), ~map(., "value")) %>%
  mutate_at(vars(emails.email, phones.phone, websites.website), as.character) %>%
  mutate_at(vars(emails.email, phones.phone, websites.website), ~case_when(. == "NULL" ~ NA_character_, TRUE ~ .)) %>%
  unnest(locations.location) %>%
  rename_at(vars(contains(".")), ~str_split(., "\\.",simplify = TRUE)[, 2]) %>%
  mutate_at(vars(lat, lng), as.numeric) %>%
  st_as_sf(coords = c("lng", "lat"), crs = '+proj=longlat +datum=WGS84') %>%
  mutate(popup = pmap_chr(list(name, type, website, phone, email, description),
                          ~caption_template(header_template(..1, ..2, ..3, ..4, ..5), body_template(Description = ..6)))) %>%
  identity()

dogs <- fromJSON("data/original_data/DPR_DogRuns_001.json", flatten = TRUE) %>%
  # select(Name, DogRuns_Type) %>%
  identity() %>%
  inner_join(park_shapes, by = c("Prop_ID" = "gispropnum")) %>%
  st_as_sf() %>%
  st_transform('+proj=longlat +datum=WGS84') %>%
  mutate(popup = pmap_chr(list(Name, Address, DogRuns_Type),
                          ~caption_template(header_template(..1, ..2), body_template(Type = ..3))),
         dog_name = Name)

#separate dataframes into lists of frames to be spatially joined (for greatest accuracy)
#and frames to be joined by attribute.

dats_lats <- list(bball_lat, handball_lat, tracks_lat, bbq)
dats_no_lats <- list(bball_nolat, handball_nolat, tracks_nolat)


parks <- reduce(.x=dats_lats, function(x,y) st_join(x %>% st_transform('+proj=longlat +datum=WGS84'),
                                                    y %>% st_transform('+proj=longlat +datum=WGS84'),
                                                    left=TRUE), .init = park_shapes)
parks <- reduce(.x=dats_no_lats, function(x,y) left_join(x,y),.init = parks)

#parks <- reduce(.x=dats_no_lats, function(x,y) left_join.sf(x,y, by= c('name311'='')),.init=parks)

parks$basketball <- as.numeric(!is.na(parks$bball_Name))
# parks$bocce <- as.numeric(!is.na(parks$bocce_Name))
# parks$cricket <- as.numeric(!is.na(parks$crick_Name))
parks$handball <- as.numeric(!is.na(parks$handb_Name))
# parks$tennis <- as.numeric(!is.na(parks$tennis_Name))
parks$tracks <- as.numeric(!is.na(parks$tracks_Name))
parks$bbq <- as.numeric(!is.na(parks$Name))
# parks$dog <- as.numeric(!is.na(parks$dog_name))

drops <- c("bball_Name","bocce_Name",'crick_Name','handb_Name','tennis_Name', 'tracks_Name')
parks <- parks[ , !(names(parks) %in% drops)]
# parks$label <- with(parks, paste(name311,bball_Accessible, sep = '\n'))
parks$centroid <- st_transform(parks$geometry,4326) %>%
  st_centroid() %>%
  st_transform(., '+proj=longlat +datum=WGS84')%>%
  st_geometry()



# all_bball <- subset(parks, basketball = 1,
#                                select=c(basketball, bball_geo2))


# all_bball <- parks[ which(parks$basketball==1), ]
# bball_keeps = c('basketball', 'bball_geo2')
# all_bball<-all_bball[bball_keeps]
# nrow(basketball)
# nrow(all_bball)


labs <- lapply(seq(nrow(parks)),function(i) {
  paste0( '<p>', parks[["name311"]][i], '<p></p>',
          parks[["basketball"]][i], ', ',
          parks[["handball"]][i],'</p><p>',
          parks[["tennis"]][i], '</p>' )
})

parks <- distinct(parks, name311, .keep_all = TRUE) %>%
  mutate(popup = map_chr(name311, ~caption_template(header_template(.), NULL)))


colors <- c(
  "Basketball courts" = "#d05d4e",
  "Handball courts" = "#ff9938",
  "Running tracks" = "#960057",
  "Playgrounds" = "#ff59bf",
  "Parks with BBQ facilities" = "#F59F00",
  "Food service" = "#be4bdb",
  "Pools" = "#228ae6",
  "Dog runs and off-leash areas" = "#a07952"
)

labels <- paste0("<div style='background-color:",
      colors,
      ";position: relative; right:2px; top: 4px; display: inline-block; width: 1em;height: 1em; margin: 2px;'></div>",
      names(colors))
names(labels) <- names(colors)

crs_string <- '+proj=longlat +datum=WGS84'

m <- leaflet() %>%
  # addProviderTiles("CartoDB.Positron") %>%
  addCouncilStyle() %>%
  addPolygons(data = park_shapes %>% st_transform(crs_string), weight =0,
              fillColor = "#82c91e",
              fillOpacity = .4,
              # popup = ~popup,#lapply(labs, HTML),
              labelOptions = labelOptions(noHide = F,
                                          direction = 'auto')) %>%
  addCircleMarkers(data = play_areas %>% st_centroid() %>% st_transform(crs_string),
              # weight = 10,
              # label = 'park_name',
              fillColor = unname(colors['Playgrounds']),
              group = unname(labels['Playgrounds']),
             fillOpacity = .8,
             stroke = 30, color = "#00000000",
             radius = 4,
             popup = ~popup)%>%
  addCircleMarkers(data=bball_lat, group = unname(labels['Basketball courts']), popup = ~popup, fillColor= unname(colors["Basketball courts"]), fill = TRUE, fillOpacity = .8, stroke = 30, color = "#00000000", radius = 4)%>%
  addCircleMarkers(data=handball_lat, group = unname(labels['Handball courts']), fillColor= unname(colors['Handball courts']), fill = TRUE, fillOpacity = .8, stroke = 30, color = "#00000000", radius = 4, popup = ~popup)%>%
  addCircleMarkers(data=tracks_lat, group = unname(labels['Running tracks']), fillColor= unname(colors['Running tracks']), fill = TRUE, fillOpacity = .8, stroke = 30, color = "#00000000", radius = 4, popup = ~popup)%>%
  addPolygons(data = bbq, fillColor = unname(colors["Parks with BBQ facilities"]), group = unname(labels["Parks with BBQ facilities"]), fillOpacity = 1, stroke = FALSE, popup = ~popup) %>%
  addCircleMarkers(data = concessions, fillColor = unname(colors["Food service"]), group = unname(labels["Food service"]), fillOpacity = .8, stroke = 30, color = "#00000000", radius = 4, popup = ~popup, popupOptions = popupOptions(maxHeight = 400)) %>%
  addCircleMarkers(data = pools, fillColor = unname(colors["Pools"]), group = unname(labels["Pools"]), fillOpacity = .8, stroke = 30, color = "#00000000", radius = 4, popup = ~popup) %>%
  addPolygons(data = dogs, fillColor = unname(colors["Dog runs and off-leash areas"]), group =  unname(labels["Dog runs and off-leash areas"]), fillOpacity = 1, stroke = FALSE, popup = ~popup) %>%
  addLayersControl(
    baseGroups = unname(labels),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  ) %>%
  # addLegend("topleft", values = ~basketball,
  #           colors = c('red', 'blue', 'pink', 'green', 'purple', 'orange', 'black', "gray"),
  #           labels = c('basketball','bocce', 'cricket', 'handball','tennis','tracks', 'play areas', "BBQ"),
  #           title = "test",
  #           opacity = 1
  # ) %>%
  setView(-73.88099670410158,40.72540497175607,  zoom = 10.5) %>%
  registerPlugin(geocoder) %>%
  # registerPlugin(fontawsome_markers) %>%
  onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY"))) %>%
identity()
m

htmlwidgets::saveWidget(m, file = "facilities_map.html", selfcontained = FALSE)
unlink(here::here("results", "facilities_map_files"), recursive = TRUE)
file.rename("facilities_map.html", "results/facilities_map.html")
file.rename("facilities_map_files", "results/facilities_map_files")


library(ggmap)
register_google(Sys.getenv("GEOCODE_API_KEY"))

boros <- st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")


playgrounds <- play_areas %>%
  ungroup() %>%
  mutate(lon = (st_centroid(.) %>% st_coordinates())[,1],
         lat = (st_centroid(.) %>% st_coordinates())[,2]) %>%
  as_tibble() %>%
  drop_na(lat, lon) %>%
  mutate(rev_geocode = map2(lon, lat, ~revgeocode(c(.x, .y))))

playgrounds %>%
  mutate(rev_geocode = as.character(rev_geocode)) %>%
  select(Name = park_name, Location = rev_geocode, Borough = borough) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/playgrounds.json")

bball_lat %>%
  st_join(boros, st_intersects) %>%
  as_tibble() %>%
  select(Name = bball_Name, Location = bball_Location,
         `Number of courts` = bball_Num_of_Courts,
         Accessible = bball_Accessible,
         Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/bball.json")
handball_lat %>%
  st_join(boros, st_intersects) %>%
  as_tibble() %>%
  select(Name = handb_Name, Location = handb_Location, `Number of courts` = handb_Num_of_Courts, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/handball.json")
tracks_lat %>%
  st_join(boros, st_intersects) %>%
  as_data_frame() %>%
  select(Name = tracks_Name, Location = tracks_Location, Size = tracks_Size, `Surface type` = tracks_RunningTracks_Type, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/tracks.json")
bbq %>%
  st_join(boros, st_intersects) %>%
  as_data_frame() %>%
  select(Name, Location, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/bbq.json")
concessions %>%
  st_join(boros, st_intersects) %>%
  as_data_frame() %>%
  select(Name = name, Location = location, Type = type, Website = website, Phone = phone, Email = email, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/concessions.json")
pools %>%
  st_join(boros, st_intersects) %>%
  as_data_frame() %>%
  select(Name, Location, Phone, Setting, Accessible, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/pools.json")
dogs %>%
  st_join(boros, st_intersects) %>%
  as_data_frame() %>%
  select(Name, Address, Type = DogRuns_Type, Accessible, Borough = boro_name) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(~replace_na(., "")) %>%
  write_json("results/data_files/dogs.json")


#
# parks[duplicated(parks$geometry),] %>%
#   as_tibble() %>%
#   select_if(~!is.list(.)) %>%
#   group_by(name311) %>%
#   summarise_all(~length(unique(.))) %>%
#   gather(col, num, -name311) %>%
#   pull(num) %>%
#   unique()
#
#
# ggplot(play_areas) + geom_sf()
#
#
# l
#




































# plot(st_geometry(parks$geometry), col = sf.colors(12, categorical = TRUE), border = 'grey',
#      axes = TRUE)
# plot(st_geometry(parks$centroid), pch = 3, col = 'red', add = TRUE)
