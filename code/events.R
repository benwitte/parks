library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)
library(councildown)
library(lubridate)
# library(timevis)
library(htmltools)
library(htmlwidgets)
library(testthat)
library(gmailr)

send_mail <- function() {
  send_message(mime(from = "nyccdummyemail@gmail.com", to = "bwitte@council.nyc.gov", subject = ":( Oh no", body = "Ya done goof'd!"))
  q(status = 1)
}


options(error = send_mail)

source(here::here("code", "util.R"))

updated_at <- list(time = Sys.time())

events_raw <- fromJSON("https://www.nycgovparks.org/xml/events_300_rss.json") %>%
  unite(start, starts_with("start"), sep = " ") %>%
  unite(end, starts_with("end"), sep = "") %>%
  mutate(start = ymd_hm(start),
         end = ymd_hm(end)) %>%
  filter(start > Sys.time()) %>%
  # as_tibble() %>%
  # mutate(coords = map(coordinates, ~as_tibble(str_split(., ";", simplify = TRUE))),
  #        coords = map(coords, ~separate(., 1, c("lat", "lng"), sep = ", ", convert = TRUE))) %>%
  # unnest(coords) %>%
  # st_as_sf(coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  identity()

# sum(duplicated(events_raw$geometry))

# make_caption <- function(dat) {
#   window_start <- as_datetime(dat$start[1] - 30*60)
#   window_end <- as_datetime(dat$end[1] + 30*60)
#   dat %>%
#     dplyr::select(description, start, end) %>%
#     rename(content = description) %>%
#     # mutate(id = row_number()) %>%
#     timevis::timevis() %>%
#     # setWindow(window_start, window_end) %>%
#     identity()
#   # print(window_start)
#   # print(window_end)
# }

make_caption <- function(dat) {
  dat <- dat %>%
    arrange(start) %>%
    mutate(cap = paste("<h4>", title, "</h4>", description))
  out <- paste(dat$cap, collapse = "<hr>")
  out
}

expect_is(events_raw, "data.frame", info = "Raw event data is not a data frame.")
expect_gt(nrow(events_raw), 0)
expect_is(events_raw$description, "character", info = "Description is not text data")
expect_is(events_raw$coordinates, "character", info = "Coordinates are not text data")
expect_equal(sum(is.na(events_raw$coordinates)), 0, info = "Missing coordinates")
expect_equal(sum(is.na(events_raw$description)), 0, info = "Missing descriptions")


events <- events_raw %>%
  group_by(coordinates) %>%
  nest() %>%
  mutate(caption = map_chr(data, make_caption)) %>%
  mutate(coords = map(coordinates, ~as_tibble(str_split(., ";", simplify = TRUE))),
         coords = map(coords, ~separate(., 1, c("lat", "lng"), sep = ", ", convert = TRUE))) %>%
  unnest(coords) %>%
  st_as_sf(coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  identity()

expect_is(events, "sf", info = "Events not spatial data")
expect_equal(sum(is.na(events$geometry)), 0, info = "Missing points")
expect_is(events$caption, "character", info = "Generated caption is not text data")
expect_equal(sum(is.na(events$caption)), 0, info = "Missing captions")

events_map <- leaflet(events) %>%
  addCouncilStyle() %>%
  addCircleMarkers(radius = 4,
                   fillColor = "#F59F00",
                   popup = ~councilPopup(caption), popupOptions = popupOptions(maxHeight = 250), fillOpacity = .8,
                   stroke = 30,
                   color = "#00000000") %>%
  setView(-73.88099670410158,40.72540497175607,  zoom = 10.5)%>%
  registerPlugin(geocoder) %>%
  # registerPlugin(fontawsome_markers) %>%
  onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY")))

time <- Sys.time()

htmlwidgets::saveWidget(events_map, file = "events_map.html", selfcontained = FALSE)

expect_true(file.mtime("events_map.html") >= time, info = "events_map.html was not properly saved")
expect_true(file.mtime("events_map_files") >= time, info = "events_map_files was not properly saved")

delete <- unlink(here::here("results", "events_map_files"), recursive = TRUE)
move1 <- file.rename("events_map.html", "results/events_map.html")
move2 <- file.rename("events_map_files", "results/events_map_files")

expect_equal(delete, 0, info = "Could not delete old JS dependencies")
expect_true(move1, info = "Could not move events_map.html to results/")
expect_true(move2, info = "Could not move events_map_files/ to results/")

num_events <- list(num_events = nrow(events_raw))

expect_equal(length(num_events), 1)
expect_equal(length(num_events[[1]]), 1)
expect_gt(num_events[[1]], 0)

write_json(num_events, "results/num_events.json")
write_json(updated_at, "results/update_time.json")

events_out <- events_raw %>%
  mutate(pretty_time = format(start, format = "%b %d %I:%M %p")) %>%
  arrange(start) %>%
  filter(start > Sys.Date() + 1)

write_json(events_out, "results/data_files/events.json")
