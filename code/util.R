caption_template <- function(header, body) {
  if (!is.null(body)) {
  paste(header, body, sep = "<hr>") %>%
    councilPopup()
  } else {
    header %>%
      councilPopup()
    }
}


header_template <- function(name, ...) {
  dots <- list(..., sep = "<br>")
  dots <- dots[!is.na(dots)]
  dots <- do.call(paste, dots)

  # browser()
  paste(tags$h4(name), "<small><em>", dots, "</em></small>")
}

body_template <- function(...) {
  dots <- list(...)
  # browser()
  dots <- dots[!is.na(dots)]


  labels <- names(dots)

  reduce2(labels, dots, function(old, x, y) paste0(old,  tags$strong(x), ": ", y, "<br>"), .init = "")
}


# caption_template(header_template("Test", "123 Main St", "212 555 1234"), body_template(test = "Yes", fail = "No"))

# JS for map --------------------------------------------------------------

geocoder <- htmlDependency("leaflet.geocoder", "1.6.0",
                           src = c(href = "https://unpkg.com/leaflet-control-geocoder/dist"),
                           script = "Control.Geocoder.js",
                           stylesheet = "Control.Geocoder.css"
)

fontawsome_markers <- htmlDependency("fontawesome", "2.0.4",
                                     src = c(href = "https://unpkg.com/leaflet.awesome-markers@2.0.4/dist"),
                                     script = "leaflet.awesome-markers.js",
                                     stylesheet = "leaflet.awesome-markers.css")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


geocode_js <- "function(el, x, data) {
var MapGeoCoderProvider = L.Control.Geocoder.google(data.key, {}),
marker;
map = this;

L.Control.geocoder({geocoder:MapGeoCoderProvider,
                    defaultMarkGeocode: false,
                    collapsed: false,
                    placeholder: 'Address | Borough'})
.on('markgeocode', function(e) {
var bbox = e.geocode.bbox;
var poly = L.polygon([
bbox.getSouthEast(),
bbox.getNorthEast(),
bbox.getNorthWest(),
bbox.getSouthWest()
]);
map.fitBounds(poly.getBounds());


if (marker) {
marker.setLatLng(e.geocode.center);
} else {
marker = L.circleMarker(e.geocode.center, {weight: 1, fillOpacity: .5}).addTo(map);
}
map.setZoom(16);

})
.addTo(this);
}"
