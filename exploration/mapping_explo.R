# Authors : Gabriel Bergeron
# Contributors :

# Create simple map of the playing area

## Leading libraries
# the chunk of code below can be run to load all packages and install those that are not already installed
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("raster", "tidyverse", "sf")


## Load data

# Ressource localisation
ress_loc <- read.csv2("data_raw/Resources_Location_Final.csv") |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs=4326) |> 
  st_transform(crs = 32619)

# Refuge localisation
refu_loc <- read.csv2("data_raw/Refuges_Locations_Final.csv") |> 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs=4326) |> 
  st_transform(crs = 32619)

# Elevation gradient raster
elev_rast <- raster::raster("data_raw/map_layers/elevation_raster_espg2959.tif") |> 
  as.data.frame(xy = TRUE) |> 
  filter(elevation_raster_espg2959 > 0)

# Must change the CRS of the raster to match the sf object (refuge and ress)
# Must change the extent of the raster to plot it alongside of the other data
ggplot() +
  geom_raster(data = elev_rast,
              aes(x = x, y = y, fill = elevation_raster_espg2959)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10))

# elev_spdf <- as(elev_rast, "SpatialPixelsDataFrame")
# elev_rast <- as.data.frame(elev_spdf)
# colnames(elev_rast) <- c("value", "x", "y")

plot(elev_rast)
points(ress_loc$geometry)


ggplot(ress_loc) + 
  geom_sf(aes(), col = "red") + 
  geom_sf(refu_loc, mapping = aes(), col = "blue") +
  coord_sf(datum = st_crs(32619)) + 
  geom_tile(data = elev_rast, 
            aes(x = x, y = y, fill = value),
            alpha = 0.8)
