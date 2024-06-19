#Import librarys
library("gitcreds")
library("readr")
library("dplyr")
library("sf")
library("ggplot2")
library("tidyr")
library("tmap")
library("ConfusionTableR")
library("caret")
library("reshape2")

#Import training dataset
data <- read_delim("Posmo_Movement_data.csv")
#Data filter, only time and coordinates, travel mode
data <- data %>%
  select(-user_id, -weekday, -place_name)
# Daten in ein sf-Objekt umwandeln
data_modif <- st_as_sf(data, coords = c("lon_x", "lat_y"), crs = 4326)
# Koordinatensystem zu EPSG 2056 transformieren
data_modif <- st_transform(data_modif, crs = 2056)

#Extract coordinate to owwn column
data_coord <- st_coordinates(data_modif) 
data_modif <- cbind(data_modif, data_coord) 
#Ensure that the correct time format is set
data_modif$datetime <- as.POSIXct(data_modif$datetime, format = "%Y-%m-%dT%H:%M:%SZ")
### Filter out the day which the coordinates where outside Swiss and Lichtenstein
data_modif <- data_modif |>
  st_as_sf(coords = c("x", "y"), crs = 2056, remove = FALSE) |>
  filter(datetime < "2024-04-07 09:00:00"| datetime >= "2024-04-11 17:34:50")

#Import shapefile TLM3D
streets <- st_read("swisstlm3d_2024-03_2056_5728.shp/TLM_STRASSEN/swissTLM3D_TLM_STRASSE.shp")|> select(OBJEKTART, geometry) |> 
  filter(OBJEKTART != "Verbindung" & OBJEKTART != "Raststaette" & OBJEKTART != "Dienstzufahrt" & OBJEKTART != "Verbindung" & OBJEKTART != "Zufahrt " & OBJEKTART != "Klettersteig") |> 
  st_transform(2056)
#Import public stops and railway
public_stops <- st_read("swisstlm3d_2024-03_2056_5728.shp/TLM_OEV/swissTLM3D_TLM_HALTESTELLE.shp") |> 
  select(OBJEKTART, geometry) |> 
  st_transform(2056) 
railway <- st_read("swisstlm3d_2024-03_2056_5728.shp/TLM_OEV/swissTLM3D_TLM_EISENBAHN.shp") |> 
  select(VERKEHRSMI, geometry) |> 
  rename(OBJEKTART = VERKEHRSMI) |> 
  st_transform(2056)

