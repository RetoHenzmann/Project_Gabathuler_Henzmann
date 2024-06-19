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
install.packages("rmarkdown")

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

#Segmentation and filter

# Plot the movement data with datetime as the color scale
movment_datamodif <-  ggplot(data_modif, aes(X,Y, color =datetime))+
  geom_point()+
  geom_path()+
  coord_fixed()+
  scale_color_datetime(low = "blue", high ="red")+
  guides(color = guide_colorbar(title.position = "top")
  )

# Define a function to calculate Euclidean distance by element
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Calculate distances for each point within a specified temporal window
movment <- data_modif |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),   
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)),  
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  
  )
# Calculate the mean distance for each point within the temporal window
movment <- movment |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2),na.rm = TRUE)
  ) |>
  ungroup()

# Identify static points based on the mean distance threshold 2
movment <- movment |>
  mutate(static = stepMean < 2)

#Segment-based analysis
#function to assign unique ID for each segment
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
  
}
# Assinge for each segment a unique id and get ride of statics
movment <- movment |>
  mutate(segment_id = rle_id(static)) |>
  filter(!static)

threshold <- quantile(movment$stepMean, 0.25, na.rm = TRUE)

#remove short segments 
movment2 <- movment %>%
  group_by(segment_id) %>%
  filter(sum(stepMean, na.rm = TRUE) > threshold) %>%
  ungroup

#Visualise segmentation
movment2 |> 
  ggplot(aes(X, Y, color=segment_id))+ 
  geom_point()+
  geom_path()+
  coord_equal() +
  theme_classic() +
  theme(legend.position = "none")


#Calculate SPeed and Acceleration
difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}
#Calculate speed and accelleration
movement3 <- movment2 %>%
  group_by(segment_id) %>%
  mutate(
    timelag = difftime_secs(lead(datetime, 3), lag(datetime, 3)),
    steplength = distance_by_element(lead(geometry, 3), lag(geometry, 3))
  ) %>%
  ungroup() %>%
  mutate(
    speed = ifelse(!is.na(timelag) & !is.na(steplength) & timelag > 0, round((steplength / timelag) * 3.6, 2), NA) ,
    acc = round(speed/timelag,2)
  )

# Convert the data to sf objects
movement3_filtered <- movement3 %>%
  filter(!is.na(speed) & speed <= 170)
movment3_sf <- st_as_sf(movement3_filtered, coords = c("X", "Y"), crs = 4326)

# Ensure that only `segment_id`s with more than one point are included in the line conversion
movment3_sf_filtered <- movment3_sf %>%
  group_by(segment_id) %>%
  filter(n() > 1)

# Preapare for printing map
movment3_sf_line <- movment3_sf_filtered %>%
  group_by(segment_id) %>%
  summarize(do_union = FALSE, speed = first(speed)) %>%
  st_cast("LINESTRING")

tmap_mode("view")

# Create interactive map
tm_shape(movment3_sf) +
  tm_dots(col = "speed", size = 0.1) +
  tm_shape(movment3_sf_line) +
  tm_lines(col = "speed") +
  tm_layout(legend.show = FALSE) +
  tm_basemap("Esri.WorldImagery")

#Summary Transport Mode
#Calculate the mean , max and mean speed and acceleration and pivot long
plot_data <-movement3_filtered %>%
  group_by(transport_mode, segment_id) |> 
  st_drop_geometry() |> 
  summarise(
    mean_speed = mean(speed, na.rm=T),
    max_speed = max(speed,na.rm=T),
    min_speed = min(speed,na.rm=T),
    mean_acc = mean(acc, na.rm=T),
    max_acc = max(acc,na.rm=T),
    min_acc = min(acc,na.rm=T),
  ) |> 
  pivot_longer(-c(transport_mode,segment_id))  

ggplot(plot_data,aes(transport_mode, value, fill = transport_mode)) +
  geom_boxplot(outlier.shape = FALSE) +
  facet_wrap(~name, scales = "free_y")
#Save boxplot
ggsave("transport_mode_boxplot.png", plot = p, width = 10, height = 6)

##Link to Geoinformation
#Linking / joint the geodata to the movment data:nearest route
geographic <- rbind(streets, railway) 
geo_join <- st_join(movement3_filtered, geographic, join = st_nearest_feature)

geo_join$OBJEKTART <- as.factor(geo_join$OBJEKTART) 
geo_join_summarise <- geo_join |> 
  group_by(segment_id) |> 
  summarise(
    nearest_route = levels(OBJEKTART)[which.max(table(OBJEKTART))],  
    percentage_nearest_route = max(table(OBJEKTART)) / length(OBJEKTART) * 100 
  ) |> 
  as_tibble() |> 
  select(-c("geometry")) 

#Visualasie one day join

day_join <- geo_join |> filter(as.Date(datetime) == "2024-04-05")

daybox <- st_bbox(day_join)
geographic_clipped <- st_crop(geographic, day_join) 

ggplot() +
  geom_sf(data = geographic_clipped, aes(color = OBJEKTART)) +
  geom_sf(data = day_join, aes(color = OBJEKTART)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10)
  ) +
  labs(color = "transport infrastructure", title = "Geographic Distribution of used Street Types")

# Rename OBJEKTART to stop_type
public_stops <- public_stops |> 
  rename(stop_type  = OBJEKTART)

# Calculate a buffer of 75m around every stop
stops_buffer <- st_buffer(public_stops, 75)
# Perform a spatial join with POSMO data
stop_join <- st_join(movement3_filtered, stops_buffer, join = st_within)

# Extract stop type for the first point of every segment
first_point <- stop_join |> 
  group_by(segment_id) |> 
  slice_head(n = 1) |> 
  select(segment_id, stop_type) |> 
  pull(stop_type)

# Extract stop type for the last point of every segment
last_point <- stop_join |> 
  group_by(segment_id) |> 
  slice_tail(n = 1) |> 
  select(segment_id, stop_type) |> 
  pull(stop_type)

#merging all parameters:

geo_join_summarise <- geo_join_summarise |> 
  cbind(first_point, last_point) 

data_geo_join <- left_join(plot_data, geo_join_summarise, by = "segment_id")


#Implementing Transport mode detection
data_geo_join$travel_mode_det <- NA

# Reshape the data from long to wide format
data_wide <- data_geo_join %>%
  pivot_wider(names_from = name, values_from = value)
# Apply the mutate and case_when logic
data_geo_join1 <- data_wide %>%
  mutate(
    travel_mode_det = case_when(
      nearest_route == "Bahn"~ "Train",
      nearest_route == "Tram" ~ "Tram",
      (first_point %in% c("Haltestelle Bahn", "Haltestelle Bus")) & (last_point %in% c("Haltestelle Bus", "Haltestelle Bahn")) ~ "Bus",
      max_speed > 20 | max_acc > 0.3 ~ "Car",
      mean_speed > 2 | max_speed > 5  ~ "Bike",
      TRUE ~ "Walk"
    )
  )



#Validation 


#Validation Data import
data_val<- read_delim("POSMO_Val_Data.csv")


#Extraaxt Koordinates anc convert to 2056, delet unused colums 
data_val <- data_val %>%
  select(-user_id, -weekday, -place_name)

data_val <- st_as_sf(data_val, coords = c("lon_x", "lat_y"), crs = 4326)
data_val <- st_transform(data_val, crs = 2056)

data_cord_val <- st_coordinates(data_val) 
data_val <- cbind(data_val, data_cord_val) 

#Ensure that the correct time format is set
data_val$datetime <- as.POSIXct(data_val$datetime, format = "%Y-%m-%dT%H:%M:%SZ")


#Segementation and filtering

# Define a function to calculate Euclidean distance by element
distance_by_element <- function(later, now) {
  as.numeric(st_distance(later, now, by_element = TRUE))
}

# Calculate distances for each point within a specified temporal window
vmovement <- data_val |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),   
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)),  
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))
  )

# Calculate the mean distance for each point within the temporal window
vmovement <- vmovement |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2), na.rm = TRUE)
  ) |>
  ungroup()

# Identify static points based on the 50% distance threshold
v_static_threshold <- quantile(vmovement$stepMean, 0.5, na.rm = TRUE)
vmovement <- vmovement |>
  mutate(static = stepMean < v_static_threshold)

# Function for assigning a unique ID for each segment
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

# Assignment of a unique ID for each segment and removal of static points
vmovement <- vmovement |>
  mutate(segment_id = rle_id(static)) |>
  filter(!static)

# Removing short segments
vshort_segment_threshold <- 10
vmovement2 <- vmovement %>%
  group_by(segment_id) %>%
  filter(sum(stepMean, na.rm = TRUE) > vshort_segment_threshold) %>%
  ungroup()

#Calculate speed and accceletation
difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

vmovement3 <- vmovement2 %>%
  group_by(segment_id) %>%
  mutate(
    timelag = difftime_secs(lead(datetime, 3), lag(datetime, 3)),
    steplength = distance_by_element(lead(geometry, 3), lag(geometry, 3))
  ) %>%
  ungroup() %>%
  mutate(
    speed = ifelse(!is.na(timelag) & !is.na(steplength) & timelag > 0, round((steplength / timelag) * 3.6, 2), NA) ,
    acc = round(speed/timelag,2)
  )

# Remove speeds above 170 and  Na
vmovement3_filtered <- vmovement3 %>%
  filter(!is.na(speed) & speed <= 170)

vplot_data <-vmovement3_filtered %>%
  group_by(transport_mode, segment_id) |> 
  st_drop_geometry() |> 
  summarise(
    mean_speed = mean(speed, na.rm=T),
    max_speed = max(speed,na.rm=T),
    min_speed = min(speed,na.rm=T),
    mean_acc = mean(acc, na.rm=T),
    max_acc = max(acc,na.rm=T),
    min_acc = min(acc,na.rm=T),
  ) |> 
  pivot_longer(-c(transport_mode,segment_id)) 

vgeographic <- rbind(streets, railway) 
vgeo_join <- st_join(vmovement3_filtered, vgeographic, join = st_nearest_feature)

vgeo_join$OBJEKTART <- as.factor(vgeo_join$OBJEKTART) 
vgeo_join_summarise <- vgeo_join |> 
  group_by(segment_id) |> 
  summarise(
    nearest_route = levels(OBJEKTART)[which.max(table(OBJEKTART))],  
    percentage_nearest_route = max(table(OBJEKTART)) / length(OBJEKTART) * 100 
  ) |> 
  as_tibble() |> 
  select(-c("geometry")) 

#Visualasie one day join

vday_join <- vgeo_join |> filter(as.Date(datetime) == "2024-06-10")

vdaybox <- st_bbox(vday_join)
vgeographic_clipped <- st_crop(vgeographic, vdaybox) 

ggplot() +
  geom_sf(data = vgeographic_clipped, aes(color = OBJEKTART)) +
  geom_sf(data = vday_join, aes(color = OBJEKTART)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10)
  ) +
  labs(color = "transport infrastructure", title = "Geographic Distribution of used Street Types")

# Rename OBJEKTART to stop_type
vpublic_stops <- public_stops 
#|> rename(stop_type  = OBJEKTART)

# Calculate a buffer of 75m around every stop
vstops_buffer <- st_buffer(vpublic_stops, 75)

# Perform a spatial join with POSMO data
vstop_join <- st_join(vmovement3_filtered, vstops_buffer, join = st_within)

# Extract stop type for the first point of every segment
vfirst_point <- vstop_join |> 
  group_by(segment_id) |> 
  slice_head(n = 1) |> 
  select(segment_id, stop_type) |> 
  pull(stop_type)

# Extract stop type for the last point of every segment
vlast_point <- vstop_join |> 
  group_by(segment_id) |> 
  slice_tail(n = 1) |> 
  select(segment_id, stop_type) |> 
  pull(stop_type)

#merging all parameters:

vgeo_join_summarise <- vgeo_join_summarise |> 
  cbind(vfirst_point, vlast_point) 

vdata_geo_join <- left_join(vplot_data, vgeo_join_summarise, by = "segment_id")

vdata_geo_join$travel_mode_det <- NA

# Reshape the data from long to wide format
vdata_wide <- vdata_geo_join %>%
  pivot_wider(names_from = name, values_from = value)
# Apply the mutate and case_when logic
vdata_geo_join1 <- vdata_wide %>%
  mutate(
    travel_mode_det = case_when(
      nearest_route == "Bahn"~ "Train",
      nearest_route == "Tram" ~ "Tram",
      (vfirst_point %in% c("Haltestelle Bahn", "Haltestelle Bus")) & (vlast_point %in% c("Haltestelle Bus", "Haltestelle Bahn")) ~ "Bus",
      max_speed > 20 | max_acc > 0.3 ~ "Car",
      mean_speed > 2 | max_speed > 5  ~ "Bike",
      TRUE ~ "Walk"
    )
  )


#############Confusionmatrix#############

# Convert columns in data_geo_join1 to factors
data_geo_join1$transport_mode <- as.factor(data_geo_join1$transport_mode)
data_geo_join1$travel_mode_det <- as.factor(data_geo_join1$travel_mode_det)

# Combine levels from both factors
all_levels <- union(levels(data_geo_join1$transport_mode), levels(data_geo_join1$travel_mode_det))

# Set the same levels for both factors in data_geo_join2
data_geo_join1$transport_mode <- factor(data_geo_join1$transport_mode, levels = all_levels)
data_geo_join1$travel_mode_det <- factor(data_geo_join1$travel_mode_det, levels = all_levels)

con_train <- confusionMatrix(data_geo_join1$transport_mode, data_geo_join1$travel_mode_det)

# Extract the confusion matrix table
conf_matrix_table <- as.table(con_train$table)

# Add row and column sums to the table
conf_matrix_table <- addmargins(conf_matrix_table)

# Melt the table to long format for ggplot2
conf_matrix_melt <- melt(conf_matrix_table)


# Identify rows and columns with sums
conf_matrix_melt$is_sum <- with(conf_matrix_melt, Reference == rownames(conf_matrix_table)[nrow(conf_matrix_table)] | Prediction == colnames(conf_matrix_table)[ncol(conf_matrix_table)])

# Plot the confusion matrix using ggplot2
ggplot(data = conf_matrix_melt, aes(x = Reference, y = Prediction, fill = ifelse(is_sum, NA, value))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey90") +
  geom_text(aes(label = sprintf("%d", value)), vjust = 1) +
  theme_minimal() +
  labs(title = "Confusion Matrix Traingsdata", x = "Actual", y = "Predicted") +
  guides(fill = guide_legend(title = "Count"))

####Validation data
# Convert columns in vdata_geo_join1 to factors
vdata_geo_join1$transport_mode <- as.factor(vdata_geo_join1$transport_mode)
vdata_geo_join1$travel_mode_det <- as.factor(vdata_geo_join1$travel_mode_det)

# Combine levels from both factors
all_levelsv <- union(levels(vdata_geo_join1$transport_mode), levels(vdata_geo_join1$travel_mode_det))

# Set the same levels for both factors in data_geo_join2
vdata_geo_join1$transport_mode <- factor(vdata_geo_join1$transport_mode, levels = all_levelsv)
vdata_geo_join1$travel_mode_det <- factor(vdata_geo_join1$travel_mode_det, levels = all_levelsv)

con_val <- confusionMatrix(vdata_geo_join1$transport_mode, vdata_geo_join1$travel_mode_det)

# Extract the confusion matrix table
conf_matrix_val <- as.table(con_val$table)

# Add row and column sums to the table
conf_matrix_val <- addmargins(conf_matrix_val)

# Melt the table to long format for ggplot2
conf_matrix_val_melt <- melt(conf_matrix_val)


# Identify rows and columns with sums
conf_matrix_val_melt$is_sum <- with(conf_matrix_val_melt, Reference == rownames(conf_matrix_val)[nrow(conf_matrix_val)] | Prediction == colnames(conf_matrix_val)[ncol(conf_matrix_val)])

# Plot the confusion matrix using ggplot2
ggplot(data = conf_matrix_val_melt, aes(x = Reference, y = Prediction, fill = ifelse(is_sum, NA, value))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey90") +
  geom_text(aes(label = sprintf("%d", value)), vjust = 1) +
  theme_minimal() +
  labs(title = "Confusion Matrix Validationdata", x = "Actual", y = "Predicted") +
  guides(fill = guide_legend(title = "Count")) 



#############Analysis###########
#Validiation Data set
val_per <- vdata_geo_join1  |>
  mutate(test = ifelse(transport_mode == travel_mode_det, TRUE, FALSE ))

filtered_data <- val_per |> 
  filter(!is.na(travel_mode_det))

ggplot(filtered_data, aes(x = test, y = percentage_nearest_route, fill = test)) +
  geom_boxplot() +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Correct Classification", y = "Percentage") +
  facet_wrap(~ travel_mode_det, scales="free_y")

#Trainings data set 
trai_per <- data_geo_join1  |>
  mutate(test = ifelse(transport_mode == travel_mode_det, TRUE, FALSE ))

filtered_data_train <- trai_per |> 
  filter(!is.na(travel_mode_det))

ggplot(filtered_data_train, aes(x = test, y = percentage_nearest_route, fill = test)) +
  geom_boxplot() +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Correct Classification", y = "Percentage") +
  facet_wrap(~ travel_mode_det, scales="free_y")