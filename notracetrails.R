library(jsonlite)
library(data.table)
library(dplyr)
library(sf)
library(mapview)
library(mapdata)
library(tidyverse)
library(RColorBrewer)
library(httr)
library(ggplot2)
library(rvest)
library(NADA)
library(tidyr)
library(ggbreak)
library(stringr)

confidence_interval_width <- function(proportion, sample_size, population_size){
    1.96*sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1))
}

BootMean <- function(data) {
    B <- 10000
    mean <- numeric(B)
    n = length(data)
    
    set.seed(342)
    for (i in 1:B) {
        boot <- sample(1:n, size=n, replace = TRUE)
        mean[i] <- mean(data[boot], na.rm = T)
    }
    return(quantile(mean, c(0.025, 0.5, 0.975), na.rm = T))
}

#Read Cleaned Data

full_points <- sf::read_sf("Full Points.geojson") %>%
  left_join(sf::read_sf("Full Points.geojson") %>%
              distinct(Mile, runDescription) %>%
              dplyr::filter(runDescription != "") %>%
              rename(check = runDescription) %>%
              filter(!duplicated(Mile))) %>% #Checked this rigorously to make sure that no mile markers were duplicated that also were missing things. 
  mutate(runDescription = ifelse(runDescription == "", check, runDescription)) %>%
  select(Name, photoURL, lat, long, rubbishRunID, userName, rubbishType, Morphology, Brand, city, totalNumberOfItemsTagged, numberOfItemsTagged, runDescription, datetime, X, Y,Mile, dist_m) %>%
  rename(trashpiece_lat = lat, 
         trashpiece_long = long, 
         rubbish_run_x = X, 
         rubbish_run_y = Y
         ) %>%
  mutate(trashpiece_lat = as.numeric(trashpiece_lat), 
         trashpiece_long = as.numeric(trashpiece_long),
         rubbish_run_x = as.numeric(rubbish_run_x),
         rubbish_run_y = as.numeric(rubbish_run_y)) %>%
  as.data.table()

full_summary <- sf::read_sf("Full Summary.geojson") %>%
  filter(!Survey_Type %in% c("unconfirmed", "skipped", "uncomfirmed")) %>%
  as.data.table() %>%
  select(X, Y, Mile, Survey_Type) %>%
  rename(MileMark_X = X, 
         MileMark_Y = Y) %>%
  mutate(MileMark_X = as.numeric(MileMark_X), 
         MileMark_Y = as.numeric(MileMark_Y))

joined <- inner_join(full_points, full_summary, by = "Mile") %>%
  mutate(Mile = as.integer(Mile), 
         totalNumberOfItemsTagged = as.integer(totalNumberOfItemsTagged), 
         numberOfItemsTagged = as.integer(numberOfItemsTagged)) %>%
  mutate(rubbishType = case_when(
    rubbishType %in% c("", "otherLitter", "uncategorized") ~ "other", 
    totalNumberOfItemsTagged == 0 ~ NA,
    .default = rubbishType
  )) %>%
  mutate(Morphology = tolower(Morphology)) %>%
  mutate(Morphology = case_when(
    Morphology %in% c("duck tape") ~ "duct tape", 
    Morphology %in% c("fragement") ~ "fragment", 
    Morphology %in% c("hiking pole tip") ~ "hiking pole ends", 
    Morphology %in% c("package end", "package corner", "wrapper end") ~ "package ends",
    Morphology %in% c("sunflower seed") ~ "sunflower seeds", 
    Morphology %in% c("wipe", "toilet paper") ~ "wipes", 
    Morphology %in% c("") ~ "other", 
    totalNumberOfItemsTagged == 0 ~ NA,
    .default = Morphology
  )) %>%
  mutate(Brand = tolower(Brand)) %>%
  mutate(Brand = case_when(
    Brand %in% c("") ~ "other", 
    totalNumberOfItemsTagged == 0 ~ NA,
    .default = Brand
  )) %>%
  mutate(Number_Of_People = as.numeric(gsub(" .*", "", str_extract(pattern = "([0-9] surveyer)|([0-9] people)|([0-9] cleaners)|([0-9] person)", runDescription)))) %>%
  mutate(Number_Of_People = ifelse(Number_Of_People == 0, NA, Number_Of_People)) %>%
  mutate(Reported_Mile_Marker = str_extract(pattern = "(mile [0-9]*)|([0-9]* mile)", runDescription)) %>%
  mutate(Is_Control = grepl("(DUPLICATE)|(REPEAT)", runDescription)) %>%
  mutate(Extrapolation_Mile_Fraction = case_when(
    grepl("0.03 miles, stopped at 100 items, 2 suveyers; lots more trash in", runDescription) ~ 0.03,
    grepl("0.41 miles to hit 100 pieces, 2 surveyers; trail followed dirt road- surveyed road and not sides", runDescription) ~ 0.41,
    .default = NA
  )) %>%
  mutate(Snow_Percent = str_extract(Survey_Type, "[0-9]*%")) %>%
  mutate(Snow_Percent = case_when(
    Survey_Type == "full_snow" ~ "100%", 
    Survey_Type == "some_snow" ~ "10%",
    !is.na(Snow_Percent) ~ Snow_Percent,
    .default = "0%"
  )) %>%
  mutate(Snow_Percent = as.numeric(gsub("%", "", Snow_Percent))) %>%
  mutate(Night_Survey = ifelse(grepl("(night)", Survey_Type), TRUE, FALSE))

fwrite(joined, "clean_data.csv")

morph_categories <- joined %>%
  distinct(Morphology)

fwrite(morph_categories, "morphologies.csv")

mat_categories <- joined %>%
  distinct(rubbishType)

fwrite(mat_categories, "materials.csv")

set.seed(1223)
summary_prep <- joined |>
  group_by(Mile, 
           rubbish_run_x, 
           rubbish_run_y, 
           Is_Control, 
           Night_Survey, 
           runDescription, 
           Snow_Percent,
           Number_Of_People,
           Extrapolation_Mile_Fraction) |>
  summarise(count = sum(as.numeric(totalNumberOfItemsTagged))) |>
  ungroup() |>
  as.data.frame() |>
  mutate(count = ifelse(!is.na(Extrapolation_Mile_Fraction), count/(Extrapolation_Mile_Fraction/0.621371), count)) %>%
  mutate(fit = cenros(.$count, .$count == 0)$modeled) %>% #replace zero values. 
  mutate(count = ifelse(count == 0, sample(fit[fit < 1], length(fit[fit < 1]), replace = F), count)) %>%
  select(-fit)
  
hist(log10(summary_prep$count))

  #0.03 miles, stopped at 100 items, 2 suveyers; lots more trash in
  #0.41 miles to hit 100 pieces, 2 surveyers; trail followed dirt road- surveyed road and not sides
  
#Tests
identical(joined$Name,full_points$Name)
!any(is.na(joined$totalNumberOfItemsTagged))

#Summary stats ----

## Test bias ----
## Controls
control_test <- summary_prep |>
  group_by(Mile, rubbish_run_x, rubbish_run_y, Is_Control) |>
  summarise(count = sum(as.numeric(count))) |>
  ungroup() |>
  as.data.frame() |>
  pivot_wider(names_from = "Is_Control", values_from = "count") |>
  filter(!is.na(`TRUE`))

#correction on the total counts
control_correction_factor <- mean(control_test$`FALSE`)/mean(control_test$`TRUE`)
#high uncertainty suggests that we may want to follow up with a methods study to further quantify the uncertainties in the technique.
BootMean(control_test$`TRUE`)
BootMean(control_test$`FALSE`)

#Night Difference
night_test <- summary_prep |>
  group_by(Mile, rubbish_run_x, rubbish_run_y, Night_Survey, runDescription) |>
  summarise(count = sum(as.numeric(count))) |>
  ungroup() |>
  as.data.frame() |>
  pivot_wider(names_from = "Night_Survey", values_from = "count")

#correction on the total counts
BootMean(night_test$`FALSE`)
BootMean(night_test$`TRUE`)
night_cor_factor <- mean(night_test$`FALSE`, na.rm = T)/mean(night_test$`TRUE`, na.rm = T) #could use this to correct for a daylight observation. 

#Snow Effect
snow_test <- summary_prep |>
  group_by(Mile, rubbish_run_x, rubbish_run_y, Snow_Percent) |>
  summarise(count = sum(as.numeric(count))) |>
  ungroup() |>
  as.data.frame() |>
  mutate(Snow_Percent = Snow_Percent > 0) |>
  pivot_wider(names_from = "Snow_Percent", values_from = "count")

#correction on the total counts
BootMean(snow_test$`FALSE`)
BootMean(snow_test$`TRUE`)
snow_correction_factor <- mean(snow_test$`FALSE`, na.rm = T)/mean(snow_test$`TRUE`, na.rm = T) 
#could use this to correct for a non-snow observation. 

#Surveyor Effect
surveyor_test <- summary_prep |>
  group_by(Mile, rubbish_run_x, rubbish_run_y, Number_Of_People) |>
  summarise(count = sum(as.numeric(count))) |>
  ungroup() |>
  as.data.frame() |>
  filter(Number_Of_People < 5) #This point has too much leverage. 

ggplot(surveyor_test, aes(x = Number_Of_People, y = count)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  theme_classic(base_size = 14) +
  labs(y = "Trash Count per km", x = "Number of Surveyors")

sum(is.na(summary_prep$Number_Of_People))
table(surveyor_test$Number_Of_People)
model_people <- lm(log10(count)~Number_Of_People, data = surveyor_test)
summary(model_people) 
#has a significant effect but maybe shouldn't include the 
#effect because it could introduce a spatial bias to the data since single 
#surveys were done more often in the norther states?

10^model_people$coefficients[2]

correction_factor <- 10^(4*model_people$coefficients[2]+model_people$coefficients[1])

correction_factor/0.71

mean_people <- mean(surveyor_test$Number_Of_People)

## Concentrations ----
summary <- summary_prep |>
  group_by(Mile, rubbish_run_x, rubbish_run_y, Number_Of_People, Night_Survey, Snow_Percent) |>
  filter(!Is_Control) |>
  summarise(count = sum(as.numeric(count))) |>
  ungroup() |>
  mutate(count_people_cor = ifelse(!is.na(Number_Of_People), 
                        count * (correction_factor/10^(Number_Of_People*model_people$coefficients[2]+model_people$coefficients[1])), 
                        count * (correction_factor/10^(mean_people*model_people$coefficients[2]+model_people$coefficients[1])))) %>%
  mutate(count_surveyor_acc = count*control_correction_factor) |>
  mutate(count_night = ifelse(Night_Survey, count*night_cor_factor, count)) |>
  mutate(count_snow = ifelse(Snow_Percent > 0, count*snow_correction_factor, count)) |>
  as.data.frame() |>
  st_as_sf(coords = c("rubbish_run_x", "rubbish_run_y"), crs = 4326) |>
  mutate(count_group = cut(count, breaks = c(-1,0, 10,100,1000,10000), labels = c("0", "1-10", "11-100", "100-1000", "1000-10000")))

options(scipen = 99)
ggplot(data = summary, aes(x = Mile, y = count)) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1692, color = "green", linewidth = 2) +
  geom_vline(xintercept = 2174, color = "purple", linewidth = 2) +
    geom_point(color = "black") +
    theme_classic(base_size = 15) +
    labs(y = "Trash Count per km", x = "Mile Marker (1.6 km Marker)") +
    scale_y_log10(breaks = c(10^(-2:4))) +
  geom_smooth() 
 

BootMean(summary$count)

BootMean(summary$count_people_cor)

BootMean(summary$count_surveyor_acc)

BootMean(summary$count_night)

BootMean(summary$count_snow)


mean(summary$count)

mean(summary$count_people_cor)

mean(summary$count_surveyor_acc)

mean(summary$count_night)

mean(summary$count_snow)

#Prediction total
BootMean(summary$count) * 2650/0.621371

#Prediction per person
BootMean(summary$count) * 2650/0.621371 / 800

mean(summary$count, na.rm = T) * 2650/0.621371 #This is being heavily upweighted by the worst site. Can mention. 

#Surveys without counts
sum(summary$count < 1)/length(summary$count)


ggplot() +
  stat_ecdf(aes(x = summary$count)) +
  scale_x_log10()

class(summary)

#mapview(pct, legend = FALSE, color = "gray") +
mapview(summary, zcol = 'count_group', stroke = NA, cex = 2, alpha.regions = 0.75, legend = TRUE, color = hcl.colors(4, palette = "ArmyRose")) 

fwrite(summary, "summary.csv")

## Trash Types ----

unique(joined$rubbishType) |>
  sort()

#Change the unique mile, it filters later. 
grid = expand.grid(Mile = unique(summary$Mile), rubbishType = unique(joined$rubbishType))

types_join <- joined |>
  as.data.table(.data) |>
  group_by(Mile, rubbishType) |>
  summarise(total_count = sum(as.numeric(totalNumberOfItemsTagged))) |>
  ungroup()|>
  right_join(grid) |>
  mutate(total_count = ifelse(is.na(total_count), 0, total_count)) |>
  group_by(Mile) |>
  mutate(proportion = total_count/sum(total_count)) |>
  ungroup() |>
  filter(Mile %in% summary[summary$count != 0,]$Mile) |>
  group_by(rubbishType) |>
  summarise(mean_prop = mean(proportion, na.rm = T), min_prop = BootMean(proportion)[1], max_prop = BootMean(proportion)[3])

sum(types_join$mean_prop)

#Need to rearrange
types_join %>%
    dplyr::filter(!is.na(rubbishType)) %>%
  mutate(rubbishType = ifelse(rubbishType == "cloth", "textile", rubbishType)) %>%
  ggplot(aes(x = reorder(rubbishType, mean_prop), y = mean_prop * 100, ymin = min_prop*100 , ymax = max_prop*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(colour="gray") +
  #geom_point(position=position_dodge(width=0.9), aes(y=Percent), color = "gray") + 
  coord_flip() +
  #facet_grid(.~office) +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_d() +
  labs(y = "Mean Percent", x = "Trash Material")

unique(joined$Morphology) |>
  sort()

grid2 = expand.grid(Mile = unique(summary$Mile), Morphology = unique(joined$Morphology))

morphs_join <- joined |>
  as.data.table(.data) |>
  group_by(Mile, Morphology) |>
  summarise(total_count = sum(as.numeric(totalNumberOfItemsTagged))) |>
  ungroup()|>
  right_join(grid2) |>
  mutate(total_count = ifelse(is.na(total_count), 0, total_count)) |>
  group_by(Mile) |>
  mutate(proportion = total_count/sum(total_count)) |>
  ungroup() |>
  filter(Mile %in% summary[summary$count != 0,]$Mile) |>
  group_by(Morphology) |>
  summarise(mean_prop = mean(proportion, na.rm = T), min_prop = BootMean(proportion)[1], max_prop = BootMean(proportion)[3]) %>%
  mutate(Morphology = gsub("s$", "", Morphology, ignore.case = T))

sum(morphs_join$mean_prop)

#Need to rearrange
morphs_join %>%
  dplyr::filter(!is.na(Morphology)) %>%
  filter(mean_prop > 0.01) %>%
  ggplot(aes(x = reorder(Morphology, mean_prop), y = mean_prop * 100, ymin = min_prop*100 , ymax = max_prop*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(colour="gray") +
  #geom_point(position=position_dodge(width=0.9), aes(y=Percent), color = "gray") + 
  coord_flip() +
  #facet_grid(.~office) +
  theme_classic(base_size = 18) + 
  scale_fill_viridis_d() +
  labs(y = "Mean Percent", x = "Trash Morphology")

#Percent that can be branded

table(joined$Brand)

sum(joined$totalNumberOfItemsTagged[joined$Brand != "other"])

sum(joined$totalNumberOfItemsTagged[joined$Brand == "other"])/sum(joined$totalNumberOfItemsTagged)

grid3 = expand.grid(Mile = unique(summary$Mile), Brand = unique(joined$Brand))

brand_join <- joined |>
  as.data.table(.data) |>
  group_by(Mile, Brand) |>
  summarise(total_count = sum(as.numeric(totalNumberOfItemsTagged))) |>
  ungroup()|>
  right_join(grid3) |>
  mutate(total_count = ifelse(is.na(total_count), 0, total_count)) |>
  group_by(Mile) |>
  mutate(proportion = total_count/sum(total_count)) |>
  ungroup() |>
  filter(Mile %in% summary[summary$count != 0,]$Mile) |>
  group_by(Brand) |>
  summarise(mean_prop = mean(proportion), min_prop = BootMean(proportion)[1], max_prop = BootMean(proportion)[3])

sum(brand_join$mean_prop)

#Need to rearrange
brand_join %>%
  dplyr::filter(!is.na(Brand)) %>%
  ggplot(aes(x = reorder(Brand, mean_prop), y = mean_prop * 100, ymin = min_prop*100 , ymax = max_prop*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(colour="gray") +
  #geom_point(position=position_dodge(width=0.9), aes(y=Percent), color = "gray") + 
  coord_flip() +
  #facet_grid(.~office) +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_d() +
  labs(y = "Mean Percent", x = "Trash Brand")

brand_join %>%
  dplyr::filter(!is.na(Brand)) %>%
  #filter(mean_prop > 0.01) %>%
  ggplot(aes(x = reorder(Brand, mean_prop), y = mean_prop * 100, ymin = min_prop*100 , ymax = max_prop*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(colour="gray") +
  #geom_point(position=position_dodge(width=0.9), aes(y=Percent), color = "gray") + 
  coord_flip() +
  #facet_grid(.~office) +
  theme_classic(base_size = 12) + 
  scale_fill_viridis_d() +
  labs(y = "Mean Percent", x = "Trash Brand")


##OSM Spatial Analysis ----
# Load necessary packages
library(osmdata)
library(osmextract)
library(sf)

# Create a buffer of 1 km around each point
#distances <- 2^(3:17)

#test <- logical(nrow(summary))
#distance_to_road <- integer(nrow(summary))

#for(distance in distances[13:length(distances)]){
#  print(distance)
#  buffers_sf <- st_buffer(summary[!test,], dist = distance)
#  osm_results <- lapply(1:nrow(buffers_sf), 
#                           function(x){
#                             bbox <- st_bbox(buffers_sf[x,])
#                             osm_data <- opq(bbox = bbox) %>%
#                               add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")) %>%
#                               osmdata_sf()
#                             osm_data 
#                           })
#  saveRDS(osm_results, paste0("osm_results",distance, ".rds"))
#  roads <- vapply(osm_results, function(x){!is.null(x$osm_lines)}, FUN.VALUE = logical(1))
#  distance_to_road[!test][roads] <- distance
#  test[!test] <- test[!test] | roads
#}

#saveRDS(distance_to_road, paste0("distance_to_road.rds"))
distance_to_road <- readRDS(paste0("distance_to_road.rds"))

summary$road_proximity <- distance_to_road

ggplot(summary, aes(x = road_proximity, y = count)) +
  geom_point() +
  scale_y_log10(breaks = 10^(-2:4)) +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  theme_classic(base_size = 14) +
  ggplot2::coord_fixed() +
  labs(x = "Road Proximity (m)", y = "Trash Count per km")

model <- lm(log10(count)~log10(road_proximity), data = summary)
summary(model) 


## Raw data reading from API ----
# set the API endpoint
bearer_token <- readLines("bearer.txt")

route <- "https://qr.rubbish.love/public/api/v1/reports/Kmi7ClumQbNpa2vh7l9c/get"

# set the authorization header
headers <- c("Authorization" = paste("Bearer", bearer_token))

# add the limit parameter
params <- list(limit = 10000)

# send the API request and retrieve the response
response <- GET(url = route, add_headers(headers), query = params)

# check the status code of the response
status_code <- response$status_code

if (status_code == 200) {
  # extract the data from the response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # explore the data
  str(data)
} else {
  # handle error
  stop("API request failed with status code ", status_code)
}

saveRDS(data, file = "data.rds")

runs_with_data <- unique(data$rubbishRunID)

run_url <- "https://qr.rubbish.love/public/api/v1/runs/Kmi7ClumQbNpa2vh7l9c/get"

#publicApi.get("https://qr.rubbish.love/public/api/v1/runs/Kmi7ClumQbNpa2vh7l9c/get", apiAuth, getTeamRunsApi)

runs <- GET(url = run_url, add_headers(headers))

runs_data <- fromJSON(content(runs, "text"), flatten = TRUE)

saveRDS(runs_data, file = "runs_data.rds")

unique_runs <- unique(runs_data$id)

unique_runs %in% runs_with_data #Check if data exists. 

movement_url <-  paste0("https://qr.rubbish.love/public/api/v1/runs/Kmi7ClumQbNpa2vh7l9c/movement/", unique_runs[[1]])

movements <- GET(url = movement_url, add_headers(headers))

movements_data <- fromJSON(content(movements, "text"), flatten = TRUE)

saveRDS(movements_data, file = "movements_data.rds")

data_joined <- runs_data |> 
  select(id, distance, endLat, endLong, startLat, startLong, totalRubbishRunTime, numberOfItemsTagged, userTimeStampEnd, userName, runDescription) |>
  rename(userName_run = userName) |>
  full_join(data, by = c("id" = "rubbishRunID")) |>
  rename(rubbishRunID = id, id = id.y)


#Test
unique(data$totalNumberOfItemsTagged)
unique(data$userName)

data_cleaned <- data_joined |>
  mutate(reportedTimeStamp = ifelse(is.na(reportedTimeStamp), userTimeStampEnd, reportedTimeStamp)) |>
  mutate(userName = ifelse(is.na(userName), userName_run, userName)) |>
  mutate(lat = ifelse(is.na(lat), startLat, lat)) |>
  mutate(long = ifelse(is.na(long), startLong, long)) |>
  mutate(totalNumberOfItemsTagged = ifelse(is.na(totalNumberOfItemsTagged), numberOfItemsTagged, totalNumberOfItemsTagged)) |>
  distinct(id, photoURL, lat, long, rubbishRunID, userName, rubbishType, comments, reportedTimeStamp, city, totalNumberOfItemsTagged, numberOfItemsTagged, runDescription) |>
  mutate(datetime = as.POSIXct(reportedTimeStamp, origin="1970-01-01", tz = "UTC")) |>
  mutate(date = as.Date(datetime)) |>
  filter(date >= as.Date("2023-03-27")) |>
  filter(!userName %in% c("emin"))

pct <- read_sf("Full_PCT.shp")

start_points = seq(10,2661, by = 10)

all_current <- data.table(Mile = start_points) |>
  filter(Mile <= 2661)

milemarkers <- read.csv("milemarkers.csv") |>
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) |>
  filter(round(Mile, 1) %in% start_points)

Samples_Map <- data_cleaned %>%
  filter(lat !=  "") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) |>
  st_join(milemarkers, st_nearest_feature) 

NTT_Cleanup <- read.csv("No Trace Trails Data Cleanup.csv") |>
  select(id, Morphology, Brand, Survey.Type, Data.Cleaning.Notes)

Samples_Map_with_dist <- Samples_Map |>
  mutate(dist_m = as.numeric(st_distance(Samples_Map, milemarkers[st_nearest_feature(Samples_Map,milemarkers),], by_element=TRUE))) |>
  left_join(NTT_Cleanup)

#Test
unique(Samples_Map_with_dist$totalNumberOfItemsTagged) #Make sure nothing weird here. 
distinct(Samples_Map_with_dist, Mile, rubbishRunID) #Issue several rubbish run ids for some of the points and some locations don't have data. 
filter(Samples_Map_with_dist, dist_m > 5000) #Test if some surveys occur greater than 5 km of a survey location. 

fwrite(Samples_Map_with_dist, "Samples_Map_with_dist.csv")

mapview(Samples_Map_with_dist, zcol = 'reportedTimeStamp', legend = FALSE) #+

## Intersection test

# Create a buffer of 1 km around each point
buffers_sf_10000 <- st_buffer(summary, dist = 10000)

# Apply the function to each buffer and collect results
osm_results_10000 <- lapply(1:nrow(buffers_sf_10000), 
                            function(x){
                              bbox <- st_bbox(buffers_sf_10000[x,])
                              osm_data <- opq(bbox = bbox) %>%
                                add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")) %>%
                                osmdata_sf()
                              osm_data 
                            })

# Create a buffer of 1 km around each point
buffers_sf_1000 <- st_buffer(summary, dist = 1000)

# Apply the function to each buffer and collect results
osm_results_1000 <- lapply(1:nrow(buffers_sf_1000), 
                           function(x){
                             bbox <- st_bbox(buffers_sf_1000[x,])
                             osm_data <- opq(bbox = bbox) %>%
                               add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")) %>%
                               osmdata_sf()
                             osm_data 
                           })

# Create a buffer of 1 km around each point
buffers_sf_100 <- st_buffer(summary, dist = 100)

# Apply the function to each buffer and collect results
osm_results_100 <- lapply(1:nrow(buffers_sf_100), 
                          function(x){
                            bbox <- st_bbox(buffers_sf_100[x,])
                            osm_data <- opq(bbox = bbox) %>%
                              add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")) %>%
                              osmdata_sf()
                            osm_data 
                          })

# Apply the function to each buffer and collect results
osm_results_10 <- lapply(1:nrow(buffers_sf_10), 
                         function(x){
                           bbox <- st_bbox(buffers_sf_10[x,])
                           osm_data <- opq(bbox = bbox) %>%
                             add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")) %>%
                             osmdata_sf()
                           osm_data 
                         })

saveRDS(osm_results_10000, "osm_results_10000.rds")
saveRDS(osm_results_1000, "osm_results_1000.rds")
saveRDS(osm_results_100, "osm_results_100.rds")
saveRDS(osm_results_10, "osm_results_10.rds")

osm_results_10000 <- readRDS("osm_results_10000.rds")
osm_results_1000 <- readRDS("osm_results_1000.rds")
osm_results_100 <- readRDS("osm_results_100.rds")
osm_results_10 <- readRDS("osm_results_10.rds")

summary$road_in_10000 <- vapply(osm_results_10000, function(x){!is.null(x$osm_lines)}, FUN.VALUE = logical(1))
summary$road_in_1000 <- vapply(osm_results_1000, function(x){!is.null(x$osm_lines)}, FUN.VALUE = logical(1))
summary$road_in_100 <- vapply(osm_results_100, function(x){!is.null(x$osm_lines)}, FUN.VALUE = logical(1))
summary$road_in_10 <- vapply(osm_results_10, function(x){!is.null(x$osm_lines)}, FUN.VALUE = logical(1))

