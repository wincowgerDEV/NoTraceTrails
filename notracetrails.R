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

fwrite(Samples_Map_with_dist, "G:/My Drive/MooreInstitute/Projects/NoTraceTrails/Samples_Map_with_dist.csv")

mapview(Samples_Map_with_dist, zcol = 'reportedTimeStamp', legend = FALSE) #+

  
#Summary stats ----
## Concentrations ----
summary <- Samples_Map_with_dist |>
  group_by(Mile) |>
  summarise(count = sum(as.numeric(totalNumberOfItemsTagged))) |>
  as.data.frame(.data) |>
  select(-geometry) |>
  right_join(milemarkers, by = "Mile") |>
  st_sf(sf_column_name = "geometry") |>
  filter(Mile <= 2661) |>
  mutate(count =  ifelse(is.na(count), 0, count)) %>%
  mutate(count_group = cut(count, breaks = c(-1,0, 1,10,200), labels = c("0", "1", "2-10", "11-103")))

ggplot(summary, aes(x = Mile, y = count)) +
    geom_bar(width = 0.6, stat = "identity", color = "black") +
    theme_classic(base_size = 15) +
    labs(y = "Count")

BootMean(summary$count)

mean(summary$count)

class(summary)

#mapview(pct, legend = FALSE, color = "gray") +
mapview(summary, zcol = 'count_group', stroke = NA, cex = 2, alpha.regions = 0.75, legend = TRUE) 

fwrite(summary, "G:/My Drive/MooreInstitute/Projects/NoTraceTrails/summary.csv")

## Trash Types ----

#Change the unique mile, it filters later. 
grid = expand.grid(Mile = unique(summary$Mile), rubbishType = unique(data_cleaned$rubbishType))

types_join <- Samples_Map |>
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
  summarise(mean_prop = mean(proportion), min_prop = BootMean(proportion)[1], max_prop = BootMean(proportion)[3])

sum(types_join$mean_prop)

#Need to rearrange
types_join %>%
    dplyr::filter(!is.na(rubbishType)) %>%
  ggplot(aes(x = reorder(rubbishType, mean_prop), y = mean_prop * 100, ymin = min_prop*100 , ymax = max_prop*100)) +
  geom_bar(stat="identity") +
  geom_errorbar(colour="gray") +
  #geom_point(position=position_dodge(width=0.9), aes(y=Percent), color = "gray") + 
  coord_flip() +
  #facet_grid(.~office) +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_d() +
  labs(y = "Mean Percent", x = "Trash Type")
