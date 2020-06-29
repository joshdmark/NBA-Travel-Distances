library(tidyverse)
library(geosphere)
library(data.table)
library(sqldf)

## set wd
setwd("C:/Users/joshua.mark/Downloads")

## load schedule file
schedule <- readxl::read_xlsx('nba_locations.xlsx'
                              ,sheet = 'schedule') %>% data.frame()

## cross join teams to get all distances 
teams1 <- schedule %>% select(home_team_id, home_team_lat, home_team_lon) %>% distinct()
teams2 <- schedule %>% select(visitor_team_id, visitor_team_lat, visitor_team_lon) %>% distinct()
all_combos <- merge(teams1, teams2) %>% 
  data.frame() %>% 
  filter(home_team_id != visitor_team_id)

## rm for space 
rm(teams1, teams2)

distances <- data.frame()
for (i in 1:nrow(all_combos)){
  # print(i)
  select_row <- all_combos[i, ]
  lon1 <- select_row$home_team_lon
  lat1 <- select_row$home_team_lat
  lon2 <- select_row$visitor_team_lon
  lat2 <- select_row$visitor_team_lat
  dist <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  select_row <- select_row %>% cbind(dist_m = dist)
  
  distances <- suppressWarnings(bind_rows(distances, select_row))
}

## add distances to schedule 
schedule <- sqldf("select s.*, d.dist_m
             from schedule s 
             join distances d on s.home_team_id = d.home_team_id 
                  and s.visitor_team_id = d.visitor_team_id") %>% data.frame() %>% 
  mutate(dist_km = dist_m * 0.001, 
         dist_mi = dist_m * 0.000621371)

## individual team 
teams <- schedule$home_team_id %>% unique()
NBA_restart_distances <- data.frame()
for (t in teams){
  # print(team)
  team_schedule <- schedule %>% 
    filter(home_team_id == t | visitor_team_id == t) %>% 
    # mutate(
    transmute(
      ## game info
      game_dt = game_dt, 
      game_time_est = game_time_est,
      ## home_team_ind 
      home_team_ind = ifelse(t == home_team_id, 1, 0), 
      ## team 
      team_id = t, 
      team_name = ifelse(t == home_team_id, home_team_name, visitor_team_name), 
      team = ifelse(t == home_team_id, home_team, visitor_team), 
      team_lat = ifelse(t == home_team_id, home_team_lat, visitor_team_lat), 
      team_lon = ifelse(t == home_team_id, home_team_lon, visitor_team_lon), 
      ## opponent 
      opp_id = ifelse(t == home_team_id, visitor_team_id, home_team_id), 
      opp_name = ifelse(t == home_team_id, visitor_team_name, home_team_name),
      opp = ifelse(t == home_team_id, visitor_team, home_team), 
      opp_lat = ifelse(t == home_team_id, visitor_team_lat, home_team_lat), 
      opp_lon = ifelse(t == home_team_id, visitor_team_lon, home_team_lon), 
      ## dist
      team_travel_m = ifelse(home_team_ind == 1, 0, dist_m),
      team_travel_km = ifelse(home_team_ind == 1, 0, dist_km),
      team_travel_mi = ifelse(home_team_ind == 1, 0, dist_mi)) %>% 
    mutate(game_id = paste0(team_id, '_', opp_id), 
           route_order = ifelse(home_team_ind == 1, 2, 1))
  
  ## add team schedule to NBA_restart_distances 
  NBA_restart_distances <- bind_rows(NBA_restart_distances, team_schedule)
}

## output final file
fwrite(NBA_restart_distances,'NBA_restart_distances.csv')
