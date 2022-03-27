library(tidyverse)
source('functions.R')

# ------------------------------------------------------------------------------

season_list <- c("2016-17", "2017-18", "2018-19")

# ------------------------------------------------------------------------------

player_data <- map_dfr(season_list, players_per_season)

# ------------------------------------------------------------------------------

shot_data <-
  map2_dfr(player_data$player_id[1:41],
           player_data$season[1:41],
           shot_info_per_player_per_season) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  na.omit()

# ------------------------------------------------------------------------------

player_data <- 
  player_data %>% 
  mutate(across(where(is.character), as.factor))

# ------------------------------------------------------------------------------

save(player_data, file = "players.RData", compress = "xz", version = 2)
save(shot_data, file = "shotss.RData", compress = "xz", version = 2)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

# ------------------------------------------------------------------------------

q("no")
