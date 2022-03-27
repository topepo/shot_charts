
players_per_season <- function(season, pause = rpois(1, 4) + 1) {
  require(tidyverse, quietly = TRUE)
  require(httr, quietly = TRUE)
  Sys.sleep(pause)
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  params = list(
    `College` = '',
    `Conference` = '',
    `Country` = '',
    `DateFrom` = '',
    `DateTo` = '',
    `DistanceRange` = 'By Zone',
    `Division` = '',
    `DraftPick` = '',
    `DraftYear` = '',
    `GameScope` = '',
    `GameSegment` = '',
    `Height` = '',
    `LastNGames` = '0',
    `LeagueID` = '00',
    `Location` = '',
    `MeasureType` = 'Base',
    `Month` = '0',
    `OpponentTeamID` = '0',
    `Outcome` = '',
    `PORound` = '0',
    `PaceAdjust` = 'N',
    `PerMode` = 'Totals',
    `Period` = '0',
    `PlayerExperience` = '',
    `PlayerPosition` = '',
    `PlusMinus` = 'N',
    `Rank` = 'N',
    `Season` = season,
    `SeasonSegment` = '',
    `SeasonType` = 'Regular Season',
    `ShotClockRange` = '',
    `StarterBench` = '',
    `TeamID` = '0',
    `VsConference` = '',
    `VsDivision` = '',
    `Weight` = ''
  )
  
  res <-
    httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayershotlocations',
              httr::add_headers(.headers = headers),
              query = params)
  
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  player_data <- json_resp$resultSets$rowSet[, 1:4]
  colnames(player_data) <-
    c("player_id", "player_name", "team_id", "team_abbr")
  player_data <-
    as_tibble(player_data) %>%
    distinct() %>%
    mutate(season = season) %>% 
    na.omit()
}

no_shots <- 
  structure(
    list(
      player_id = character(0), season = character(0),
      game_id = character(0), game_event_id = character(0), team_id = character(0),
      team_name = character(0), team_home = character(0), team_away = character(0),
      period = numeric(0), minutes_remaining = character(0), seconds_remaining = character(0),
      action_type = character(0), shot_type = character(0), shot_zone_basic = character(0),
      shot_zone_area = character(0), shot_distance = character(0),
      shot_made = character(0), loc_x = character(0), loc_y = character(0)
      ), 
    row.names = integer(0), 
    class = c("tbl_df", "tbl", "data.frame")
  )

shot_info_per_player_per_season <- function(id, season, pause = rpois(1, 4) + 1) {
  require(tidyverse, quietly = TRUE)
  require(httr, quietly = TRUE)
  require(janitor, quietly = TRUE)
  require(glue, quietly = TRUE)
  
  # cat(cli::rule(id), "\n")
  Sys.sleep(pause)
  pb$tick()
  shot_url <- 
    glue::glue(
      'https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=33&CFPARAMS={season}&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID={id}&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=0&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID='
    )
  
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  res <- httr::GET(url = shot_url, httr::add_headers(.headers = headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  
  if(identical(list(), json_resp$resultSets$rowSet[[1]])) {
    return(no_shots)
  }
  
  dat <- json_resp$resultSets$rowSet[[1]]
  colnames(dat) <- json_resp[["resultSets"]][["headers"]][[1]]
  dat <- 
    as_tibble(dat) %>% 
    mutate(season = season) %>% 
    clean_names() %>% 
    rename(
      team_home = htm,
      team_away = vtm,
      shot_made = shot_made_flag
    ) %>% 
    select(
      player_id, season,
      game_id, game_event_id,
      team_id, team_home, team_away,
      period, minutes_remaining, seconds_remaining,
      action_type, 
      shot_type, shot_zone_basic, shot_zone_area, shot_distance, shot_made,
      loc_x, loc_y
    ) %>% 
    mutate(
      across(
        c(period, minutes_remaining, seconds_remaining, shot_distance, shot_made,
          loc_x, loc_y),
        .fns = ~ as.numeric(.x)
      )
    )
  
  dat
}


