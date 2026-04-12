### Originally written by Zach Vinik

library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(lubridate)
library(purrr)
library(readr)
library(glue)

list_of_game_ids <- function(date_) {
  games <- 
    GET(paste0('http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&gameType=R&date=', date_)) %>%
    content(as = 'text') %>%
    fromJSON(flatten = T)
  
  all_games_date <- 
    games$dates$games[[1]] %>%
    as_tibble() %>% 
    filter(status.statusCode %in% c('F','FR')) %>% 
    pull(gamePk)
  
  
  return(all_games_date)
  
}


game_data <- function(game_id, date_id) {
  resp_box <- 
    GET(paste0('http://statsapi.mlb.com/api/v1/game/', game_id, '/boxscore')) %>%
    content(as = 'text') %>%
    fromJSON()
  
  returnDataAway <- 
    map_dfr(resp_box$teams$away$batters, function(x) {
      playerData <- resp_box$teams$away$players[[paste0('ID', x)]]
      tibble(
        game_id = game_id,
        date = date_id,
        mlbam_id = x,
        player = playerData$person$fullName,
        position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        starter = str_sub(playerData$battingOrder, 2, 3) == "00",
        home_runs = playerData$stats$batting$homeRuns,
        h_1b = playerData$stats$batting$hits - sum(playerData$stats$batting$homeRuns, playerData$stats$batting$doubles, playerData$stats$batting$triples),
        h_2b = playerData$stats$batting$doubles,
        h_3b = playerData$stats$batting$triples,
        b_ab = playerData$stats$batting$atBats,
        b_pa = playerData$stats$batting$plateAppearances,
        b_walks = playerData$stats$batting$baseOnBalls,
        b_hpb =  playerData$stats$batting$hitByPitch,
        b_ibb =  playerData$stats$batting$intentionalWalks,
        b_sf =  playerData$stats$batting$sacFlies,
        b_runs = playerData$stats$batting$runs,
        b_rbi = playerData$stats$batting$rbi,
        b_k = playerData$stats$batting$strikeOuts,
        b_gidp = playerData$stats$batting$groundIntoDoublePlay + playerData$stats$batting$groundIntoTriplePlay,
        b_E = playerData$stats$fielding$errors,
        b_SB = playerData$stats$batting$stolenBases,
        b_CS = playerData$stats$batting$caughtStealing,
        b_OFAS = 0 #the API just has assists, and there doesn't seem to be an easy 100% way to get this accurate if a player moved from infield to outfield mid game
      )
    }) %>% 
    filter(!is.na(player))
  
  returnDataHome <- 
    map_dfr(resp_box$teams$home$batters, function(x) {
      playerData <- resp_box$teams$home$players[[paste0('ID', x)]]
      tibble(
        game_id = game_id,
        date = date_id,
        mlbam_id = x,
        player = playerData$person$fullName,
        position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        starter = str_sub(playerData$battingOrder, 2, 3) == "00",
        home_runs = playerData$stats$batting$homeRuns,
        h_1b = playerData$stats$batting$hits - sum(playerData$stats$batting$homeRuns, playerData$stats$batting$doubles, playerData$stats$batting$triples),
        h_2b = playerData$stats$batting$doubles,
        h_3b = playerData$stats$batting$triples,
        b_ab = playerData$stats$batting$atBats,
        b_pa = playerData$stats$batting$plateAppearances,
        b_walks = playerData$stats$batting$baseOnBalls,
        b_hpb =  playerData$stats$batting$hitByPitch,
        b_ibb =  playerData$stats$batting$intentionalWalks,
        b_sf =  playerData$stats$batting$sacFlies,
        b_runs = playerData$stats$batting$runs,
        b_rbi = playerData$stats$batting$rbi,
        b_k = playerData$stats$batting$strikeOuts,
        b_gidp = playerData$stats$batting$groundIntoDoublePlay + playerData$stats$batting$groundIntoTriplePlay,
        b_E = playerData$stats$fielding$errors,
        b_SB = playerData$stats$batting$stolenBases,
        b_CS = playerData$stats$batting$caughtStealing,
        b_OFAS = 0 #the API just has assists, and there doesn't seem to be an easy 100% way to get this accurate if a player moved from infield to outfield mid game
      )
    }) %>% 
    filter(!is.na(player))
  
  
  total_game_data <- 
    bind_rows(returnDataHome, returnDataAway) %>% 
    mutate('b_CYCL' = ifelse(h_1b > 0 & h_2b > 0 & h_3b > 0 & home_runs > 0, 1, 0),
           'points' = 
             h_1b * 1 + h_2b * 2 + h_3b * 3 + home_runs * 4 + 
             b_walks * 1 + b_hpb * 0.5 + b_ibb * 0.5 + 
             b_rbi * 1 + b_runs * 1 + b_k * (-0.5) + b_SB * 1 + b_CS * (-1) + b_gidp * (-1) +
             b_CYCL*10 + b_E*(-1))
  
  return(total_game_data)
  
}

pitcher_game_data <- function(game_id, date_id) {
  
  resp_box <- 
    GET(paste0('http://statsapi.mlb.com/api/v1/game/', game_id, '/boxscore')) %>%
    content(as = 'text') %>%
    fromJSON()
  
  PitcherDataAway <- 
    map_dfr(resp_box$teams$away$pitchers, function(x) {
      playerData <- resp_box$teams$away$players[[paste0('ID', x)]]
      tibble(
        game_id = game_id,
        date = date_id,
        mlbam_id = x,
        player = playerData$person$fullName,
        position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        starter = playerData$stats$pitching$gamesStarted == 1,
        outs = playerData$stats$pitching$outs,
        hits = playerData$stats$pitching$hits,
        earnedruns = playerData$stats$pitching$earnedRuns,
        walks = playerData$stats$pitching$baseOnBalls + playerData$stats$pitching$intentionalWalks,
        hbp = playerData$stats$pitching$hits,
        strikeouts = playerData$stats$pitching$strikeOuts,
        wildPitches = playerData$stats$pitching$wildPitches,
        balks = playerData$stats$pitching$balks,
        pickoffs = playerData$stats$pitching$pickoffs,
        #qualityStarts will be below
        #completeGames will be below
        shutouts = playerData$stats$pitching$shutouts,
        #noHitters will be below
        #perfectGames will be below
        saves = playerData$stats$pitching$saves,
        holds = playerData$stats$pitching$holds,
        blownSaves = playerData$stats$pitching$blownSaves
      )
    }) %>% 
    filter(!is.na(player))
  
  PitcherDataHome <- 
    map_dfr(resp_box$teams$home$pitchers, function(x) {
      playerData <- resp_box$teams$home$players[[paste0('ID', x)]]
      tibble(
        game_id = game_id,
        date = date_id,
        mlbam_id = x,
        player = playerData$person$fullName,
        position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        starter = playerData$stats$pitching$gamesStarted == 1,
        outs = playerData$stats$pitching$outs,
        hits = playerData$stats$pitching$hits,
        earnedruns = playerData$stats$pitching$earnedRuns,
        walks = playerData$stats$pitching$baseOnBalls + playerData$stats$pitching$intentionalWalks,
        hbp = playerData$stats$pitching$hits,
        strikeouts = playerData$stats$pitching$strikeOuts,
        wildPitches = playerData$stats$pitching$wildPitches,
        balks = playerData$stats$pitching$balks,
        pickoffs = playerData$stats$pitching$pickoffs,
        #qualityStarts will be below
        #completeGames will be below
        shutouts = playerData$stats$pitching$shutouts,
        #noHitters will be below
        #perfectGames will be below
        saves = playerData$stats$pitching$saves,
        holds = playerData$stats$pitching$holds,
        blownSaves = playerData$stats$pitching$blownSaves
      )
    }) %>% 
    filter(!is.na(player))
  
  total_pitcher_data <- 
    bind_rows(PitcherDataAway, PitcherDataHome) %>% 
    mutate('qualityStarts' = ifelse(outs >=18 & earnedruns <= 3, 1, 0),
           'completeGames' = completeGames := ifelse(outs >= 15 & outs == sum(outs), 1, 0),
           'noHitters' = ifelse(outs >= 21 & completeGames == 1 & hits == 0, 1, 0),
           'perfectGames' = ifelse(outs >= 21 & completeGames == 1 & hits == 0 & walks == 0 & hbp == 0, 1, 0),
           'points' = outs * 1 + hits * (-1) + earnedruns * (-2) + walks * (-0.5) + hbp * (-0.5) +
             strikeouts * 1 + wildPitches * (-0.5) + balks * (-1) + pickoffs * 1 + qualityStarts * 5 +
             completeGames * 5 + shutouts * 5 + noHitters * 18 + perfectGames * 27+
             saves * 3 +holds * 2 + blownSaves * (-2))
  
  return(total_pitcher_data)
}
