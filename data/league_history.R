library(tidyverse)
library(googlesheets4)
library(glue)
library(gt)



### Processing Functions
convert_schedule <- function(df, format = 'old') {
  if(format == 'old') {
    bind_rows(
      df %>% 
        select(season, 
               matchup_id, 
               'team' = home, 
               'team_score' = home_score,
               'opp' = away, 
               'opp_score' = away_score),
      
      df %>% 
        select(season, 
               matchup_id, 
               'team' = away, 
               'team_score' = away_score,
               'opp' = home, 
               'opp_score' = home_score)
    )
  } else {
    bind_rows(
      df %>% 
        select(season, 
               matchup_id, 
               'team' = home_team_id, 
               'team_score' = home_total_points,
               'opp' = away_team_id, 
               'opp_score' = away_total_points),
      
      df %>% 
        select(season, 
               matchup_id, 
               'team' = away_team_id, 
               'team_score' = away_total_points,
               'opp' = home_team_id, 
               'opp_score' = home_total_points)
    )
    
  }
  
}




summarise_season <- function(season, format, history_url, rs_bounds_old) {
  df_start <- read_csv('data/df_start.csv')
  old_managers <- read_sheet(history_url, 'Managers')
  
  if(format == 'new') {
    teams <- read_csv(glue('data/stats/{season}/teams_{season}.csv'))
    schedule <- read_csv(glue('data/stats/{season}/schedule_{season}.csv'))
    
    season_stats <- 
      schedule %>% 
      filter(matchup_id <= max(df_start$matchup_id[df_start$season == season & !df_start$playoffs])) %>% 
      mutate('season' = season) %>% 
      convert_schedule('new') %>% 
      group_by(team_id = team) %>% 
      summarise('season' = season[1],
                'n_wins' = sum(as.numeric(team_score > opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
                'n_loss' = sum(as.numeric(team_score < opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
                'n_points' = sum(team_score, na.rm = T),
                'ppg' = mean(team_score, na.rm = T)) %>% 
      ungroup() %>% 
      mutate('ppg_scaled' = ppg - mean(ppg)) %>% 
      arrange(-n_wins, -n_points) %>% 
      mutate('rs_finish' = 1:nrow(.)) %>% 
      arrange(team_id) %>% 
      mutate('points_champ' = ifelse(n_points == max(n_points)  & !any(is.na(schedule$home_total_points)), 1, 0))
    
    ### Playoffs
    df_playoffs <- 
      schedule %>% 
      mutate('season' = season) %>% 
      filter(matchup_id == max(matchup_id)) %>% 
      head(2) %>% 
      convert_schedule('new') %>% 
      mutate('game_id' = c('champ', 'third', 'champ', 'third')) %>% 
      summarise('champion' = team[team_score > opp_score & game_id == 'champ'],
                'runner_up' = team[team_score < opp_score & game_id == 'champ'],
                'third' = team[team_score > opp_score & game_id == 'third'],
                'fourth' = team[team_score < opp_score & game_id == 'third'])
    
    season_stats <- 
      season_stats %>% 
      mutate('champion' = ifelse(team_id == df_playoffs$champion & !is.na(df_playoffs$champion), 1, 0),
             'runner_up' = ifelse(team_id == df_playoffs$runner_up & !is.na(df_playoffs$champion), 1, 0),
             'third' = ifelse(team_id == df_playoffs$third & !is.na(df_playoffs$champion), 1, 0),
             'playoffs' = champion + runner_up + third + ifelse(team_id == df_playoffs$fourth & !is.na(df_playoffs$champion), 1, 0),
             'ferry' = ifelse(rs_finish == 12 & !is.na(df_playoffs$champion) & season >= 2022, 1, 0)) %>% 
      mutate('rs_finish' = ifelse(champion == 1, 1, rs_finish),
             'rs_finish' = ifelse(runner_up == 1, 2, rs_finish),
             'rs_finish' = ifelse(third == 1, 3, rs_finish),
             'rs_finish' = ifelse(third == 0 & champion == 0 & runner_up == 0 & playoffs == 1, 4, rs_finish))
    
    
    
  } else {
    schedule <- read_sheet(history_url, sheet = as.character(season))
    
    ### Basic Season Stats
    season_stats <- 
      schedule %>% 
      filter(matchup_id <= rs_bounds_old[[as.character(season[1])]]) %>%
      convert_schedule('old') %>% 
      left_join(old_managers, by = c('team', 'season')) %>% 
      group_by(team_id) %>% 
      summarise('season' = season[1],
                'n_wins' = sum(as.numeric(team_score > opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
                'n_loss' = sum(as.numeric(team_score < opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
                'n_points' = sum(team_score, na.rm = T),
                'ppg' = mean(team_score, na.rm = T)) %>% 
      mutate('ppg' = replace(ppg, is.na(ppg), 0)) %>% 
      ungroup() %>% 
      mutate('ppg_scaled' = ppg - mean(ppg)) %>% 
      arrange(-n_wins, -n_points) %>% 
      mutate('rs_finish' = 1:nrow(.)) %>% 
      arrange(team_id) %>% 
      mutate('points_champ' = ifelse(n_points == max(n_points) & !any(is.na(schedule$home_score)), 1, 0))
    
    ### Playoffs
    df_playoffs <- 
      schedule %>% 
      filter(matchup_id == max(matchup_id)) %>% 
      head(2) %>% 
      convert_schedule('old') %>% 
      mutate('game_id' = c('champ', 'third', 'champ', 'third')) %>% 
      left_join(old_managers, by = c('team', 'season')) %>% 
      summarise('champion' = team_id[team_score > opp_score & game_id == 'champ'],
                'runner_up' = team_id[team_score < opp_score & game_id == 'champ'],
                'third' = team_id[team_score > opp_score & game_id == 'third'],
                'fourth' = team_id[team_score < opp_score & game_id == 'third'])
    
    season_stats <- 
      season_stats %>% 
      mutate('champion' = ifelse(team_id == df_playoffs$champion & !is.na(df_playoffs$champion), 1, 0),
             'runner_up' = ifelse(team_id == df_playoffs$runner_up & !is.na(df_playoffs$champion), 1, 0),
             'third' = ifelse(team_id == df_playoffs$third & !is.na(df_playoffs$champion), 1, 0),
             'playoffs' = champion + runner_up + third + ifelse(team_id == df_playoffs$fourth & !is.na(df_playoffs$champion), 1, 0),
             'ferry' = ifelse(rs_finish == 12 & !is.na(df_playoffs$champion) & season >= 2022, 1, 0)) %>% 
      mutate('rs_finish' = ifelse(champion == 1, 1, rs_finish),
             'rs_finish' = ifelse(runner_up == 1, 2, rs_finish),
             'rs_finish' = ifelse(third == 1, 3, rs_finish),
             'rs_finish' = ifelse(third == 0 & champion == 0 & runner_up == 0 & playoffs == 1, 4, rs_finish))
    
    
    
    
  }
  
}

get_schedule <- function(season, format, history_url, rs_bounds_old) {
  df_start <- read_csv('data/df_start.csv')
  old_managers <- read_sheet(history_url, 'Managers')
  
  if(format == 'old') {
    schedule <- read_sheet(history_url, sheet = as.character(season))
    schedule <- 
      schedule %>% 
      group_by(matchup_id) %>% 
      mutate('game_id' = 1:n()) %>% 
      ungroup() %>% 
      filter(matchup_id <= rs_bounds_old[[as.character(season[1])]] | 
               (matchup_id > rs_bounds_old[[as.character(season[1])]]  & game_id <= 2 )) %>%
      convert_schedule('old') %>% 
      left_join(old_managers, by = c('team', 'season')) %>% 
      left_join(old_managers %>% rename('opp' = team, 'opp_id' = team_id) %>% select(-manager), by = c('opp', 'season')) %>% 
      select(season, matchup_id, team_id, team_score, opp_id, opp_score)
    
  } else if(format == 'new') {
    schedule <- read_csv(glue('data/stats/{season}/schedule_{season}.csv'))
    
    schedule <- 
      schedule %>% 
      group_by(matchup_id) %>% 
      mutate('game_id' = 1:n()) %>% 
      filter(matchup_id <= max(df_start$matchup_id[df_start$season == season & !df_start$playoffs]) | 
               (matchup_id > max(df_start$matchup_id[df_start$season == season & !df_start$playoffs]) & game_id <= 2)) %>% 
      select(-game_id) %>% 
      mutate('season' = season) %>% 
      convert_schedule('new') %>% 
      rename('team_id' = team, 
             'opp_id' = opp)
  }
  
  return(schedule) 
}


update_league_history <- function(season_max) {
  ### History Sheet
  history_url <- 'https://docs.google.com/spreadsheets/d/1teUNQArj8mCKb5Ukctp5b9nFKFX8GHMjDJHbPUNcfV8'
  
  rs_bounds_old <- 
    list('2019' = 21,
         '2018' = 21,
         '2017' = 21,
         '2016' = 21,
         '2015' = 21)
  
  ### List of Managers
  old_managers <- read_sheet(history_url, 'Managers')
  df_managers <- 
    old_managers %>% 
    select(season, team_id, manager) %>% 
    bind_rows(map_dfr(2020:season_max, ~{
      old_managers %>% 
        select(season, team_id, manager) %>% 
        filter(season == 2019) %>% 
        mutate('season' = .x) %>% 
        mutate('manager' = case_when(season >= 2022 & manager == 'Louis Danowsky' ~ 'Zach Vinik',
                                     season >= 2020 & manager == 'Kent Hubert' ~ 'James Whitty',
                                     T ~ manager))
    })) %>% 
    arrange(-season)
  
  
  
  old_stats <- map_dfr(2015:2019, summarise_season, 'old', history_url, rs_bounds_old)
  new_stats <- map_dfr(2020:season_max, summarise_season, 'new', history_url, rs_bounds_old)
  
  league_history <- bind_rows(old_stats, new_stats) 
  write_csv(league_history, 'data/stats/league_history.csv')
  write_csv(df_managers, 'data/stats/manager_history.csv')
  
}

update_win_loss_matrix <- function(season_max) {
  rs_bounds_old <- 
    list('2019' = 21,
         '2018' = 21,
         '2017' = 21,
         '2016' = 21,
         '2015' = 21)
  
  df_managers <- read_csv('data/stats/manager_history.csv')
  history_url <- 'https://docs.google.com/spreadsheets/d/1teUNQArj8mCKb5Ukctp5b9nFKFX8GHMjDJHbPUNcfV8'
  old_stats <- map_dfr(2015:2019, get_schedule, 'old', history_url, rs_bounds_old)
  new_stats <- map_dfr(2020:season_max, get_schedule, 'new', history_url, rs_bounds_old)
  
  df_results <- 
    bind_rows(old_stats, new_stats) %>% 
    left_join(df_managers, by = c('team_id', 'season')) %>% 
    left_join(df_managers, by = c('opp_id' = 'team_id', 'season'), suffix = c('', '_opp'))
  
  
  win_mat <- 
    df_results %>% 
    group_by(manager, manager_opp) %>% 
    summarise(
      'n_wins' = sum(as.numeric(team_score > opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
      'n_loss' = sum(as.numeric(team_score < opp_score) + 0.5 * as.numeric(team_score == opp_score), na.rm = T),
      'win_pct' = n_wins/(n_wins + n_loss)
    ) 
  
  write_csv(win_mat, 'data/stats/wl_history.csv')
  
}
