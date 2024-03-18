library(tidyverse)
library(glue)
library(here)

build_train_set <- function(season) {
  ### Read in Data set 
  df_schedule <- read_csv(here(glue('data/stats/{season}/schedule_{season}.csv')))
  df_stats <- read_csv(here(glue('data/stats/{season}/daily_stats_{season}.csv')))
  df_teams <- read_csv(here(glue('data/stats/{season}/teams_{season}.csv')))
  df_start <- read_csv(here('data/df_start.csv'))
  df_rp_penalty <- 
    read_csv(here('data/red_flags/rp_penalties.csv')) %>% 
    left_join(df_teams, by = 'team') %>% 
    select(team_id, 'rp_penalty' = penalty, matchup_id, scoring_period_id) %>% 
    mutate_at(vars(everything()), as.numeric)
  df_start <- df_start[df_start$season == season,]
  
  df_features <- 
    df_stats %>%
    filter(in_lineup) %>%
    inner_join(df_start, by = 'matchup_id') %>%
    mutate('day_of_matchup' = scoring_period_id - start_period + 1) %>%
    mutate('days_left' = end_period - scoring_period_id) %>%
    group_by(matchup_id, team_id, day_of_matchup, days_left, start_cap) %>%
    summarise('day_points' = sum(points),
              'start_points' = sum(points[start], na.rm = T),
              'starts' = sum(start),
              'batting_points' = sum(points[batter]),
              'pitching_points' = sum(points[pitcher])) %>%
    inner_join(df_start %>% select(matchup_id, start_period)) %>% 
    mutate('scoring_period_id' = start_period -1 + day_of_matchup) %>% 
    group_by(matchup_id, team_id) %>%
    mutate('total_starts' = cumsum(starts)) %>% 
    mutate('over_start_cap' = total_starts > start_cap & lag(total_starts) <= start_cap ) %>% 
    mutate('penalty' = ifelse(!over_start_cap, 0, sign(start_points) * plyr::round_any((total_starts - start_cap)/starts * abs(start_points), 0.5, ceiling))) %>% 
    left_join(df_rp_penalty, by = c('team_id', 'matchup_id', 'scoring_period_id')) %>%
    mutate('penalty' = ifelse(!is.na(rp_penalty), penalty + rp_penalty, penalty)) %>% 
    mutate('total_points' = cumsum(day_points - penalty),
           'total_batting_points' = cumsum(batting_points),
           'total_pitching_points' = cumsum(pitching_points)) %>% 
    group_by(team_id, matchup_id) %>% 
    group_split() %>% 
    map_dfr(., ~{
      bind_rows(.x, tibble('matchup_id' = .x$matchup_id[1],
                           'team_id' = .x$team_id[1],
                           'day_of_matchup' = 0,
                           'days_left' = .x$days_left[1] + 1,
                           'start_cap' = .x$start_cap[1]))
    }) %>% 
    ungroup() %>% 
    mutate_if(is.numeric, ~replace(.x, is.na(.x), 0)) %>% 
    arrange(matchup_id, team_id, day_of_matchup) %>% 
    select(-penalty) %>% 
    mutate('over_start_cap' = total_starts >= start_cap,
           'penalty' = total_points - total_batting_points - total_pitching_points) %>% 
    mutate('starts_left' = pmax(0, start_cap - total_starts))
  
  
  ### Add in Pre-Matchup Day
  df_lag <- 
    df_features %>% 
    group_by(matchup_id) %>% 
    filter(days_left == min(days_left)) %>% 
    group_by(team_id) %>% 
    mutate('start_rate' = map_dbl(total_starts/start_cap, ~min(1, .x))) %>% 
    arrange(matchup_id) %>% 
    mutate('prev_points_per_day' = lag(total_points/day_of_matchup),
           'points_per_day' = lag(cummean(total_points/day_of_matchup)),
           'bat_points_per_day' = lag(cummean(total_batting_points/day_of_matchup)),
           'pitch_points_per_day' = lag(cummean(total_pitching_points/day_of_matchup)),
           'max_start_rate' = lag(cummean(start_rate)) ) %>% 
    select(team_id, matchup_id, contains('per_day'), contains('max_start_rate')) %>% 
    ungroup()
  
  df_features <-
    df_features %>% 
    inner_join(df_lag, by = c("matchup_id", "team_id")) 
  
  ### Win Results
  df_results <- 
    df_schedule %>% 
    mutate('win' = as.numeric(home_total_points > away_total_points)) %>% 
    select(matchup_id, home_team_id, away_team_id, win)
  
  
  df_train <-
    df_results %>% 
    inner_join(df_features, by = c('matchup_id', 'home_team_id' = 'team_id')) %>% 
    inner_join(df_features, by = c('matchup_id', 'day_of_matchup', 'days_left', 'start_cap',
                                   'away_team_id' = 'team_id'), suffix = c('_home', '_away')) %>% 
    mutate('score_diff' = total_points_home - total_points_away,
           'start_diff' = total_starts_home - total_starts_away) %>% 
    mutate('matchup_over' = 
             (total_points_home > total_points_away) * (days_left == 0) - 
             (total_points_home < total_points_away) * (days_left == 0) ) %>% 
    mutate('score_days_ratio_home' = abs(pmin(0, score_diff/(days_left + 0.001))),
           'score_days_ratio_away' = abs(pmax(0, score_diff/(days_left + 0.001)))) %>% 
    mutate('score_starts_ratio_home' = abs(pmin(0, score_diff/(starts_left_home + 0.001))),
           'score_starts_ratio_away' = abs(pmax(0, score_diff/(starts_left_away + 0.001)))) %>% 
    mutate('score_days_ratio' = score_days_ratio_home - score_days_ratio_away,
           'score_starts_ratio' = score_starts_ratio_home - score_starts_ratio_away) %>% 
    mutate('start_advantage' = (starts_left_home - starts_left_away)) %>% 
    mutate('start_advantage_ratio' = (starts_left_home - starts_left_away)/(starts_left_home + starts_left_away + 1),
           'pitch_spread_per_start' = (pitch_points_per_day_home - pitch_points_per_day_away) * exp(starts_left_home + starts_left_away - 2 * start_cap),
           'points_per_bat_spread' = (bat_points_per_day_home - bat_points_per_day_away) * matchup_id/20 * exp(-(7 - pmin(days_left, 7))),
           'points_per_day_spread' = points_per_day_home - points_per_day_away)
  
  
  
  df_train$points_per_bat_spread[df_train$days_left <= 4] <- 0
  df_train$points_per_day_spread[df_train$days_left <= 4] <- 0
  
  return(df_train)
}
