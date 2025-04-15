library(tidyverse)
library(googlesheets4)
library(glue)
library(gt)

### Google Sheets configuration
gs4_auth(cache=".secrets", email="lukesbenz@gmail.com")

### History Sheet
history_url <- 'https://docs.google.com/spreadsheets/d/1teUNQArj8mCKb5Ukctp5b9nFKFX8GHMjDJHbPUNcfV8'

rs_bounds_old <- 
  list('2019' = 21)

### List of Managers
old_managers <- read_sheet(history_url, 'Managers')
df_managers <- 
  old_managers %>% 
  select(season, team_id, manager) %>% 
  bind_rows(map_dfr(2020:2025, ~{
    old_managers %>% 
      select(season, team_id, manager) %>% 
      filter(season == 2019) %>% 
      mutate('season' = .x) %>% 
      mutate('manager' = case_when(season >= 2022 & manager == 'Louis Danowsky' ~ 'Zach Vinik',
                                   season >= 2020 & manager == 'Kent Hubert' ~ 'James Whitty',
                                   T ~ manager))
  })) %>% 
  arrange(-season)

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




summarise_season <- function(season, format = 'old') {
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
                'n_wins' = sum(team_score > opp_score + 0.5 * (team_score == opp_score), na.rm = T),
                'n_loss' = sum(team_score < opp_score + 0.5 * (team_score == opp_score), na.rm = T),
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
             'ferry' = ifelse(rs_finish == 12 & !is.na(df_playoffs$champion) & season >= 2022, 1, 0))
    
    
    
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
                'n_wins' = sum(team_score > opp_score + 0.5 * (team_score == opp_score), na.rm = T),
                'n_loss' = sum(team_score < opp_score + 0.5 * (team_score == opp_score), na.rm = T),
                'n_points' = sum(team_score, na.rm = T),
                'ppg' = mean(team_score, na.rm = T)) %>% 
      ungroup() %>% 
      mutate('ppg_scaled' = ppg - mean(ppg)) %>% 
      arrange(-n_wins, -n_points) %>% 
      mutate('rs_finish' = 1:nrow(.)) %>% 
      arrange(team_id) %>% 
      mutate('points_champ' = ifelse(n_points == max(n_points) & !any(is.na(schedule$home_total_points)), 1, 0))
    
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
             'ferry' = ifelse(rs_finish == 12 & !is.na(df_playoffs$champion) & season >= 2022, 1, 0))
    
    
    
    
  }
  
}


df_start <- read_csv('data/df_start.csv')
old_stats <- map_dfr(2019, summarise_season, 'old')
new_stats <- map_dfr(2020:2025, summarise_season, 'new')

league_summary <- 
  bind_rows(old_stats, new_stats) %>% 
  filter(season >= 2022) %>% 
  left_join(df_managers, by = c('team_id', 'season')) %>% 
  group_by(manager) %>% 
  summarise('seasons' = n(),
            'playoffs' = sum(playoffs),
            'champion' = sum(champion),
            'runner_up' = sum(runner_up),
            'third' = sum(third),
            'points_champ' = sum(points_champ),
            'ferry' = sum(ferry),
            'avg_rs_finish' = mean(rs_finish),
            'n_points' = sum(n_points),
            'ppg' = weighted.mean(ppg, n_wins + n_loss),
            'ppg_scaled' = weighted.mean(ppg_scaled, n_wins + n_loss),
            'n_wins' = sum(n_wins),
            'n_loss' = sum(n_loss)) %>% 
  mutate('win_pct' = n_wins/(n_wins + n_loss))  


gt(league_summary) %>% 
  cols_align('center') %>% 
  fmt_number(avg_rs_finish, decimals = 1) %>% 
  fmt_number(c(n_points, ppg, ppg_scaled), decimals = 1, drop_trailing_zeros = T) %>% 
  fmt_number(win_pct, decimals = 3) %>% 
  tab_spanner(c(playoffs, champion, runner_up, third, points_champ, ferry, avg_rs_finish), label = 'Standings') %>% 
  tab_spanner(c(n_points, ppg, ppg_scaled), label = 'Points') %>% 
  tab_spanner(c(n_wins, n_loss, win_pct), label = 'Record') %>% 
  cols_label(manager = 'Manager',
             seasons = '# Seasons',
             playoffs = 'Playoffs',
             champion = 'Champ',
             runner_up = '2nd',
             third = '3rd',
             points_champ = 'Points Lead',
             ferry = 'Ferry',
             avg_rs_finish = 'Avg. Finish',
             n_points = '# of Points',
             ppg = 'PPG',
             ppg_scaled = 'Scaled PPG',
             n_wins = 'Wins',
             n_loss = 'Loss',
             win_pct = 'Win %') %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c('seasons', 'avg_rs_finish', 'ppg_scaled')
      )
    )
  ) %>% 
  tab_footnote(
    footnote = "Average regular season finish under current seeding rules (wins, w/ points tiebreaker; no divisions)",
    locations = cells_column_labels(avg_rs_finish),
    placement = 'left'
  ) %>% 
  tab_footnote(
    footnote = "PPG relative to league average over relevant seasons, to adjust for different rules/scoring envirnoments",
    locations = cells_column_labels(ppg_scaled),
    placement = 'left'
  ) %>% 
  tab_header(title = md('**Millburnish Fantasy League History**'),
             subtitle = md('**2019-2025**')) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22)





