library(jsonlite)
library(tidyverse)
library(gt)
library(furrr)
library(glue)
library(lubridate)
library(here)
library(patchwork)
library(truncnorm)
library(fs)
library(baseballr)


plan(multisession(workers = min(parallel::detectCores(), 12)))
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2000)

source('helpers.R')
source('data/daily_stats.R')
source('data/trades.R')
source('data/free_agents.R')
source('figures/wp_graphics.R')
source('figures/all_star_teams.R')

params <- 
  list('season' = 2024,
       'opening_day' = as.Date('2024-03-20'),
       'nsims' = 10000)

df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season == params$season) 

period <- min(max(df_start$end_period), max(1, as.numeric(Sys.Date() - params$opening_day) + 1))

params$matchup_id <- max(df_start$matchup_id[df_start$start_period <= period])

reg_season <- max(df_start$matchup_id[!df_start$playoffs])
n_games <- max(params$matchup_id, reg_season)
n_match <- n_games * 6

### Get data
y <- robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{params$season}/segments/0/leagues/49106?view=mTeam'))

### Teams
teams <- jsonlite::flatten(y$teams)
teams <- 
  select(teams, 
         'team_id' = id,
         'division_id' = divisionId,
         'team' = name,
         logo,
         'owners' = owners,
         'abbreviation' = abbrev,
         'wins' = record.overall.wins,
         'losses' = record.overall.losses,
         'ties' = record.overall.ties) %>% 
  mutate('owners' = map_chr(teams$owners, ~paste0(.x, collapse = ', '))) %>% 
  mutate('team' = stripwhite(team)) %>% 
  select(team_id, division_id, team, everything())

teams$logo[which(teams$team == "The Traveling Secretaries")] <- 'https://i.imgur.com/wll1Ubw.jpg'
teams$logo[which(teams$team == 'Mt. Everest Taquito Farmers')] <-  'https://i.imgur.com/VW41hvO.gif'
write_csv(teams, glue('data/stats/{params$season}/teams_{params$season}.csv'))

### Schedule and Batting + Pitching Points
home_schedule <- NULL
away_schedule <- NULL
schedule <- NULL
batter_points <- sp_points <- rp_points <-  NULL

for(i in 1:max(reg_season, params$matchup_id)) {
  sp_id <- df_start$end_period[i]
  x <- robust_scrape(glue("https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{params$season}/segments/0/leagues/49106?scoringPeriodId={sp_id}&view=mBoxscore"))
  
  
  schedule_ <- x$schedule
  home <- schedule_$home %>% dplyr::slice(1:n_match) 
  away <- schedule_$away %>% dplyr::slice(1:n_match)
  
  schedule_ <- select(schedule_,
                      'matchup_id' = matchupPeriodId,
                      'game_id' = id) %>% dplyr::slice(1:n_match)
  
  home_schedule_ <- 
    select(home, 
           'home_team_id' = teamId,
           'home_total_points' = totalPoints) %>% 
    mutate("home_batting_points" = map_dbl(home$rosterForMatchupPeriod$entries, get_batting_points)) %>% 
    mutate("home_pitching_points" = map_dbl(home$rosterForMatchupPeriod$entries, get_pitching_points)) %>% 
    mutate('home_pitching_points' = ifelse(home_total_points == 0, home_pitching_points, home_total_points - home_batting_points))
  
  away_schedule_ <- 
    select(away, 
           'away_team_id' = teamId,
           'away_total_points' = totalPoints) %>% 
    mutate("away_batting_points" = map_dbl(away$rosterForMatchupPeriod$entries, get_batting_points)) %>% 
    mutate("away_pitching_points" = map_dbl(away$rosterForMatchupPeriod$entries, get_pitching_points)) %>% 
    mutate('away_pitching_points' = ifelse(away_total_points == 0, away_pitching_points, away_total_points - away_batting_points))
  
  
  batter_points_ <- 
    map2_dfr(home$rosterForMatchupPeriod$entries, 1:n_match, batting_points_by_game) %>% 
    mutate('team_id' = home$teamId[team_ix]) %>% 
    bind_rows(
      map2_dfr(away$rosterForMatchupPeriod$entries, 1:n_match, batting_points_by_game) %>% 
        mutate('team_id' = away$teamId[team_ix])
    )
  batter_points <- bind_rows(batter_points, batter_points_)
  
  
  rp_points_ <- 
    map2_dfr(home$rosterForMatchupPeriod$entries, 1:n_match, rp_points_by_game) %>% 
    mutate('team_id' = home$teamId[team_ix]) %>% 
    bind_rows(
      map2_dfr(away$rosterForMatchupPeriod$entries, 1:n_match, rp_points_by_game) %>% 
        mutate('team_id' = away$teamId[team_ix])
    )
  rp_points <- bind_rows(rp_points, rp_points_)
  
  
  sp_points_ <- 
    map2_dfr(home$rosterForMatchupPeriod$entries, 1:n_match, sp_points_by_game) %>% 
    mutate('team_id' = home$teamId[team_ix]) %>% 
    bind_rows(
      map2_dfr(away$rosterForMatchupPeriod$entries, 1:n_match, sp_points_by_game) %>% 
        mutate('team_id' = away$teamId[team_ix])
    )
  sp_points <- bind_rows(sp_points, sp_points_)
  
  schedule_ <- bind_cols(schedule_, home_schedule_, away_schedule_) %>%
    mutate_if(is.numeric, function(x) replace(x, x == 0, NA))
  
  
  schedule_ <- 
    schedule_ %>% 
    left_join(select(teams, "home_team" = team, team_id), by = c('home_team_id'= 'team_id')) %>% 
    left_join(select(teams, "away_team" = team, team_id), by = c('away_team_id'= 'team_id')) %>% 
    filter(matchup_id == i)
  
  schedule <- bind_rows(schedule, schedule_)
}
write_csv(schedule, glue('data/stats/{params$season}/schedule_{params$season}.csv'))

### Update Daily Stats
if(params$matchup_id == 1) {
  write_csv(get_matchup_stats(params$matchup_id, season = params$season),
            glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))
} else {
  read_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv')) %>%
    filter(matchup_id < params$matchup_id - 1) %>%
    bind_rows(get_matchup_stats(params$matchup_id - 1, season = params$season)) %>%
    bind_rows(get_matchup_stats(params$matchup_id, season = params$season)) %>%
    write_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))
}

df_daily <- read_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))

### Advanced pitching stats
pitch_stats <- 
  df_daily %>% 
  remove_postcap() %>%
  filter(in_lineup) %>% 
  filter(pitcher) %>% 
  group_by(team_id) %>% 
  summarise('qs' = sum(qs),
            'k' = sum(p_k),
            'k_sp' = sum(p_k[start]),
            'k_rp' = sum(p_k[relief & !relief_start]),
            'bb' = sum(p_walks + p_ibb),
            'bb_sp' = sum(p_walks[start] + p_ibb[start]),
            'bb_rp' = sum(p_walks[relief & !relief_start] + p_ibb[relief & !relief_start]),
            'outs' = sum(p_outs),
            'outs_sp' = sum(p_outs[start]),
            'outs_rp' = sum(p_outs[relief & !relief_start]),
            'earned_runs' = sum(p_er),
            'earned_runs_sp' = sum(p_er[start]),
            'earned_runs_rp' = sum(p_er[relief & !relief_start]),
            'hr_allowed_sp' = sum(hr_allowed[start]),
            'hr_allowed_rp' = sum(hr_allowed[relief & !relief_start]),
            'hr_allowed' = sum(hr_allowed),
            
            'hpb' = sum(p_hbp),
            'hpb_sp' = sum(p_hbp[start]),
            'hpb_rp' = sum(p_hbp[relief & !relief_start]),
            'blue_balls' = sum(blue_balls)) %>% 
  mutate('era' = earned_runs/outs * 27,
         'era_sp' = earned_runs_sp/outs_sp * 27,
         'era_rp' = earned_runs_rp/outs_rp * 27,
         
         'k9' = k/outs * 27,
         'k9_sp' = k_sp/outs_sp * 27,
         'k9_rp' = k_rp/outs_rp * 27,
         'bb9' = bb/outs * 27,
         'bb9_sp' = bb_sp/outs_sp * 27,
         'bb9_rp' = bb_rp/outs_rp * 27,
         'k_per_bb' = k/bb,
         'k_per_bb_sp' = k_sp/bb_sp,
         'k_per_bb_rp' = k_rp/bb_rp,
         'hr9' = hr_allowed/outs * 27,
         'hr9_sp' = hr_allowed_sp/outs_sp * 27,
         'hr9_rp' = hr_allowed_rp/outs_rp * 27,
         
         'fip' = (13 * hr_allowed + 3 * (bb + hpb) - 2 * k)/(outs/3),
         'fip_sp' = (13 * hr_allowed_sp + 3 * (bb_sp + hpb_sp) - 2 * k_sp)/(outs_sp/3),
         'fip_rp' = (13 * hr_allowed_rp + 3 * (bb_rp + hpb_rp) - 2 * k_rp)/(outs_rp
                                                                            /3),
         'fip_constant' = weighted.mean(era, outs/3) - (13 * sum(hr_allowed) + 3 * sum(bb + hpb) - 2 * sum(k))/(sum(outs)/3)) %>% 
  mutate('fip' = fip + fip_constant,
         'fip_sp' = fip_sp + fip_constant,
         'fip_rp' = fip_rp + fip_constant)

write_csv(pitch_stats, glue('data/stats/{params$season}/pitch_stats.csv'))

start_buckets <- 
  df_daily %>% 
  remove_postcap() %>%
  filter(in_lineup) %>% 
  filter(pitcher) %>% 
  group_by(team_id) %>%
  filter(start) %>% 
  summarise('<= 0' = sum(points <= 0),
            '1-5' = sum(points <= 5 & points > 0),
            '6-10'= sum(points <= 10 & points > 5),
            '11-15' = sum(points <= 15 & points > 10),
            '16-20' = sum(points <= 20 & points > 15),
            '21-25' = sum(points <= 25 & points > 20),
            '26-30' = sum(points <= 30 & points > 25),
            '> 30' = sum(points > 30)) %>% 
  pivot_longer(cols = -team_id,
               names_to = 'start_bucket',
               values_to = 'n_start') %>% 
  group_by(team_id) %>% 
  mutate('pct_start' = n_start/sum(n_start))

write_csv(start_buckets, glue('data/stats/{params$season}/start_buckets.csv'))


woba_factors <- 
  list('wBB' = 0.704, 
       'wHBP' = 0.734,
       'w1B' = 0.894,
       'w2B' = 1.263,
       'w3B' = 1.595,
       'wHR' = 2.041)

bat_stats <-
  df_daily %>% 
  filter(in_lineup) %>% 
  filter(lineup_id <= 12) %>% 
  group_by(team_id) %>% 
  summarise('h_1b' = sum(h_1b),
            'h_2b' = sum(h_2b),
            'h_3b' = sum(h_3b),
            'h_hr' = sum(home_runs),
            'h_bb' = sum(b_walks),
            'h_ubb' = sum(b_walks - b_ibb),
            'h_ab' = sum(b_ab),
            'h_pa' = sum(b_pa),
            'h_k' = sum(b_k),
            'h_runs' = sum(b_runs),
            'h_rbi' = sum(b_rbi),
            'h_sf' = sum(b_sf),
            'h_hbp'= sum(b_hpb)) %>% 
  mutate('avg' = (h_1b + h_2b + h_3b + h_hr)/h_ab,
         'obp' = (h_1b + h_2b + h_3b + h_hr + h_bb)/(h_ab + h_bb + h_sf),
         'slg' = (h_1b + 2 * h_2b + 3 * h_3b + 4 * h_hr)/(h_ab),
         'ops' = obp + slg,
         'k_rate' = h_k/h_pa,
         'bb_rate' = h_bb/h_pa,
         'babip' = (h_1b + h_2b + h_3b)/(h_ab - h_k - h_hr + h_sf),
         'woba' = 
           (woba_factors$wBB * h_ubb +
              woba_factors$wHBP * h_hbp +
              woba_factors$w1B * h_1b + 
              woba_factors$w2B * h_2b + 
              woba_factors$w3B * h_3b + 
              woba_factors$wHR * h_hr)/(h_ab + h_ubb + h_sf + h_hbp))

write_csv(bat_stats, glue('data/stats/{params$season}/bat_stats.csv'))

### Penalties for Relief Starts
relief_starts <- 
  df_daily %>% 
  filter(in_lineup) %>% 
  filter(pitcher) %>% 
  group_by(player, player_id, team_id, matchup_id) %>% 
  summarise('sp_games' = sum(start),
            'rp_games' = sum(relief) - sum(relief_start),
            'n_qs' = sum(qs),
            'relief_starts' = sum(relief_start),
            'sp_points' = sum(points[start], na.rm = T),
            'rp_points' = sum(points[relief & !relief_start], na.rm = T)) %>% 
  filter((sp_games > 0 & rp_games > 0) | relief_starts > 0) %>% 
  ungroup()

write_csv(relief_starts, 'data/red_flags/relief_starts.csv')

df_penalty <- 
  df_daily %>%
  filter(in_lineup) %>%
  inner_join(df_start, by = 'matchup_id') %>%
  mutate('day_of_matchup' = scoring_period_id - start_period + 1) %>%
  mutate('days_left' = end_period - scoring_period_id) %>%
  group_by(matchup_id, team_id, day_of_matchup, days_left, start_cap, scoring_period_id) %>%
  summarise('day_points' = sum(points),
            'start_points' = sum(points[start], na.rm = T),
            'starts' = sum(start),
            'batting_points' = sum(points[batter]),
            'pitching_points' = sum(points[pitcher])) %>%
  group_by(matchup_id, team_id) %>%
  mutate('total_starts' = cumsum(starts)) %>% 
  mutate('over_start_cap' = total_starts > start_cap & lag(total_starts) <= start_cap ) %>% 
  mutate('penalty' = ifelse(!over_start_cap, 0, sign(start_points) * plyr::round_any((total_starts - start_cap)/starts * abs(start_points), 0.5, ceiling))) %>%
  select(matchup_id, team_id, penalty, scoring_period_id) %>% 
  inner_join(select(teams, team, team_id)) %>% 
  filter(penalty != 0) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(team, matchup_id, scoring_period_id, penalty) 

write_csv(df_penalty, 'data/red_flags/penalties.csv')

### Relief Starts
if(nrow(relief_starts) > 0) {
  for(i in 1:nrow(relief_starts)) {
    week <- relief_starts$matchup_id[i]
    player <- relief_starts$player[i]
    ix_rp <- which(rp_points$player == player & rp_points$matchup_id == week)
    ix_sp <- which(sp_points$player == player & sp_points$matchup_id == week)
    rp_points$n_points[ix_rp] <- relief_starts$rp_points[i]
    rp_points$n_games[ix_rp] <- relief_starts$rp_games[i]
    sp_points$n_points[ix_sp] <- relief_starts$sp_points[i]
    sp_points$n_games[ix_sp] <- relief_starts$sp_games[i]
    sp_points$n_qs[ix_sp] <- relief_starts$n_qs[i]
  }
}

write_csv(batter_points, glue('data/stats/{params$season}/batting_weekly_{params$season}.csv'))
write_csv(sp_points, glue('data/stats/{params$season}/sp_weekly_{params$season}.csv'))
write_csv(rp_points, glue('data/stats/{params$season}/rp_weekly_{params$season}.csv'))

### Trades
if(params$matchup_id == 1) {
  df_trades <- get_trades(params$matchup_id, season_ = params$season)
} else {
  df_trades <-
    read_csv(glue('data/stats/{params$season}/trades_{params$season}.csv')) %>%
    mutate_at(vars(everything()), as.numeric) %>% 
    filter(matchup_id < params$matchup_id - 1) %>%
    bind_rows(get_trades(params$matchup_id - 1, season_ = params$season)) %>% 
    bind_rows(get_trades(params$matchup_id, season_ = params$season))
}
write_csv(df_trades, glue('data/stats/{params$season}/trades_{params$season}.csv'))

if(nrow(df_trades) > 0) {
  traded_players <- get_trade_players(df_trades)
  write_csv(traded_players, glue('data/stats/{params$season}/traded_players_{params$season}.csv'))
}


### Transaction Log
trans_log <- get_trans_log(params$season, nrow(df_trades) > 0)

### RP Penalties
# df_rp_penalty <- 
#   trans_log %>% 
#   inner_join(teams %>% select(team, team_id, logo)) %>% 
#   filter(transaction_type == 'Free Agent') %>%
#   mutate('matchup_id' = map_dbl(start, ~min(df_start$matchup_id[df_start$end_period >= .x]))) %>% 
#   group_by(team, matchup_id) %>% 
#   filter(rp_eligible) %>% 
#   mutate('rp_add' = cumsum(rp_eligible)) %>% 
#   summarise('n_rp' = sum(rp_eligible),
#             'penalty' = sum(n_points[rp_add > 2])) %>% 
#   ungroup() %>% 
#   filter(penalty != 0)

df_rp_penalty <- 
  df_daily %>% 
  filter(lineup_id == 15 | (lineup_id == 14 & relief) ) %>% 
  filter(!start) %>%  ### for Javier Rule
  inner_join(select(teams, team, team_id)) %>% 
  ### Ryan Helsley Week 11 on IL Days 1-2 of matchip
  # filter(!(matchup_id == 11 & player == 'Ryan Helsley')) %>% 
  # filter(!(matchup_id == 12 & player == 'Jordan Hicks')) %>% 
  # filter(!(matchup_id == 19 & player == 'Hunter Brown')) %>% 
  mutate('stint' = map2_dbl(player_id, scoring_period_id, ~min(trans_log$stint[trans_log$player_id == .x & trans_log$end >= .y]))) %>% 
  group_by(team, matchup_id) %>% 
  arrange(scoring_period_id) %>% 
  mutate('rp_id' = map2_dbl(player_id, stint, ~which(unique(paste(player_id, stint)) == paste(.x, .y)))) %>% 
  summarise('n_rp' = n_distinct(paste(stint, player_id)),
            'penalty' = sum(points[rp_id > 5]),
            'scoring_period_id' = min(scoring_period_id[rp_id > 5])) %>% 
  ungroup() %>% 
  filter(penalty != 0)

write_csv(df_rp_penalty, 'data/red_flags/rp_penalties.csv')


rp_scores <- 
  df_daily %>% 
  filter(lineup_id == 15 | (lineup_id == 14 & relief) ) %>% 
  filter(!start) %>%  ### for Javier Rule
  inner_join(select(teams, team, team_id)) %>% 
  mutate('stint' = map2_dbl(player_id, scoring_period_id, ~min(trans_log$stint[trans_log$player_id == .x & trans_log$end >= .y]))) %>% 
  group_by(team, matchup_id) %>% 
  arrange(scoring_period_id) %>% 
  mutate('rp_id' = map2_dbl(player_id, stint, ~which(unique(paste(player_id, stint)) == paste(.x, .y)))) %>% 
  summarise('rp_points' = sum(points[rp_id <= 5]))


### Expected Standings
### Team points by week
team_points <- 
  select(schedule, contains("home"), matchup_id, game_id) %>% 
  rename_with(function(x) gsub("home_", "", x)) %>% 
  bind_rows(
    select(schedule, contains("away"), matchup_id, game_id) %>% 
      rename_with(function(x) gsub("away_", "", x))
  ) %>% 
  left_join(rp_scores) %>% 
  mutate('sp_points' = pitching_points - rp_points)

team_points <- 
  team_points %>% 
  mutate("adj_pts" = case_when(
    matchup_id == 1 ~ total_points * 7/5,
    matchup_id == 14 ~ total_points * 7/10,
    matchup_id > 21 ~ total_points * 7/14,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) == 2) ~ total_points,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) != 2) ~ NA_real_,
    T ~ total_points)) %>% 
  mutate("adj_batting_pts" = case_when(
    matchup_id == 1 ~ batting_points * 7/5,
    matchup_id == 14 ~ batting_points * 7/10,
    matchup_id > 21 ~ batting_points * 7/14,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) == 2) ~ batting_points,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) != 2) ~ NA_real_,
    
    T ~ batting_points)) %>% 
  mutate("adj_pitching_pts" = case_when(
    matchup_id == 1 ~ pitching_points * 7/5,
    matchup_id == 14 ~ pitching_points * 7/10,
    matchup_id > 21 ~ pitching_points * 7/14,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) == 2) ~ pitching_points,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) != 2) ~ NA_real_,
    
    T ~ pitching_points)) %>% 
  
  mutate("adj_sp_pts" = case_when(
    matchup_id == 1 ~ sp_points * 7/5,
    matchup_id == 14 ~ sp_points * 7/10,
    matchup_id > 21 ~ sp_points * 7/14,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) == 2) ~ sp_points,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) != 2) ~ NA_real_,
    
    T ~ sp_points)) %>% 
  
  mutate("adj_rp_pts" = case_when(
    matchup_id == 1 ~ rp_points * 7/5,
    matchup_id == 14 ~ rp_points * 7/10,
    matchup_id > 21 ~ rp_points * 7/14,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) == 2) ~ rp_points,
    (matchup_id == params$matchup_id) & (wday(Sys.Date()) != 2) ~ NA_real_,
    
    T ~ rp_points))

team_points <- 
  team_points %>% 
  left_join(team_points, 
            by = c('matchup_id', 'game_id'),
            suffix = c("", "_opp"),
            relationship = 'many-to-many'
  ) %>% 
  filter(team != team_opp) 

total_team_points <- 
  team_points %>% 
  filter(matchup_id <= params$matchup_id) %>% 
  filter(matchup_id <= reg_season) %>% 
  group_by(team) %>%
  summarise("total_points" = sum(total_points, na.rm = T),
            "total_adj_points" = sum(adj_pts, na.rm = T),
            "batting_points" = sum(batting_points, na.rm = T),
            "pitching_points" = sum(pitching_points, na.rm = T),
            'sp_points' = sum(sp_points, na.rm = T),
            'rp_points' = sum(rp_points, na.rm = T),
            "adj_batting_pts" = mean(adj_batting_pts, na.rm = T),
            "adj_pitching_pts" = mean(adj_pitching_pts, na.rm = T),
            'adj_pts' = mean(adj_pts, na.rm = T),
            'adj_sp_pts' = mean(adj_sp_pts, na.rm = T),
            'adj_rp_pts' = mean(adj_rp_pts, na.rm = T))


### Summary Stats mean points by week
mean_pts_by_week <- 
  group_by(team_points, matchup_id) %>%
  summarise("adj_pts" = mean(adj_pts, na.rm = T),
            "adj_batting_pts" = mean(adj_batting_pts, na.rm = T),
            "adj_pitching_pts" = mean(adj_pitching_pts, na.rm = T),
            'adj_sp_pts' = mean(adj_sp_pts, na.rm = T),
            'adj_rp_pts' = mean(adj_rp_pts, na.rm = T))

### Weekly ranks
team_points <- 
  group_by(team_points, matchup_id) %>% 
  mutate("overall_rank" = rank(desc(total_points), ties.method = 'average'),
         "batting_rank" = rank(desc(batting_points), ties.method = 'average'),
         "pitching_rank" = rank(desc(pitching_points), ties.method = 'average')) %>% 
  ungroup()


### Batter points per game
if(period > 0) {
  batter_ppg <- 
    batter_points %>% 
    filter(matchup_id <= params$matchup_id) %>%
    filter(matchup_id <= reg_season) %>% 
    filter(!is.na(n_games)) %>% 
    inner_join(select(teams, team_id, team)) %>% 
    group_by(team) %>% 
    summarise("batting_ppg" = sum(n_points)/sum(n_games),
              'n_games' = sum(n_games),
              'total' = sum(n_points)) %>% 
    arrange(desc(batting_ppg)) 
  
  ### rp points per game
  rp_ppg <- 
    rp_points %>% 
    filter(matchup_id <= params$matchup_id) %>% 
    filter(matchup_id <= reg_season) %>% 
    filter(!is.na(n_games)) %>% 
    inner_join(select(teams, team_id, team)) %>% 
    group_by(team) %>% 
    summarise("rp_ppg" = sum(n_points)/sum(n_games),
              'n_games' = sum(n_games),
              'total' = sum(n_points)) %>% 
    arrange(desc(rp_ppg)) 
  
  ### SP Points Per Game
  sp_ppg <- 
    sp_points %>% 
    sp_remove_postcap() %>%
    filter(matchup_id <= params$matchup_id) %>% 
    filter(matchup_id <= reg_season) %>% 
    filter(!is.na(n_games)) %>% 
    inner_join(select(teams, team_id, team)) %>% 
    group_by(team) %>% 
    summarise("sp_ppg" = sum(n_points)/sum(n_games),
              'n_games' = sum(n_games),
              'total' = sum(n_points)) %>% 
    arrange(desc(sp_ppg)) 
  
  ### SP Points Per Game
  qs_pct <- 
    sp_points %>% 
    sp_remove_postcap() %>% 
    filter(matchup_id <= params$matchup_id) %>% 
    filter(matchup_id <= reg_season) %>% 
    filter(!is.na(n_games)) %>% 
    inner_join(select(teams, team_id, team)) %>% 
    group_by(team) %>% 
    summarise("qs_pct" = sum(n_qs)/sum(n_games),
              'n_games' = sum(n_games),
              'n_qs' = sum(n_qs)) %>% 
    arrange(desc(qs_pct)) 
} else {
  batter_ppg <- 
    tibble('team' = teams$team,
           'n_games' = 0,
           'total' = 0,
           'batting_ppg' = 0)
  
  sp_ppg <- 
    tibble('team' = teams$team,
           'n_games' = 0,
           'total' = 0,
           'sp_ppg' = 0)
  
  rp_ppg <- 
    tibble('team' = teams$team,
           'n_games' = 0,
           'total' = 0,
           'rp_ppg' = 0)
  
  qs_pct <- 
    tibble('team' = teams$team,
           'n_games' = 0,
           'n_qs' = 0,
           'qs_pct' = 0)
  
}


### Exp Record
if(params$matchup_id > 1) {
  exp_standings <- 
    team_points %>% 
    filter(matchup_id < min(reg_season+1, params$matchup_id)) %>% 
    group_by(team) %>% 
    summarise(
      "win_pct" = mean(total_points > total_points_opp, na.rm = T) + 0.5 * mean(total_points == total_points_opp, na.rm = T),
      "exp_win_pct" = mean((12 - overall_rank)/11, na.rm = T),
      'win' = sum(total_points > total_points_opp, na.rm = T) + 0.5 * sum(total_points == total_points_opp, na.rm = T),
      'loss' = sum(total_points < total_points_opp, na.rm = T) + 0.5 * sum(total_points == total_points_opp, na.rm = T),
      "exp_win" = sum((12 - overall_rank)/11, na.rm = T),
      "exp_loss" = sum(1 - (12 - overall_rank)/11, na.rm = T)) %>% 
    inner_join(select(teams, team, division_id, logo)) %>% 
    inner_join(total_team_points) %>% 
    left_join(batter_ppg %>% select(-n_games, -total)) %>% 
    left_join(rp_ppg %>% select(-n_games, -total)) %>% 
    left_join(sp_ppg %>% select(-n_games, -total)) %>% 
    left_join(qs_pct %>% select(-n_games, -n_qs)) %>% 
    arrange(-win_pct, -total_points) %>% 
    mutate('rank' = 1:12)
} else {
  exp_standings <- 
    team_points %>% 
    # filter(matchup_id <= min(reg_season+1, (wday(Sys.Date()) == 2 & hour(Sys.time()) < 12) + params$matchup_id)) %>% 
    filter(matchup_id <= params$matchup_id) %>% 
    group_by(team) %>% 
    summarise(
      "win_pct" = 0.5,
      "exp_win_pct" = 0.5,
      'win' = 0,
      'loss' = 0,
      "exp_win" = 0,
      "exp_loss" = 0) %>% 
    inner_join(select(teams, team, division_id, logo)) %>% 
    inner_join(total_team_points) %>% 
    left_join(batter_ppg %>% select(-n_games, -total)) %>% 
    left_join(rp_ppg %>% select(-n_games, -total)) %>% 
    left_join(sp_ppg %>% select(-n_games, -total)) %>% 
    left_join(qs_pct %>% select(-n_games, -n_qs)) %>% 
    arrange(-win_pct, -total_points) %>% 
    mutate('rank' = 1:12)
}

league_avg <-  
  tibble('team' = 'League Average',
         'division_id' = NA,
         'win' = NA,
         'loss' = NA,
         'exp_win' = NA,
         'exp_loss' = NA,
         'batting_ppg' = weighted.mean(batter_ppg$batting_ppg, batter_ppg$n_games),
         'sp_ppg' = weighted.mean(sp_ppg$sp_ppg, sp_ppg$n_games),
         'rp_ppg' = weighted.mean(rp_ppg$rp_ppg, rp_ppg$n_games),
         'adj_batting_pts' = mean(exp_standings$adj_batting_pts),
         'adj_pitching_pts' = mean(exp_standings$adj_pitching_pts),
         'adj_pts' = mean(exp_standings$adj_pts),
         'adj_sp_pts' = mean(exp_standings$adj_sp_pts),
         'adj_rp_pts' = mean(exp_standings$adj_rp_pts),
         'batting_points' = mean(exp_standings$batting_points),
         'pitching_points' = mean(exp_standings$pitching_points),
         'rp_points' = mean(exp_standings$rp_points),
         'sp_points' = mean(exp_standings$sp_points),
         'total_points' = mean(exp_standings$total_points), 
         'qs_pct' = weighted.mean(qs_pct$qs_pct, qs_pct$n_games))



exp_standings <- bind_rows(exp_standings, league_avg)
write_csv(exp_standings, glue('data/stats/{params$season}/exp_standings.csv'))
write_csv(team_points, glue('data/stats/{params$season}/team_points.csv'))

### WP for Current Week 
df_wp <- 
  plot_wp(params$season, params$matchup_id, plot = F, all = hour(Sys.time()) >= 20) 

write_csv(df_wp, glue('data/win_prob/{params$season}/week_{params$matchup_id}.csv'))
if(params$matchup_id > 1) {
  df_wp_old <- 
    plot_wp(params$season, params$matchup_id - 1, plot = F, all = T)
  write_csv(df_wp_old, glue('data/win_prob/{params$season}/week_{params$matchup_id - 1}.csv'))
}

df_wp <- 
  df_wp %>% 
  filter(day_of_matchup == max(day_of_matchup))

### playoff simulations
if(params$matchup_id > 1) {
  mu <- mean(team_points$adj_pts, na.rm = T)
  sigma <- sd(team_points$adj_pts, na.rm = T)
  
  team_mus <- 
    group_by(team_points, team) %>% 
    summarise("mean_pts" = mean(adj_pts, na.rm = T),
              "games_played" = max(matchup_id[!is.na(total_points)])) %>% 
    mutate("team_mu" = min(1, games_played/reg_season) * mean_pts  + max(0, (1 - games_played/reg_season)) * mu) %>% 
    pull(team_mu)
  
  team_sigmas <- 
    group_by(team_points, team) %>% 
    summarise("sd_pts" = sd(adj_pts, na.rm = T),
              "games_played" = max(matchup_id[!is.na(total_points)])) %>% 
    mutate("team_sigma" = min(1, games_played/reg_season) * sd_pts  + max(0, (1 - games_played/reg_season)) * sigma) %>% 
    pull(team_sigma)
  team_sigmas[is.na(team_sigmas)] <- sigma
} else {
  tmp <- 
    read_csv('data/stats/2023/schedule_2023.csv') %>% 
    filter(matchup_id %in% c(2:13, 15:20))
  
  mu <-  mean(c(tmp$home_total_points, tmp$away_total_points, na.rm = T))
  sigma <-  sd(c(tmp$home_total_points, tmp$away_total_points, na.rm = T))
  team_mus <- rep(mu, 12)
  team_sigmas <- rep(sigma, 12)
  
}

names(team_mus) <- sort(unique(team_points$team))
names(team_sigmas) <- sort(unique(team_points$team))

na_ix <- 
  (schedule$matchup_id > params$matchup_id) | (wday(Sys.Date()) == 2 & hour(Sys.Date()) < 12 & (schedule$matchup_id >= params$matchup_id))


df_sims <- future_map_dfr(1:params$nsims, sim_season, .options = furrr_options(seed = 12))

### Edit Current Week
if(!(wday(Sys.Date()) == 2 & hour(Sys.Date()) < 20)) {
  df_sims[df_sims$matchup_id == params$matchup_id,] <- edit_wp(df_sims, df_wp, team_mus, team_sigmas)
}

df_sims <- 
  select(df_sims, contains("home"), matchup_id, game_id, sim_id) %>% 
  rename_with(function(x) gsub("home_", "", x)) %>% 
  bind_rows(
    select(df_sims, contains("away"), matchup_id, game_id, sim_id) %>% 
      rename_with(function(x) gsub("away_", "", x))
  )


df_sims <- 
  df_sims %>% 
  select(-contains('batting'), -contains('pitching')) %>% 
  left_join(df_sims %>% select(-contains('batting'), -contains('pitching')),
            by = c('matchup_id', 'game_id', 'sim_id'),
            suffix = c("", "_opp"),
            relationship = 'many-to-many') %>% 
  filter(team != team_opp)  %>% 
  left_join(select(teams, team_id, division_id), by  = 'team_id')

write_csv(df_sims, 'data/playoff_odds/raw_sims.csv')

x <- 
  df_sims %>% 
  filter(matchup_id <= reg_season) %>% 
  group_by(team, division_id, sim_id) %>% 
  summarise('wins' = sum(total_points > total_points_opp),
            'points' = sum(total_points)) %>% 
  ungroup() 
write_csv(x, 'data/playoff_odds/distributions.csv')

x <- 
  group_by(x, sim_id) %>% 
  group_by(sim_id) %>% 
  mutate("playoffs" = get_playoffs(wins, points)) %>% 
  mutate('last_place' = get_last_place(wins, points)) %>% 
  mutate('playoff_seed' = get_playoff_seed(wins, points))

champions <- 
  x %>% 
  group_by(sim_id) %>% 
  arrange(playoff_seed) %>% 
  dplyr::slice(1:4) %>% 
  group_split() %>% 
  future_map_chr(~{
    if(params$matchup_id < max(df_start$matchup_id)) {
      championship_sim(.x$team, team_mus, team_sigmas, matchup_id = params$matchup_id, wp = df_wp$win_prob[1:2])
    } else {
      championship_sim(c(df_wp$team_home[1], df_wp$team_away[1]), team_mus, team_sigmas, matchup_id = params$matchup_id, wp = df_wp$win_prob[1])
    }
  })

sim_results <- 
  group_by(x, team) %>% 
  summarise("mean_wins" = round(mean(wins), 1),
            "mean_pts" = round(mean(points)),
            "playoffs" = mean(playoffs),
            'last_place' = mean(last_place),
            'champ' = mean(champions == team))

if(params$matchup_id == 1) {
  df_sims0 <- 
    tibble('team' = teams$team,
           'playoffs' = 4/12,
           'last_place' = 1/12,
           'champ' = 1/12,
           'matchup_id' = 0,
           'mean_pts' = NA)
  
  write_csv(df_sims0, glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv'))
  
}

### Edit previous week if first day of matchup
params$sim_match_id <- ifelse(!(wday(Sys.Date()) == 2 & hour(Sys.Date()) < 12), params$matchup_id, params$matchup_id - 1)
read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>%
  filter(matchup_id != params$sim_match_id) %>%
  bind_rows(sim_results %>% mutate('matchup_id' = params$sim_match_id)) %>%
  mutate('team' = case_when(team == 'Ketel of Fish' ~ 'Ketel Bells',
                            team == 'Don Julios' ~ 'The Greatest Stroman',
                            team == 'The Greatest Stroman' ~ 'It\'s Gonna Be Gley',
                            team == 'Cron\'s Diease' ~ 'FVOS Re-imagined',
                            team == 'It\'s Gonna Be Gley' ~ 'Takin\' Care of Rizzness',
                            team == 'Hader\'s Gonna Hate' ~ 'Elly De La Snooze',
                            T ~ team)) %>% 
  write_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv'))

if(params$matchup_id != params$sim_match_id) {
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>%
    filter(matchup_id != params$matchup_id) %>%
    bind_rows(sim_results %>% mutate('matchup_id' = params$matchup_id)) %>%
    mutate('team' = case_when(team == 'Ketel of Fish' ~ 'Ketel Bells',
                              team == 'Don Julios' ~ 'The Greatest Stroman',
                              team == 'The Greatest Stroman' ~ 'It\'s Gonna Be Gley',
                              team == 'Cron\'s Diease' ~ 'FVOS Re-imagined',
                              team == 'It\'s Gonna Be Gley' ~ 'Takin\' Care of Rizzness',
                              team == 'Hader\'s Gonna Hate' ~ 'Elly De La Snooze',
                              T ~ team)) %>% 
    write_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv'))
}

### Best Line-up
best_lineup(params$season, params$matchup_id, save = F)
if(params$matchup_id > 1) {
  best_lineup(params$season, params$matchup_id-1, save = F)
}

### ASG
if(params$matchup_id > 6) {
  pkg <- make_asg_graphics(params$season, save = F)
  write_csv(pkg$stars, glue('figures/top_performers/{params$season}/best_lineup/asg_counts.csv'))
  write_csv(pkg$lineups, glue('figures/top_performers/{params$season}/best_lineup/asg_lineups.csv'))
}


### MLB Probables
df_probables <- 
  mlb_schedule(params$season) %>% 
  filter(date >= Sys.Date(),
         date <= Sys.Date() + 2) %>% 
  pull(game_pk) %>% 
  map_dfr(mlb_probables) %>% 
  select(game_date, 'player' = fullName) %>% 
  mutate('player' = stringi::stri_trans_general(str = player, 
                                                id = 'Latin-ASCII')) %>% 
  inner_join(
    df_daily %>% 
      filter(scoring_period_id == max(scoring_period_id)) %>% 
      distinct(player_id, player, team_id),
    
    by = c('player')
  )

write_csv(df_probables, glue('data/stats/{params$season}/probables.csv'))


### Pitch Matrix
df_daily %>% 
  filter(in_lineup) %>% 
  filter(start | relief_start) %>% 
  mutate('ip' = case_when(p_cg > 0 ~ 'CG', 
                          p_outs < 9 ~ '< 3',
                          p_outs > 21 ~ '> 7',
                          T ~ paste(floor(p_outs/3), p_outs %% 3, sep = '.'))) %>% 
  mutate('earned_runs' = ifelse(p_er > 5, '6+', as.character(p_er))) %>% 
  group_by(team_id, ip, earned_runs) %>% 
  count() %>% 
  ungroup() %>% 
  mutate('ip' = factor(ip, levels = c('< 3', '3.0', '3.1', '3.2', '4.0', '4.1', '4.2', '5.0', '5.1', '5.2', '6.0', 
                                      '6.1', '6.2', '7.0', '> 7', 'CG'))) %>% 
  mutate('start_type' = case_when(ip == 'CG' ~ 'CG',
                                  ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(0:3) ~ 'QS',
                                  ip == '5.2' & earned_runs %in% as.character(0:3) ~ 'Blue Balls',
                                  ip %in% c('5.0', '5.1') & earned_runs %in% as.character(0:3) ~ 'QS Potential',
                                  ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(4) ~ 'QS Potential',
                                  T ~ 'Bad Start')) %>% 
  inner_join(teams %>% select(team, team_id), by = 'team_id') %>% 
  write_csv(glue('data/stats/{params$season}/pitch_matrix.csv'))


### Draft
draft <- robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{params$season}/segments/0/leagues/49106?view=mDraftDetail'))
df_draft <- 
  draft$draftDetail$picks %>% 
  select('team_id' = teamId,
         'player_id' = playerId,
         'round_id' = roundId,
         'pick_id' = overallPickNumber)
write_csv(df_draft, glue('data/stats/{params$season}/draft.csv'))


dir_copy('data/', 'app/data', overwrite = T)
dir_copy('figures/', 'app/figures', overwrite = T)
dir_copy('models/', 'app/models', overwrite = T)
dir_delete(glue('app/data/stats/{2020:(params$season - 1)}'))
dir_delete(glue('app/data/win_prob/{2023:(params$season - 1)}'))
file.remove('app/data/playoff_odds/raw_sims.csv')
file.remove('data/playoff_odds/raw_sims.csv')

