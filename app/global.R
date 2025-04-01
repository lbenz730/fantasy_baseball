library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(glue)
library(lubridate)
library(rsvg)
library(gt)
library(patchwork)
library(stringr)
library(purrr)
library(forcats)

options(readr.show_col_types = F)
options(dplyr.summarise.inform = F)

source('helpers.R')



### Custom ggplot theme
theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  plot.subtitle = element_text(size = 16, hjust = 0.5),
                  strip.text = element_text(size = 12),
                  legend.position = "none")
)


### Ferry Logo
ferry <- '<img src="www/ferry.jpg" style="height:30px;">'

### Parameters
params <- 
  list('season' = 2025,
       'opening_day' = as.Date('2025-03-18'),
       'opening_day_chart' = as.Date('2025-03-27'),
       'period_rm' = 3:9)

### Load in All Data
df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season == params$season) 

period <- min(max(df_start$end_period), max(1, as.numeric(as.Date(substring(as.POSIXct(Sys.time(), tz="EST") - 5 * 60 * 60, 1, 10)) - params$opening_day) + 1))

params$current_matchup <- max(df_start$matchup_id[df_start$start_period <= period])


#### Read in Data Sets
teams <- change_logo(read_csv(glue('data/stats/{params$season}/teams_{params$season}.csv')))
exp_standings <- change_logo(read_csv(glue('data/stats/{params$season}/exp_standings.csv')))
sim_results <- 
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>% 
  filter(matchup_id == params$current_matchup)
df_daily <- 
  read_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv')) %>% 
  select(team_id, player, player_id, in_lineup, scoring_period_id, matchup_id, points, played, relief, start, pitcher, batter, qs, relief_start, eligible_slots)
pitch_matrix <- 
  read_csv(glue('data/stats/{params$season}/pitch_matrix.csv')) %>% 
  mutate_at(vars(ip, earned_runs), ~as.character(.x)) %>% 
  mutate(ip = case_when(ip == '4' ~ '4.0', 
                        ip == '5' ~ '5.0',
                        ip == '6' ~ '6.0',
                        ip == '7' ~ '7.0',
                        T ~ ip)) %>% 
  mutate('ip' = factor(ip, levels = c('< 3', '3.0', '3.1', '3.2', '4.0', '4.1', '4.2', '5.0', '5.1', '5.2', '6.0', 
                                      '6.1', '6.2', '7.0', '> 7', 'CG'))) %>% 
  mutate('start_type' = case_when(ip == 'CG' ~ 'CG',
                                  ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(0:3) ~ 'QS',
                                  ip == '5.2' & earned_runs %in% as.character(0:3) ~ 'Blue Balls',
                                  ip %in% c('5.0', '5.1') & earned_runs %in% as.character(0:3) ~ 'QS Potential',
                                  ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(4) ~ 'QS Potential',
                                  T ~ 'Bad Start')) %>% 
  mutate('start_type' = factor(start_type, levels = c('Bad Start', 'Blue Balls', 'QS Potential', 'QS', 'CG')))
df_log <- read_csv(glue('figures/top_performers/{params$season}/best_lineup/best_lineups.csv'))
team_points <- read_csv(glue('data/stats/{params$season}/team_points.csv')) %>% 
  select(team_id, team, contains('adj'), matchup_id, -ends_with('opp'), -adj_sp_pts, -adj_rp_pts)
bat_stats <- read_csv(glue('data/stats/{params$season}/bat_stats.csv'))
pitch_stats <- read_csv(glue('data/stats/{params$season}/pitch_stats.csv'))
df_penalty <- read_csv('data/red_flags/penalties.csv') %>% 
  mutate('scoring_period_id' = as.numeric(scoring_period_id),
         'penalty' = as.numeric(penalty),
         'matchup_id' = as.numeric(matchup_id))

start_buckets <- 
  read_csv(glue('data/stats/{params$season}/start_buckets.csv')) %>% 
  inner_join(select(teams, team, team_id)) %>% 
  select(team, start_bucket, pct_start)

start_buckets_avg <- 
  read_csv(glue('data/stats/{params$season}/start_buckets.csv')) %>% 
  group_by(start_bucket) %>% 
  summarise('n' = sum(n_start)) %>% 
  mutate('league_avg' = n/sum(n))

df_rp_penalty <- 
  read_csv('data/red_flags/rp_penalties.csv') %>% 
  mutate('penalty' = as.numeric(penalty),
         'matchup_id' = as.numeric(matchup_id)) %>% 
  arrange(matchup_id) 

df_trades <- read_csv(glue('data/stats/{params$season}/trades_{params$season}.csv'))
if(nrow(df_trades) > 0) {
  traded_players <- read_csv(glue('data/stats/{params$season}/traded_players_{params$season}.csv'))
}

trans_log <- read_csv(glue('data/stats/{params$season}/transaction_log_{params$season}.csv'))


distributions <-
  read_csv('data/playoff_odds/distributions.csv') %>%
  filter(sim_id <= 1000) %>% 
  select(-division_id, -sim_id)

df_whatif <- 
  read_csv(glue('data/stats/{params$season}/whatif.csv')) %>% 
  mutate('record' = paste0(n_win, '-', n_loss),
         'win_pct' = n_win/(n_win + n_loss + 0.00000000000000001)) %>%
  inner_join(select(teams, team_id, logo), by = 'team_id') %>% 
  inner_join(select(teams, team_id, logo), by = c('schedule_id' = 'team_id'), 
             suffix = c('_1', '_2')) %>%
  select(-team_id, -schedule_id, -n_win, -n_loss) %>% 
  pivot_wider(names_from = 'logo_2', 
              values_from = c('record', 'win_pct'))


### Probable Pitchers
# df_probables <- 
#   read_csv(glue('data/stats/{params$season}/probables.csv')) %>% 
#   mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
#   select(game_date, player, player_url, team_id) %>% 
#   inner_join(select(teams, team_id, logo)) %>% 
#   select(-team_id)
#   

# gt_probables <- 
# df_probables %>% 
#   group_by(game_date) %>% 
#   gt() %>% 
#   
#   ### Align Columns
#   cols_align(align = "center", columns = everything()) %>%
#   
#   ### Borders
#   tab_style(
#     style = list(
#       cell_borders(
#         sides = "bottom",
#         color = "black",
#         weight = px(3)
#       )
#     ),
#     locations = list(
#       cells_column_labels(
#         columns = gt::everything()
#       )
#     )
#   )  %>% 
#   
#   ### Logos
#   text_transform(
#     locations = cells_body(contains(c('player_url'))),
#     fn = function(x) {
#       web_image(
#         url = x,
#         height = 50
#       )
#     }
#   ) %>%
#   
#   text_transform(
#     locations = cells_body(columns = contains(c('logo'))), 
#     fn = function(x) {
#       local_image(
#         filename = x,
#         height = 50
#       )
#     }
#   ) %>%
#   
#   
#   
#   ### Names
#   cols_label(
#     'player' = 'Pitcher',
#     'player_url' = '',
#     'logo' = 'Team',
#   ) %>%
#   tab_header(
#     title = md('**Probable Starters**'),
#     subtitle = md('**Next 3 Days**')
#   ) %>%
#   tab_options(column_labels.font.size = 20,
#               heading.title.font.size = 40,
#               heading.subtitle.font.size = 40,
#               heading.title.font.weight = 'bold',
#               heading.subtitle.font.weight = 'bold',
#               column_labels.font.weight = 'bold'
#               
#   )


### Summary Stats mean points by week
mean_pts_by_week <- 
  group_by(team_points, matchup_id) %>%
  summarise("adj_pts" = mean(adj_pts, na.rm = T),
            "adj_batting_pts" = mean(adj_batting_pts, na.rm = T),
            "adj_pitching_pts" = mean(adj_pitching_pts, na.rm = T))

### For ggbump
df_points <- 
  df_daily %>% 
  filter(in_lineup) %>% 
  inner_join(teams, by = 'team_id') %>% 
  bind_rows(df_penalty %>% mutate('points' = -penalty)) %>% 
  group_by(team, scoring_period_id) %>% 
  summarise('n_points' = sum(points)) %>% 
  mutate('total_points' = cumsum(n_points)) %>% 
  group_by(scoring_period_id) %>% 
  mutate('points_back' = total_points - max(total_points)) %>% 
  mutate('rank' = rank(-total_points)) %>% 
  ungroup() 


scale_factors <- 
  df_daily %>% 
  filter(!scoring_period_id %in% params$period_rm) %>% 
  filter(in_lineup) %>% 
  filter(matchup_id < max(2, params$current_matchup)) %>% 
  group_by(team_id, matchup_id) %>% 
  summarise('n_bat' = sum(played),
            'n_rp' = sum(relief & !start)) %>% 
  inner_join(df_start, by = 'matchup_id') %>% 
  mutate('duration' = ifelse(matchup_id == 1, duration - length(params$period_rm), duration)) %>% 
  mutate('n_rp' = n_rp/duration * 7,
         'n_bat' = n_bat/duration * 7) %>% 
  ungroup() %>% 
  summarise('n_sp' = 8/6,
            'n_rp' = mean(n_rp)/3,
            'n_bat' = mean(n_bat)/13)


### Trades
if(nrow(df_trades) > 0) {
  traded_players <- 
    trans_log %>%
    filter(transaction_type == 'Trade')
} else {
  traded_players <- df_trades
}

threshold <- pmin(20, (period-length(params$period_rm))/2)
threshold_p <- pmin(5, (period-length(params$period_rm))/5)

df_fa <- 
  trans_log %>% 
  filter(!is.na(w_bat), !is.na(w_sp), !is.na(w_rp)) %>% 
  filter(transaction_type == 'Free Agent') %>% 
  filter(w_bat * n_games >= threshold | w_sp * n_games >= threshold_p  | w_rp * n_games >= threshold_p) %>% 
  mutate('ppg_vs_avg' = 
           (ppg - exp_standings$batting_ppg[13]) * scale_factors$n_bat * w_bat + 
           (ppg - exp_standings$rp_ppg[13]) * scale_factors$n_rp * w_rp + 
           (ppg - exp_standings$sp_ppg[13]) * scale_factors$n_sp * w_sp) %>% 
  mutate('total_value' = n_points - (end - 
                                       case_when(start <= min(params$period_rm) - 1 ~ start,
                                                 start >= max(params$period_rm) + 1 ~ start,
                                                 start <= max(params$period_rm) ~  max(params$period_rm)
                                       )
                                     + 1)/7 * 
           (exp_standings$batting_ppg[13] * scale_factors$n_bat * w_bat + 
              exp_standings$rp_ppg[13] * scale_factors$n_rp * w_rp + 
              exp_standings$sp_ppg[13] * scale_factors$n_sp * w_sp)) %>% 
  mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
  inner_join(select(teams, team, team_id, logo), by = 'team_id') %>% 
  mutate('added' = as.character(format.Date(as.Date(params$opening_day) - 1 + start, "%b, %d")),
         'dropped' = ifelse(end == max(end), NA, as.character(format.Date(as.Date(params$opening_day) - 1 + end, "%b, %d")))) %>% 
  select(player, player_url, team, logo, added, dropped, n_points, n_games, ppg, total_value) 

df1 <- 
  df_fa %>% 
  arrange(-total_value) %>% 
  head(20)
names(df1) <- paste0(names(df1), '_1')

df2 <- 
  df_fa %>% 
  arrange(-n_points) %>% 
  head(20)
names(df2) <- paste0(names(df2), '_2')

df_fa <- bind_cols(df1, df2)


if(nrow(traded_players) > 0) {
  trade_stats <- NULL
  
  for(i in 1:nrow(traded_players)) {
    old_team <- traded_players$team_from[i]
    new_team <- traded_players$team_id[i]
    scoring_period <- traded_players$start[i]
    end_period <- traded_players$end[i]
    
    tmp_before <- 
      df_daily %>% 
      filter(player_id == traded_players$player_id[i]) %>% 
      filter(team_id == old_team) %>% 
      filter(scoring_period_id < scoring_period) %>% 
      summarise('points' = sum(points),
                'played' = sum(start) + sum(relief) + sum(played),
                'batter' = sum(played),
                'sp' = sum(start),
                'rp' = sum(relief)) %>% 
      mutate('ppg' = points/played) %>% 
      mutate('ppg_vs_avg' = case_when(rp == 0 & sp == 0 ~ (ppg - exp_standings$batting_ppg[13]) * scale_factors$n_bat,
                                      rp > 0 & sp == 0 ~ (ppg - exp_standings$rp_ppg[13]) * scale_factors$n_rp,
                                      sp > 0 ~ (ppg - exp_standings$sp_ppg[13]) * scale_factors$n_sp)) %>% 
      select(points, played, ppg, ppg_vs_avg)
    names(tmp_before) <- paste0(names(tmp_before), '_before')
    
    tmp_after <- 
      df_daily %>% 
      filter(player_id == traded_players$player_id[i]) %>% 
      filter(team_id == new_team) %>% 
      filter(scoring_period_id >= scoring_period) %>%
      filter(scoring_period_id <= end_period) %>% 
      summarise('points' = sum(points),
                'played' = sum(start) + sum(relief) + sum(played),
                'batter' = sum(played),
                'pitcher' = sum(pitcher),
                'sp' = sum(start),
                'rp' = sum(relief),
                'n_days' = n()) %>% 
      mutate('ppg' =  points/played) %>% 
      mutate('ppg_vs_avg' = case_when(rp == 0 & sp == 0 ~ (ppg - exp_standings$batting_ppg[13]) * scale_factors$n_bat,
                                      rp > 0 & sp == 0 ~ (ppg - exp_standings$rp_ppg[13]) * scale_factors$n_rp,
                                      sp > 0 ~ (ppg - exp_standings$sp_ppg[13]) * scale_factors$n_sp)) %>% 
      mutate('total_value' = points - n_days/7 * 
               case_when(rp == 0 & sp == 0 & pitcher == 0 ~ exp_standings$batting_ppg[13] * scale_factors$n_bat,
                         rp > 0 & sp == 0 ~ exp_standings$rp_ppg[13] * scale_factors$n_rp,
                         sp > 0 | pitcher > 0 ~ exp_standings$sp_ppg[13] *scale_factors$n_sp)
      ) %>% 
      select(points, played, ppg, ppg_vs_avg, total_value)
    names(tmp_after) <- paste0(names(tmp_after), '_after')
    
    trade_stats <- 
      trade_stats %>% bind_rows(
        bind_cols(tmp_before, tmp_after) %>% 
          mutate('player_id' = traded_players$player_id[i],
                 'team_from' = old_team,
                 'team_to' = new_team)
      )
    
  }
  
  tmp_stats <- 
    traded_players %>%
    inner_join(trade_stats, by = c('player_id', 'team_from')) %>% 
    inner_join(select(teams, team_id, team, logo), by = c('team_to' = 'team_id')) %>% 
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
    group_by(trade_id) %>% 
    mutate('n_rec' = map_dbl(team_to, ~sum(team_to == .x)))  %>% 
    mutate('n_given' = map_dbl(team_from, ~sum(team_to == .x)))  %>% 
    group_by(trade_id, team_to) %>% 
    group_split() %>% 
    map(~{ 
      if(.x$n_given[1]  > .x$n_rec[1]) {
        tmp_z <- NULL
        for(z in 1:(.x$n_given[1] - .x$n_rec[1])) {
          tmp <- .x[1,]
          tmp[1,] <- NA
          tmp$trade_id <- .x$trade_id[1]
          tmp_z <- bind_rows(tmp, tmp_z)
        }
        
        
        bind_rows(.x, tmp_z)
      } else {
        .x 
      }
    })
  
  
  df_trades <- 
    map_dfr(1:(length(tmp_stats)/2),  ~{
      x <- tmp_stats[[2 * .x -1]]
      names(x) <- paste0(names(x), '_1')
      y <- tmp_stats[[2 * .x]]
      names(y) <- paste0(names(y), '_2')
      bind_cols(x, y)
    }) %>% 
    select(trade_id_1, 
           player_1, player_url_1, team_1, logo_1,
           points_before_1, played_before_1, ppg_before_1, ppg_vs_avg_before_1,
           points_after_1, played_after_1, ppg_after_1, ppg_vs_avg_after_1, total_value_after_1,
           player_2, player_url_2, team_2, logo_2,
           points_before_2, played_before_2, ppg_before_2, ppg_vs_avg_before_2,
           points_after_2, played_after_2, ppg_after_2, ppg_vs_avg_after_2, total_value_after_2) %>% 
    group_by(trade_id_1) %>% 
    mutate('total_value_after_1' = c(sum(total_value_after_1, na.rm = T), rep(NA, n() - 1)),
           'total_value_after_2' = c(sum(total_value_after_2, na.rm = T), rep(NA, n() - 1))) %>% 
    ungroup() %>% 
    mutate('trade_id_1' = fct_reorder(factor(paste0('Trade #', trade_id_1)), trade_id_1)) %>% 
    group_by(trade_id_1)
  
  m <- 
    df_trades %>% 
    ungroup() %>% 
    select(contains('avg')) %>% 
    abs() %>% 
    max(na.rm = T)
  
}


### GT for Penalties
df_penalty <- 
  df_penalty %>% 
  inner_join(select(teams, team, logo), by = 'team') 

df_rp_penalty <- 
  df_rp_penalty %>% 
  inner_join(select(teams, team,logo), by = 'team') 



### ASG 
if(params$current_matchup > 6) {
  df_asg_lineup <- 
    change_logo(read_csv(glue('figures/top_performers/{params$season}/best_lineup/asg_lineups.csv')),
                cols = c('logo_1', 'logo_2', 'logo_3'),
                team_cols = c('team_1', 'team_2', 'team_3'))
  
  df_asg_counts <- change_logo(read_csv(glue('figures/top_performers/{params$season}/best_lineup/asg_counts.csv')))
}



### Draft


df_draft <- read_csv(glue('data/stats/{params$season}/draft.csv')) %>% 
  rename('draft_id' = team_id)

draft_analysis <- 
  trans_log %>% 
  right_join(df_draft) %>% 
  group_by(player_id, player) %>% 
  summarise(
    'points_total' = sum(n_points),
    'n_games_total' = sum(n_games),
    'points_draft' = sum(n_points[team_id == draft_id]),
    'n_games_draft' = sum(n_games[team_id == draft_id]),
    'w_sp' = max(w_sp, na.rm = T),
    'w_rp' = max(w_rp,na.rm = T),
    'w_bat' = max(w_bat, na.rm = T),
    'ppg' = points_total/(n_games_total + 0.001),
    'ppg_draft' = points_draft/(n_games_draft + 0.001),
    'team_id' = first(draft_id),) %>% 
  right_join(df_draft) %>% 
  inner_join(teams) %>% 
  inner_join(
    df_daily %>% 
      group_by(player_id) %>% 
      filter(scoring_period_id == max(scoring_period_id)) %>% 
      summarise('player_type' = case_when(grepl('14', eligible_slots) ~ 'SP',
                                          grepl('15', eligible_slots) ~ 'RP',
                                          T ~ 'Batter'))
  ) %>% 
  mutate('player_type' = case_when(w_rp == 1 ~ 'RP',
                                   w_sp >= 0.75 ~ 'SP',
                                   w_bat == 1 ~ 'Batter',
                                   w_bat == 0 ~ 'RP/SP',
                                   w_bat > 0 ~ 'Ohtani',
                                   T ~ player_type)) %>% 
  ungroup() %>% 
  mutate('fit' = loess(points_total ~ pick_id, data = .)$fitted) %>% 
  mutate('residual' = points_total - fit) %>% 
  mutate('text' = 
           paste("Player: ", player, "<br>", 
                 "Pick: ", pick_id, "<br>",
                 "# of Games Played: ", n_games_total, " (Drafted Team: ", n_games_draft, ")<br>",
                 "Points: ", points_total, " (Drafted Team: ", points_draft, ")<br>",
                 "PPG: ", sprintf('%0.2f', ppg), " (Drafted Team: ", sprintf('%0.2f', ppg_draft), ")<br>",
                 'Expected Points from Draft Slot: ', sprintf('%0.1f', fit), '<br>',
                 'Points Relative to Draft Slot Expectation: ', sprintf('%0.1f', residual), '<br>',
                 
                 
                 sep = "")) %>% 
  select(team, player, player_id, player_type, contains('points'), contains('games'), contains('ppg'), pick_id, round_id, text, residual, fit)
