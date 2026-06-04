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
library(arrow)

options(readr.show_col_types = F)
options(dplyr.summarise.inform = F)

source('helpers.R')
source('figures/weekly_summary.R')



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
  list('season' = 2026,
       'opening_day' = as.Date('2026-03-25'),
       'opening_day_chart' = as.Date('2026-03-25'),
       'period_rm' = 0)

### Load in All Data
df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season == params$season) 

position <- c('0' = 'C',
              '1' = '1B',
              '2' = '2B',
              '3' = '3B',
              '4' = 'SS',
              '5' = 'OF',
              '7' = '1B/3B',
              '6' = '2B/SS',
              '12' = 'UTIL')

period <- min(max(df_start$end_period), max(1, as.numeric(as.Date(substring(as.POSIXct(Sys.time(), tz="EST") - 5 * 60 * 60, 1, 10)) - params$opening_day) + 1))

params$current_matchup <- max(df_start$matchup_id[df_start$start_period <= period])


#### Read in Data Sets
teams <- change_logo(read_csv(glue('data/stats/{params$season}/teams_{params$season}.csv')))
exp_standings <- change_logo(read_csv(glue('data/stats/{params$season}/exp_standings.csv')))
sim_results <- 
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>% 
  filter(matchup_id == params$current_matchup)
df_daily <-
  read_parquet(glue('data/stats/{params$season}/daily_stats.parquet'))
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
  select(team_id, team, contains('adj'), matchup_id, -ends_with('opp'), -adj_sp_pts, -adj_rp_pts,
         total_points, batting_points, sp_points, rp_points,
         game_id, team_opp, total_points_opp)
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

df_relief_start <- 
  read_csv('data/red_flags/relief_starts_flags.csv')

df_trades <- read_csv(glue('data/stats/{params$season}/trades_{params$season}.csv'))
if(nrow(df_trades) > 0) {
  traded_players <- read_csv(glue('data/stats/{params$season}/traded_players_{params$season}.csv'))
}

trans_log <- read_csv(glue('data/stats/{params$season}/transaction_log_{params$season}.csv'))


distributions <-
  read_parquet('data/playoff_odds/distributions.parquet')

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
mean_pts_by_week <- readRDS('data/cache/mean_pts_by_week.rds')

### For ggbump
df_points <- readRDS('data/cache/df_points.rds')


scale_factors <- readRDS('data/cache/scale_factors.rds') 

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

draft_analysis <- readRDS('data/cache/draft_analysis.rds')


lineup_cache <- readRDS('data/cache/lineup_stats.rds')
lineup_stats  <- lineup_cache$stats
lineup_avg    <- lineup_cache$avg
rm(lineup_cache)

leverage <- read_csv(glue('data/playoff_odds/leverage_{params$season}.csv'))
current_wp <- 
  read_csv(glue('data/win_prob/{params$season}/week_{params$current_matchup}.csv')) %>% 
  mutate('start_factor' = factor(case_when(start_advantage >= 4 ~ '> +3',
                                           start_advantage <= -4 ~ '< -3',
                                           start_advantage > 0 ~ paste0('+', start_advantage),
                                           start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                           T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                '+1', '+2', '+3', '> +3'))) %>% 
  select(day_of_matchup, win_prob, team_home, team_away, start_factor, days_left) %>% 
  filter(day_of_matchup == max(day_of_matchup)) %>% 
  select(team_home, team_away, win_prob) %>% 
  pivot_longer(cols = contains('team'),
               names_to = 'home_away',
               values_to = 'team') %>% 
  mutate('win_prob' = ifelse(home_away == 'team_home', win_prob, 1-win_prob))

leverage_long <- 
  leverage %>% 
  filter(n_W >= 50, n_L >= 50) %>% 
  select(-n_W, -n_L) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(ends_with('_L'), ends_with('_W'), ends_with('_baseline')),
               names_to = c('event', 'win_loss'),
               values_to = 'prob',
               names_sep = '_') %>% 
  mutate('leverage' = ifelse(event == 'playoffs', playoff_leverage, -ferry_leverage)) %>% 
  mutate('leverage' = ifelse(leverage == 0, 1 * 0, leverage)) %>% 
  mutate('label' = sprintf('%0.1f', 100 * leverage)) %>% 
  mutate('label' = glue('{team} ({label}%)')) %>% 
  inner_join(current_wp, by = 'team') %>% 
  mutate('text' = 
           case_when(event == 'playoffs' & win_loss == 'W' ~ glue('Playoff Odds w/ Win: {sprintf("%0.1f", 100 * prob)}%'),
                     event == 'playoffs' & win_loss == 'L' ~ glue('Playoff Odds w/ Loss: {sprintf("%0.1f", 100 * prob)}%'),
                     event == 'playoffs' ~ paste(glue('Current Playoff Odds: {sprintf("%0.1f", 100 * prob)}%'),
                                                 glue('Matchup Win Probability: {sprintf("%0.1f", 100 * win_prob)}%'),
                                                 sep = '\n'),
                     event == 'ferry' & win_loss == 'W' ~ glue('Ferry Odds w/ Win: {sprintf("%0.1f", 100 * prob)}%'),
                     event == 'ferry' & win_loss == 'L' ~ glue('Ferry Odds w/ Loss: {sprintf("%0.1f", 100 * prob)}%'),
                     event == 'ferry' ~ paste(glue('Current Ferry Odds: {sprintf("%0.1f", 100 * prob)}%'),
                                              glue('Matchup Win Probability: {sprintf("%0.1f", 100 * win_prob)}%'),
                                              sep = '\n')))


### League History
league_history <- read_csv('data/stats/league_history.csv')
df_managers <- read_csv('data/stats/manager_history.csv')
win_mat <- read_csv('data/stats/wl_history.csv')

manager_order <- 
  df_managers %>% 
  arrange(desc(season), manager) %>% 
  pull(manager) %>% 
  unique()


### Vinik's MLB Rolling Averages
mlb_batting  <- read_parquet(glue('data/stats/{params$season}/mlb_batting_logs.parquet'))
mlb_pitching <- read_parquet(glue('data/stats/{params$season}/mlb_pitching_logs.parquet'))

mlb_batters <- 
  mlb_batting %>% 
  distinct(player, mlbam_id) %>% 
  arrange(player)

batters <- mlb_batters$mlbam_id 
names(batters) <- mlb_batters$player

bat_stat_cols <- 
  c('HR' = 'home_runs',
    '1B' = 'h_1b',
    '2B' = 'h_2b',
    '3B' = 'h_3b',
    'AB' = 'h_ab',
    'PA' = 'b_pa',
    'BB' = 'b_walks',
    'IBB' = 'b_ibb',
    'SF' = 'b_sf',
    'R' = 'b_runs',
    'RBI' = 'b_rbi',
    'K' = 'b_k',
    'GIDP' = 'b_gidp',
    'Errors' = 'b_E',
    'SB' = 'b_SB',
    'CS' = 'b_cs',
    'Fantasy Points' = 'points')
    
  
mlb_pitchers <- 
  mlb_pitching %>% 
  distinct(player, mlbam_id) %>% 
  arrange(player)

pitchers <- mlb_pitchers$mlbam_id
names(pitchers) <- mlb_pitchers$player

pitch_stat_cols <- 
  c('Outs Recorded' = 'outs',
    'H' = 'hits',
    'ER' = 'earnedruns',
    'BB' = 'walks',
    'HBP' = 'hpb',
    'K' = 'strikeouts',
    'WP' = 'wildPitches',
    'Balks' = 'balks',
    'PO' = 'pickoffs',
    'Shutouts' = 'shitouts',
    'SV' = 'Saves',
    'HD' = 'holds',
    'BS' = 'blownSaves',
    'QS' = 'Quality Starts',
    'CG' = 'Complete Games',
    'Fantasy Points' = 'points')
