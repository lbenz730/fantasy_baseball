### cache_files.R
### Sourced at end of update_data.R (after dir_copy).
### Writes pre-computed objects and pre-rendered outputs directly
### into app/ so the Shiny app can serve them without recomputing.

library(arrow)
library(ggridges)
library(ggbump)
library(patchwork)
library(ggsci)
library(scales)
library(fs)

season <- params$season
app_dir <- 'app'

### Setup ---------------------------------------------------------
dir_create(glue('{app_dir}/data/cache'))
dir_create(glue('{app_dir}/www/cache'))

theme_set(
  theme_bw() +
    theme(
      plot.title    = element_text(size = 20, hjust = 0.5),
      axis.title    = element_text(size = 16),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      strip.text    = element_text(size = 12),
      legend.position = 'none'
    )
)

### ---------------------------------------------------------------
### 1. PARQUET: Large tabular files
### ---------------------------------------------------------------

daily_cols <- c(
  'team_id', 'lineup_id', 'player', 'player_id', 'in_lineup',
  'scoring_period_id', 'matchup_id', 'points', 'played',
  'relief', 'start', 'pitcher', 'batter', 'qs', 'relief_start', 'eligible_slots'
)

write_parquet(
  select(df_daily, all_of(daily_cols)),
  glue('{app_dir}/data/stats/{season}/daily_stats.parquet')
)

write_parquet(
  read_csv('data/playoff_odds/distributions.csv') %>%
    filter(sim_id <= 1000) %>%
    select(team, wins, points),
  glue('{app_dir}/data/playoff_odds/distributions.parquet')
)

write_parquet(
  read_csv(glue('data/stats/{season}/mlb_batting_logs.csv')),
  glue('{app_dir}/data/stats/{season}/mlb_batting_logs.parquet')
)

write_parquet(
  read_csv(glue('data/stats/{season}/mlb_pitching_logs.csv')),
  glue('{app_dir}/data/stats/{season}/mlb_pitching_logs.parquet')
)

### ---------------------------------------------------------------
### 2. RDS: Pre-computed expensive data objects
### ---------------------------------------------------------------

teams_simple <-
  teams %>%
  select(team_id, team)

### df_points (for bump rank chart)
df_points_cache <-
  df_daily %>%
  filter(in_lineup) %>%
  inner_join(teams_simple, by = 'team_id') %>%
  bind_rows(df_penalty %>% mutate('points' = -penalty)) %>%
  group_by(team, scoring_period_id) %>%
  summarise('n_points' = sum(points)) %>%
  mutate('total_points' = cumsum(n_points)) %>%
  group_by(scoring_period_id) %>%
  mutate('points_back' = total_points - max(total_points)) %>%
  mutate('rank' = rank(-total_points)) %>%
  ungroup()

saveRDS(df_points_cache, glue('{app_dir}/data/cache/df_points.rds'))

### draft_analysis (loess fit — most expensive computation in global.R)
position <- c(
  '0' = 'C', '1' = '1B', '2' = '2B', '3' = '3B', '4' = 'SS',
  '5' = 'OF', '7' = '1B/3B', '6' = '2B/SS', '12' = 'UTIL'
)

df_draft_cache <-
  read_csv(glue('data/stats/{season}/draft.csv')) %>%
  rename('draft_id' = team_id)

draft_analysis_cache <-
  trans_log %>%
  select(-round_id, -pick_id) %>%
  right_join(df_draft_cache) %>%
  group_by(player_id, player) %>%
  summarise(
    'points_total'  = sum(n_points),
    'n_games_total' = sum(n_games),
    'points_draft'  = sum(n_points[team_id == draft_id]),
    'n_games_draft' = sum(n_games[team_id == draft_id]),
    'w_sp'  = max(w_sp, na.rm = T),
    'w_rp'  = max(w_rp, na.rm = T),
    'w_bat' = max(w_bat, na.rm = T),
    'ppg'       = points_total / (n_games_total + 0.001),
    'ppg_draft' = points_draft / (n_games_draft + 0.001),
    'team_id'   = first(draft_id)
  ) %>%
  right_join(df_draft_cache) %>%
  inner_join(select(teams, team_id, team)) %>%
  inner_join(
    df_daily %>%
      group_by(player_id) %>%
      filter(scoring_period_id == max(scoring_period_id)) %>%
      summarise('player_type' = case_when(
        grepl('14', eligible_slots) ~ 'SP',
        grepl('15', eligible_slots) ~ 'RP',
        T ~ 'Batter'
      ))
  ) %>%
  mutate('player_type' = case_when(
    w_rp == 1      ~ 'RP',
    w_sp >= 0.75   ~ 'SP',
    w_bat == 1     ~ 'Batter',
    w_bat == 0     ~ 'RP/SP',
    w_bat > 0      ~ 'Ohtani',
    T              ~ player_type
  )) %>%
  ungroup() %>%
  mutate('fit'      = loess(points_total ~ pick_id, data = .)$fitted) %>%
  mutate('residual' = points_total - fit) %>%
  mutate('text' =
    paste(
      "Player: ", player, "<br>",
      "Pick: ", pick_id, "<br>",
      "# of Games Played: ", n_games_total, " (Drafted Team: ", n_games_draft, ")<br>",
      "Points: ", points_total, " (Drafted Team: ", points_draft, ")<br>",
      "PPG: ", sprintf('%0.2f', ppg), " (Drafted Team: ", sprintf('%0.2f', ppg_draft), ")<br>",
      'Expected Points from Draft Slot: ', sprintf('%0.1f', fit), '<br>',
      'Points Relative to Draft Slot Expectation: ', sprintf('%0.1f', residual), '<br>',
      sep = ""
    )
  ) %>%
  select(
    team, player, player_id, player_type,
    contains('points'), contains('games'), contains('ppg'),
    pick_id, round_id, text, residual, fit
  )

saveRDS(draft_analysis_cache, glue('{app_dir}/data/cache/draft_analysis.rds'))

### mean_pts_by_week (reference line for ppw plots)
team_points_cache <-
  read_csv(glue('data/stats/{season}/team_points.csv')) %>%
  select(
    team_id, team, contains('adj'), matchup_id,
    -ends_with('opp'), -adj_sp_pts, -adj_rp_pts,
    total_points, batting_points, sp_points, rp_points,
    game_id, team_opp, total_points_opp
  )

mean_pts_by_week_cache <-
  group_by(team_points_cache, matchup_id) %>%
  summarise(
    'adj_pts'         = mean(adj_pts, na.rm = T),
    'adj_batting_pts' = mean(adj_batting_pts, na.rm = T),
    'adj_pitching_pts' = mean(adj_pitching_pts, na.rm = T)
  )

saveRDS(mean_pts_by_week_cache, glue('{app_dir}/data/cache/mean_pts_by_week.rds'))

### lineup_stats (for positional ppg plot)
lineup_stats_cache <-
  df_daily %>%
  inner_join(teams_simple, by = 'team_id') %>%
  filter(batter, in_lineup) %>%
  group_by(team, lineup_id) %>%
  summarise(
    'ppg'   = sum(points) / sum(played),
    'games' = sum(played)
  ) %>%
  mutate('lineup_id' = position[as.character(lineup_id)]) %>%
  mutate('lineup_id' = fct_relevel(
    lineup_id, 'C', '1B', '3B', '1B/3B', '2B', 'SS', '2B/SS', 'OF', 'UTIL'
  )) %>%
  group_by(lineup_id) %>%
  mutate('ppg_avg' = weighted.mean(ppg, games)) %>%
  ungroup()

lineup_avg_cache <-
  lineup_stats_cache %>%
  group_by(lineup_id) %>%
  summarise('ppg' = weighted.mean(ppg, games))

saveRDS(
  list(stats = lineup_stats_cache, avg = lineup_avg_cache),
  glue('{app_dir}/data/cache/lineup_stats.rds')
)

### ---------------------------------------------------------------
### 3. PNG: Static ggplots
### ---------------------------------------------------------------

current_matchup <- params$matchup_id

### SP Matrix
pitch_matrix_cache <-
  read_csv(glue('data/stats/{season}/pitch_matrix.csv')) %>%
  mutate_at(vars(ip, earned_runs), ~as.character(.x)) %>%
  mutate(ip = case_when(
    ip == '4' ~ '4.0', ip == '5' ~ '5.0',
    ip == '6' ~ '6.0', ip == '7' ~ '7.0',
    T ~ ip
  )) %>%
  mutate('ip' = factor(ip, levels = c(
    '< 3', '3.0', '3.1', '3.2', '4.0', '4.1', '4.2',
    '5.0', '5.1', '5.2', '6.0', '6.1', '6.2', '7.0', '> 7', 'CG'
  ))) %>%
  mutate('start_type' = case_when(
    ip == 'CG' ~ 'CG',
    ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(0:3) ~ 'QS',
    ip == '5.2' & earned_runs %in% as.character(0:3) ~ 'Blue Balls',
    ip %in% c('5.0', '5.1') & earned_runs %in% as.character(0:3) ~ 'QS Potential',
    ip %in% c('6.0', '6.1', '6.2', '7.0', '> 7') & earned_runs %in% as.character(4) ~ 'QS Potential',
    T ~ 'Bad Start'
  )) %>%
  mutate('start_type' = factor(
    start_type, levels = c('Bad Start', 'Blue Balls', 'QS Potential', 'QS', 'CG')
  ))

p_sp_matrix <-
  ggplot(pitch_matrix_cache, aes(x = ip, y = earned_runs)) +
  facet_wrap(~team) +
  annotate(geom = 'rect', xmin = '< 3', xmax = 'CG',   ymin = '4', ymax = '6+',
           col = NA,       fill = 'red',              alpha = 0.1) +
  annotate(geom = 'rect', xmin = '< 3', xmax = '5.0',  ymin = '0', ymax = '3',
           col = NA,       fill = 'red',              alpha = 0.1) +
  annotate(geom = 'rect', xmin = '< 3', xmax = '6.0',  ymin = '3', ymax = '4',
           col = NA,       fill = 'red',              alpha = 0.1) +
  annotate(geom = 'rect', xmin = '5.0', xmax = '6.0',  ymin = '0', ymax = '3',
           col = 'orange', fill = 'orange',           alpha = 0.1) +
  annotate(geom = 'rect', xmin = '6.0', xmax = 'CG',   ymin = '3', ymax = '4',
           col = 'orange', fill = 'orange',           alpha = 0.1) +
  annotate(geom = 'rect', xmin = '6.0', xmax = 'CG',   ymin = '0', ymax = '3',
           col = 'seagreen', fill = 'mediumspringgreen', alpha = 0.1) +
  geom_label(aes(label = n, fill = start_type)) +
  scale_x_discrete(limits = c(
    '< 3', '3.0', '3.1', '3.2', '4.0', '4.1', '4.2',
    '5.0', '5.1', '5.2', '6.0', '6.1', '6.2', '7.0', '> 7', 'CG'
  )) +
  scale_fill_manual(
    values = c('salmon', 'lightskyblue', 'orange', 'seagreen3', 'violet'), drop = F
  ) +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Innings Pitched', y = 'Earned Runs', fill = '',
    title = 'Classification of SP Performances',
    caption = paste(
      'Numbers denote frequency of (ER, IP) Combination',
      'Blue Ball = 5.2 IP and 3 or fewer earned runs',
      'QS Potential = 5.0/5.1 IP and 3 or fewer earned runs OR 6+ IP and 4 earned runs',
      'Green Box = "QS Zone"', 'Orange Boxes = "Potential QS Zone"', 'Red Boxes = "Bad Start Zone"',
      sep = '\n'
    )
  ) +
  theme(
    axis.text.x    = element_text(size = 10),
    plot.caption   = element_text(size = 12),
    plot.title     = element_text(size = 24),
    axis.title     = element_text(size = 20),
    strip.text     = element_text(size = 14),
    legend.text    = element_text(size = 12)
  )

ggsave(
  glue('{app_dir}/www/cache/sp_matrix.png'), p_sp_matrix,
  width = 4800, height = 2700, units = 'px', dpi = 300
)
rm(p_sp_matrix, pitch_matrix_cache)

### Start Buckets
start_buckets_cache <-
  read_csv(glue('data/stats/{season}/start_buckets.csv')) %>%
  inner_join(teams_simple, by = 'team_id') %>%
  select(team, start_bucket, pct_start)

start_buckets_avg_cache <-
  read_csv(glue('data/stats/{season}/start_buckets.csv')) %>%
  group_by(start_bucket) %>%
  summarise('n' = sum(n_start)) %>%
  mutate('league_avg' = n / sum(n))

p_start_buckets <-
  ggplot(start_buckets_cache, aes(x = start_bucket, y = pct_start)) +
  facet_wrap(~team) +
  geom_col(aes(fill = start_bucket)) +
  geom_col(
    data = start_buckets_avg_cache, aes(x = start_bucket, y = league_avg),
    alpha = 0, lty = 2, col = 'black'
  ) +
  geom_label(
    aes(label = paste0(sprintf('%0.1f', 100 * pct_start), '%')), vjust = -0.2
  ) +
  labs(
    x = 'Points', y = '% of Starts', fill = 'Start Points',
    title = 'Distribution of SP Points', caption = 'Dashed Line = League Average'
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, max(start_buckets_cache$pct_start) + 0.05)
  ) +
  scale_x_discrete(limits = c(
    '<= 0', '1-5', '6-10', '11-15', '16-20', '21-25', '26-30', '> 30'
  )) +
  scale_fill_manual(
    values = c('red3', 'salmon', 'orange', 'violet', 'lightskyblue', 'aquamarine2', 'seagreen3', 'forestgreen'),
    drop = F,
    limits = c('<= 0', '1-5', '6-10', '11-15', '16-20', '21-25', '26-30', '> 30')
  ) +
  theme(
    legend.position  = 'bottom',
    plot.caption     = element_text(size = 16),
    plot.title       = element_text(size = 24),
    axis.title       = element_text(size = 20),
    strip.text       = element_text(size = 14),
    legend.text      = element_text(size = 12)
  )

ggsave(
  glue('{app_dir}/www/cache/start_buckets.png'), p_start_buckets,
  width = 4800, height = 2700, units = 'px', dpi = 300
)
rm(p_start_buckets, start_buckets_cache, start_buckets_avg_cache)

### PPW 1/2/3 (points per week by team)
p_ppw_1 <-
  ggplot(team_points_cache, aes(x = matchup_id, y = adj_pts)) +
  facet_wrap(~team) +
  geom_point(
    data = select(team_points_cache, matchup_id, adj_pts),
    aes(x = matchup_id, y = adj_pts), fill = 'grey', alpha = 0.2
  ) +
  geom_line(data = mean_pts_by_week_cache, alpha = 0.4, lty = 2) +
  geom_point(aes(color = team), size = 2) +
  labs(
    x = 'Week', y = 'Points',
    subtitle = 'Total Points (Normalized to 7 Day Matchup)',
    title = 'Fantasy Points by Week'
  ) +
  scale_x_continuous(limits = c(1, current_matchup), breaks = 0:current_matchup)

p_ppw_2 <-
  ggplot(team_points_cache, aes(x = matchup_id, y = adj_batting_pts)) +
  facet_wrap(~team) +
  geom_point(
    data = select(team_points_cache, matchup_id, adj_batting_pts),
    aes(x = matchup_id, y = adj_batting_pts), fill = 'grey', alpha = 0.2
  ) +
  geom_line(data = mean_pts_by_week_cache, alpha = 0.4, lty = 2) +
  geom_point(aes(color = team), size = 2) +
  labs(
    x = 'Week', y = 'Points',
    subtitle = 'Batting Points (Normalized to 7 Day Matchup)',
    title = 'Fantasy Points by Week'
  ) +
  scale_x_continuous(limits = c(1, current_matchup), breaks = 0:current_matchup)

p_ppw_3 <-
  ggplot(team_points_cache, aes(x = matchup_id, y = adj_pitching_pts)) +
  facet_wrap(~team) +
  geom_point(
    data = select(team_points_cache, matchup_id, adj_pitching_pts),
    aes(x = matchup_id, y = adj_pitching_pts), fill = 'grey', alpha = 0.2
  ) +
  geom_line(data = mean_pts_by_week_cache, alpha = 0.4, lty = 2) +
  geom_point(aes(color = team), size = 2) +
  labs(
    x = 'Week', y = 'Points',
    subtitle = 'Pitching Points (Normalized to 7 Day Matchup)',
    title = 'Fantasy Points by Week'
  ) +
  scale_x_continuous(limits = c(1, current_matchup), breaks = 0:current_matchup)

ggsave(glue('{app_dir}/www/cache/ppw_1.png'), p_ppw_1, width = 4000, height = 2250, units = 'px', dpi = 300)
ggsave(glue('{app_dir}/www/cache/ppw_2.png'), p_ppw_2, width = 4000, height = 2250, units = 'px', dpi = 300)
ggsave(glue('{app_dir}/www/cache/ppw_3.png'), p_ppw_3, width = 4000, height = 2250, units = 'px', dpi = 300)
rm(p_ppw_1, p_ppw_2, p_ppw_3)

### Bump chart (scoring rank over time)
p_bump <-
  ggplot(df_points_cache, aes(x = scoring_period_id, y = rank)) +
  facet_wrap(~team) +
  geom_vline(
    data = df_start %>% filter(matchup_id <= current_matchup),
    aes(xintercept = end_period), lty = 2, alpha = 0.5
  ) +
  ggbump::geom_bump(aes(col = team), lwd = 1.2, lineend = 'round') +
  scale_y_reverse(limits = c(12, 1), breaks = 12:1) +
  labs(
    x = 'Day of Season', y = 'Rank by Points Scored',
    title = 'Scoring Rank Over Time'
  )

ggsave(glue('{app_dir}/www/cache/bump.png'), p_bump, width = 4000, height = 2250, units = 'px', dpi = 300)
rm(p_bump)

### Playoff history
historical_odds <-
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{season}.csv'))

p_playoff_history <-
  historical_odds %>%
  rename('ferry' = 'last_place') %>%
  pivot_longer(
    cols = c('playoffs', 'ferry', 'champ'),
    names_to = 'type', values_to = 'odds'
  ) %>%
  select(matchup_id, odds, type, team) %>%
  ggplot(aes(x = matchup_id, y = odds, color = type, fill = type)) +
  facet_wrap(~team) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0, current_matchup, 2)) +
  scale_color_discrete(labels = c('Champion', 'Ferry', 'Playoffs')) +
  scale_fill_discrete(labels = c('Champion', 'Ferry', 'Playoffs')) +
  labs(
    x = 'Week', y = 'Odds', color = '', fill = '',
    title = 'League Odds Over Time'
  ) +
  theme(axis.text = element_text(size = 12), legend.position = 'bottom')

ggsave(
  glue('{app_dir}/www/cache/playoff_history.png'), p_playoff_history,
  width = 3200, height = 1800, units = 'px', dpi = 300
)
rm(p_playoff_history)

### Win / Points distributions
sim_results_cache <-
  historical_odds %>%
  filter(matchup_id == current_matchup)

distributions_cache <-
  read_csv('data/playoff_odds/distributions.csv') %>%
  filter(sim_id <= 1000) %>%
  select(team, wins, points)

p_win_dist <-
  distributions_cache %>%
  mutate('team_' = fct_relevel(
    team, sim_results_cache$team[order(sim_results_cache$mean_wins)]
  )) %>%
  ggplot(aes(x = wins, y = team_, fill = team)) +
  ggridges::geom_density_ridges(
    stat = 'binline', scale = 0.7, binwidth = 1, rel_min_height = 0.02
  ) +
  scale_x_continuous(breaks = 0:21) +
  labs(
    x = '# of Wins', y = 'Team',
    title = 'Distribution of Wins',
    subtitle = 'Across 10,000 Season Simulation'
  ) +
  theme(legend.position = 'none', axis.text = element_text(size = 12))

p_points_dist <-
  distributions_cache %>%
  mutate('team_' = fct_relevel(
    team, sim_results_cache$team[order(sim_results_cache$mean_pts)]
  )) %>%
  ggplot(aes(x = points, y = team_, fill = team)) +
  ggridges::geom_density_ridges(
    scale = 0.9, quantiles = 2, quantile_lines = T, rel_min_height = 0.02
  ) +
  labs(
    x = '# of Points', y = 'Team',
    title = 'Distribution of Points',
    subtitle = 'Across 10,000 Season Simulation'
  ) +
  theme(legend.position = 'none', axis.text = element_text(size = 12))

ggsave(
  glue('{app_dir}/www/cache/win_dist.png'), p_win_dist,
  width = 3200, height = 1800, units = 'px', dpi = 300
)
ggsave(
  glue('{app_dir}/www/cache/points_dist.png'), p_points_dist,
  width = 3200, height = 1800, units = 'px', dpi = 300
)
rm(p_win_dist, p_points_dist, distributions_cache, sim_results_cache)

### Positional PPG
p_positional_ppg <-
  ggplot(lineup_stats_cache, aes(x = ppg, y = fct_rev(lineup_id))) +
  facet_wrap(~team) +
  geom_point(data = lineup_avg_cache, size = 3, shape = 18) +
  geom_point(aes(fill = ppg - ppg_avg), size = 4, pch = 21, color = 'black') +
  scale_fill_gradient2(
    breaks = c(-1.25, -0.75, -0.25, 0.25, 0.75, 1.25),
    low = 'blue', mid = 'lightgrey', high = 'red', midpoint = 0
  ) +
  theme(
    legend.text     = element_text(angle = 90, size = 12, vjust = 0.5, hjust = 1),
    legend.position = 'bottom',
    axis.text       = element_text(size = 16),
    strip.text      = element_text(size = 16)
  ) +
  labs(
    x = 'PPG', y = '',
    fill = 'PPG vs. League Avg at Position',
    title = 'Positional PPG Relative to League Average'
  )

ggsave(
  glue('{app_dir}/www/cache/positional_ppg.png'), p_positional_ppg,
  width = 4000, height = 2250, units = 'px', dpi = 300
)
rm(p_positional_ppg, lineup_stats_cache, lineup_avg_cache)

### ---------------------------------------------------------------
### 3.5. RDS: scale_factors (for global.R + fa/trade chart caching)
### ---------------------------------------------------------------

period_rm_cache <- if(is.null(params$period_rm)) 0 else params$period_rm
scale_factors_cache <-
  df_daily %>%
  filter(!scoring_period_id %in% period_rm_cache) %>%
  filter(in_lineup) %>%
  filter(matchup_id < max(2, current_matchup)) %>%
  group_by(team_id, matchup_id) %>%
  summarise('n_bat' = sum(played), 'n_rp' = sum(relief & !start)) %>%
  inner_join(df_start, by = 'matchup_id') %>%
  mutate('duration' = ifelse(matchup_id == 1, duration - length(period_rm_cache), duration)) %>%
  mutate('n_rp' = n_rp/duration * 7, 'n_bat' = n_bat/duration * 7) %>%
  ungroup() %>%
  summarise('n_sp' = 8/6, 'n_rp' = mean(n_rp)/3, 'n_bat' = mean(n_bat)/13)

saveRDS(scale_factors_cache, glue('{app_dir}/data/cache/scale_factors.rds'))

### ---------------------------------------------------------------
### 4. HTML: Pre-rendered gt tables (stats_table, pitch_table, bat_table)
###    Run inside app/ so change_logo() paths resolve and logos are
###    local_image() base64-encodes files at build time, so the HTML is
###    self-contained and works correctly on shinyapps.io.
### ---------------------------------------------------------------

withr::with_dir(app_dir, {
  library(gt)
  library(ggsci)
  source('helpers.R')

  teams_gt   <- change_logo(read_csv(glue('data/stats/{season}/teams_{season}.csv')))
  exp_st_gt  <- change_logo(read_csv(glue('data/stats/{season}/exp_standings.csv')))
  sim_res_gt <-
    read_csv(glue('data/playoff_odds/historical_playoff_odds_{season}.csv')) %>%
    filter(matchup_id == params$matchup_id)
  pitch_st_gt <- read_csv(glue('data/stats/{season}/pitch_stats.csv'))
  bat_st_gt   <- read_csv(glue('data/stats/{season}/bat_stats.csv'))

  ferry_html <- '<img src="www/ferry.jpg" style="height:30px;">'

  ### ---- stats_table ----
  league_avg_stats <-
    exp_st_gt[13, ] %>%
    select(team, logo, win, loss, exp_win, exp_loss,
           batting_ppg, sp_ppg, rp_ppg,
           adj_batting_pts, adj_sp_pts, adj_rp_pts, adj_pts,
           batting_points, sp_points, rp_points, total_points, qs_pct, rank)

  df_stats <-
    select(exp_st_gt, team, logo, win, loss, exp_win, exp_loss,
           batting_ppg, sp_ppg, rp_ppg,
           adj_batting_pts, adj_sp_pts, adj_rp_pts, adj_pts,
           batting_points, sp_points, rp_points, total_points, qs_pct, rank) %>%
    dplyr::slice(1:12) %>%
    left_join(sim_res_gt, by = 'team') %>%
    select(-matchup_id, -team_id) %>%
    mutate_at(vars(contains('adj_')), ~replace(.x, is.na(.x), 0)) %>%
    bind_rows(league_avg_stats)

  df_stats$team[1:12]   <- paste0(df_stats$team[1:12], ' (', df_stats$rank[1:12], ')')
  df_stats$logo[13]     <- 'www/League.png'
  df_stats$mean_pts[13] <- mean(df_stats$mean_pts[1:12])
  df_stats <- select(df_stats, -rank)

  gt_stats <-
    gt(df_stats) %>%
    fmt_number(columns = c(exp_win, exp_loss, mean_wins), decimals = 1, sep_mark = '') %>%
    fmt_number(columns = c(win, loss), decimals = 1, sep_mark = '', drop_trailing_zeros = T, drop_trailing_dec_mark = T) %>%
    fmt_number(columns = c(mean_pts), decimals = 0, sep_mark = '') %>%
    fmt_number(columns = c(sp_ppg, rp_ppg, adj_pts, adj_batting_pts, adj_sp_pts, adj_rp_pts,
                           batting_points, sp_points, rp_points, total_points), decimals = 1, sep_mark = '') %>%
    fmt_number(columns = c(batting_ppg), decimals = 2, sep_mark = '') %>%
    fmt_percent(columns = c(qs_pct, playoffs, last_place, champ), decimals = 1, sep_mark = '') %>%
    sub_missing(columns = everything(), missing_text = '---') %>%
    cols_align(align = 'center', columns = everything()) %>%
    data_color(columns = c(win, loss, exp_win, exp_loss),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = c(0, max(df_stats$win + df_stats$loss, na.rm = T))),
               autocolor_text = F) %>%
    data_color(columns = c(batting_points),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$batting_points, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(sp_points),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$sp_points, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(rp_points),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$rp_points, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(total_points),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$total_points, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(batting_ppg),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$batting_ppg, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(rp_ppg),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(c(2, 3, df_stats$rp_ppg), na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(sp_ppg),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$sp_ppg, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(adj_pts),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$adj_pts, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(adj_batting_pts),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$adj_batting_pts, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(adj_sp_pts),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$adj_sp_pts, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(adj_rp_pts),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$adj_rp_pts, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(qs_pct),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_stats$qs_pct, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(mean_pts),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = range(df_stats$mean_pts, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(mean_wins),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = range(df_stats$mean_wins, na.rm = T)),
               autocolor_text = F) %>%
    data_color(columns = c(last_place, playoffs),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = c(0, 1)),
               autocolor_text = F) %>%
    data_color(columns = c(champ),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = c(0, 1)),
               autocolor_text = F) %>%
    tab_style(
      style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
      locations = list(cells_body(columns = c(logo, exp_loss, qs_pct, adj_pts, loss, total_points)))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'top', color = 'black', weight = px(3))),
      locations = list(cells_body(rows = team == 'League Average'))
    ) %>%
    tab_spanner(label = 'Season Simulations',       columns = c('mean_pts', 'mean_wins', 'playoffs', 'champ', 'last_place')) %>%
    tab_spanner(label = 'Total Points',             columns = c('batting_points', 'sp_points', 'rp_points', 'total_points')) %>%
    tab_spanner(label = 'Expected Record',          columns = c('exp_win', 'exp_loss')) %>%
    tab_spanner(label = 'Record',                   columns = c('win', 'loss')) %>%
    tab_spanner(label = 'Points Per Player Appearance', columns = c('batting_ppg', 'rp_ppg', 'sp_ppg', 'qs_pct')) %>%
    tab_spanner(label = 'Points Per Matchup',       columns = c('adj_batting_pts', 'adj_sp_pts', 'adj_rp_pts', 'adj_pts')) %>%
    text_transform(
      locations = cells_body(c(logo)),
      fn = function(x) local_image(filename = x, height = 30)
    ) %>%
    tab_style(
      style = list(cell_fill(color = 'white')),
      locations = cells_body(
        columns = c(team, win, loss, exp_win, exp_loss, mean_wins, playoffs, last_place, champ),
        rows = team == 'League Average'
      )
    ) %>%
    tab_style(
      style = list(cell_text(weight = 'bold')),
      locations = cells_body(
        columns = c(team, win, loss, exp_win, exp_loss, mean_wins, playoffs, last_place),
        rows = team == 'League Average'
      )
    ) %>%
    cols_label(
      team = 'Team (Seed)', logo = '', win = 'Wins', loss = 'Losses',
      exp_win = 'Wins', exp_loss = 'Losses',
      batting_ppg = 'Batter', sp_ppg = 'SP', rp_ppg = 'RP',
      adj_pts = 'Total', adj_batting_pts = 'Batting', adj_sp_pts = 'SP', adj_rp_pts = 'RP',
      batting_points = 'Batting', sp_points = 'SP', rp_points = 'RP', total_points = 'Total',
      qs_pct = 'QS %', mean_wins = 'Avg. Wins', mean_pts = 'Avg. Points',
      playoffs = 'Playoffs', champ = 'Champion', last_place = md(ferry_html),
    ) %>%
    tab_header(
      subtitle = md('**Millburnish Fantasy Baseball League Advanced Stats Table**'),
      title    = md('<img src="League.png" style="height:200px;">')
    ) %>%
    tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold') %>%
    tab_footnote(footnote = 'Expected Wins in a given week = (# of teams you scored higher than)/(11 teams you could\'ve played)',
                 locations = cells_column_spanners(spanners = 'Expected Record'), placement = 'left') %>%
    tab_footnote(footnote = 'Points Per Matchup adjusted as average per 7-day matchup',
                 locations = cells_column_spanners(spanners = 'Points Per Matchup'), placement = 'left') %>%
    tab_footnote(footnote = '10,000 simulations of remainder of season',
                 locations = cells_column_spanners(spanners = 'Season Simulations'), placement = 'left')

  writeLines(as_raw_html(gt_stats), 'data/cache/stats_table.html')
  rm(gt_stats, df_stats, league_avg_stats)

  ### ---- pitch_table ----
  df_ps <-
    pitch_st_gt %>%
    mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>%
    mutate_at(vars(everything()), ~replace(.x, .x == Inf, 0)) %>%
    inner_join(teams_gt, by = 'team_id') %>%
    inner_join(exp_st_gt %>% select(team, pitching_points), by = 'team') %>%
    arrange(-pitching_points) %>%
    select(team, logo, era, fip, k9, bb9, k_per_bb, hr9,
           era_sp, fip_sp, k9_sp, bb9_sp, k_per_bb_sp, hr9_sp,
           era_rp, fip_rp, k9_rp, bb9_rp, k_per_bb_rp, hr9_rp,
           qs, blue_balls, langes, bednars) %>%
    bind_rows(tibble(
      'team' = 'League Average', 'logo' = 'www/League.png',
      'era' = weighted.mean(pitch_st_gt$era, pitch_st_gt$outs),
      'fip' = weighted.mean(pitch_st_gt$fip, pitch_st_gt$outs),
      'k9'  = weighted.mean(pitch_st_gt$k9,  pitch_st_gt$outs),
      'bb9' = weighted.mean(pitch_st_gt$bb9, pitch_st_gt$outs),
      'hr9' = weighted.mean(pitch_st_gt$hr9, pitch_st_gt$outs),
      'k_per_bb'    = weighted.mean(pitch_st_gt$k_per_bb,    pitch_st_gt$outs),
      'era_sp'      = weighted.mean(pitch_st_gt$era_sp,      pitch_st_gt$outs_sp),
      'fip_sp'      = weighted.mean(pitch_st_gt$fip_sp,      pitch_st_gt$outs_sp),
      'k9_sp'       = weighted.mean(pitch_st_gt$k9_sp,       pitch_st_gt$outs_sp),
      'bb9_sp'      = weighted.mean(pitch_st_gt$bb9_sp,      pitch_st_gt$outs_sp),
      'hr9_sp'      = weighted.mean(pitch_st_gt$hr9_sp,      pitch_st_gt$outs_sp),
      'k_per_bb_sp' = weighted.mean(pitch_st_gt$k_per_bb_sp, pitch_st_gt$outs_sp),
      'era_rp'      = weighted.mean(pitch_st_gt$era_rp,      pitch_st_gt$outs_rp),
      'fip_rp'      = weighted.mean(pitch_st_gt$fip_rp,      pitch_st_gt$outs_rp),
      'k9_rp'       = weighted.mean(pitch_st_gt$k9_rp,       pitch_st_gt$outs_rp),
      'bb9_rp'      = weighted.mean(pitch_st_gt$bb9_rp,      pitch_st_gt$outs_rp),
      'hr9_rp'      = weighted.mean(pitch_st_gt$hr9_rp,      pitch_st_gt$outs_rp),
      'k_per_bb_rp' = weighted.mean(pitch_st_gt$k_per_bb_rp, pitch_st_gt$outs_rp),
      'qs'          = mean(pitch_st_gt$qs),
      'blue_balls'  = mean(pitch_st_gt$blue_balls),
      'langes'      = mean(pitch_st_gt$langes),
      'bednars'     = mean(pitch_st_gt$bednars),
    )) %>%
    mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>%
    mutate_at(vars(everything()), ~replace(.x, .x == Inf, 0))

  gt_pitch <-
    df_ps %>%
    gt() %>%
    fmt_number(columns = c(era, era_sp, era_rp, fip, fip_sp, fip_rp,
                           k9, k9_sp, k9_rp, bb9, bb9_sp, bb9_rp,
                           k_per_bb, k_per_bb_sp, k_per_bb_rp,
                           hr9, hr9_sp, hr9_rp), decimals = 2, sep_mark = '') %>%
    fmt_number(columns = c(qs, blue_balls, langes, bednars), decimals = 2, drop_trailing_zeros = T) %>%
    sub_missing(columns = everything(), missing_text = '---') %>%
    cols_align(align = 'center', columns = everything()) %>%
    tab_spanner(columns = c('era', 'fip', 'k9', 'bb9', 'k_per_bb', 'hr9'),          label = 'All Pitchers') %>%
    tab_spanner(columns = c('era_sp', 'fip_sp', 'k9_sp', 'bb9_sp', 'k_per_bb_sp', 'hr9_sp'), label = 'Starting Pitchers') %>%
    tab_spanner(columns = c('era_rp', 'fip_rp', 'k9_rp', 'bb9_rp', 'k_per_bb_rp', 'hr9_rp'), label = 'Relief Pitchers') %>%
    data_color(columns = c(era, fip),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(c(df_ps$era, df_ps$fip))), autocolor_text = F) %>%
    data_color(columns = c(era_sp, fip_sp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(c(df_ps$era_sp, df_ps$fip_sp))), autocolor_text = F) %>%
    data_color(columns = c(era_rp, fip_rp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(c(df_ps$era_rp, df_ps$fip_rp))), autocolor_text = F) %>%
    data_color(columns = c(k9),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k9)), autocolor_text = F) %>%
    data_color(columns = c(bb9),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$bb9)), autocolor_text = F) %>%
    data_color(columns = c(hr9),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$hr9)), autocolor_text = F) %>%
    data_color(columns = c(k_per_bb),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k_per_bb)), autocolor_text = F) %>%
    data_color(columns = c(k9_sp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k9_sp)), autocolor_text = F) %>%
    data_color(columns = c(bb9_sp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$bb9_sp)), autocolor_text = F) %>%
    data_color(columns = c(hr9_sp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$hr9_sp)), autocolor_text = F) %>%
    data_color(columns = c(k_per_bb_sp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k_per_bb_sp)), autocolor_text = F) %>%
    data_color(columns = c(k9_rp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k9_rp)), autocolor_text = F) %>%
    data_color(columns = c(bb9_rp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$bb9_rp)), autocolor_text = F) %>%
    data_color(columns = c(hr9_rp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_ps$hr9_rp)), autocolor_text = F) %>%
    data_color(columns = c(k_per_bb_rp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$k_per_bb_rp)), autocolor_text = F) %>%
    data_color(columns = c(qs),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$qs)), autocolor_text = F) %>%
    data_color(columns = c(blue_balls),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$blue_balls)), autocolor_text = F) %>%
    data_color(columns = c(langes),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$langes)), autocolor_text = F) %>%
    data_color(columns = c(bednars),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_ps$bednars)), autocolor_text = F) %>%
    tab_style(
      style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
      locations = list(cells_body(columns = c(logo, contains('hr9'))))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'top', color = 'black', weight = px(3))),
      locations = list(cells_body(rows = team == 'League Average'))
    ) %>%
    text_transform(
      locations = cells_body(c(logo)),
      fn = function(x) local_image(filename = x, height = 30)
    ) %>%
    cols_label(
      team = 'Team', logo = '',
      era = 'ERA', fip = 'FIP', era_sp = 'ERA', fip_sp = 'FIP', era_rp = 'ERA', fip_rp = 'FIP',
      k9 = 'K/9', k9_sp = 'K/9', k9_rp = 'K/9',
      bb9 = 'BB/9', bb9_sp = 'BB/9', bb9_rp = 'BB/9',
      k_per_bb = 'K/BB', k_per_bb_sp = 'K/BB', k_per_bb_rp = 'K/BB',
      hr9 = 'HR/9', hr9_sp = 'HR/9', hr9_rp = 'HR/9',
      qs = 'QS', blue_balls = html('Blue<br>Balls'), langes = 'LNG', bednars = 'BED',
    ) %>%
    tab_header(title = md('**Pitching Stats**')) %>%
    tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold') %>%
    tab_footnote(footnote = 'Blue Balls = 5.2 IP and 3 or fewer earned runs',
                 locations = cells_column_labels('blue_balls'), placement = 'left') %>%
    tab_footnote(footnote = 'RP appearances of -5 or worse, named after Alex "Scrub" Lange',
                 locations = cells_column_labels('langes'), placement = 'left') %>%
    tab_footnote(footnote = 'RP appearances of -10 or worse, named after David "Shit the" Bednar',
                 locations = cells_column_labels('bednars'), placement = 'left')

  writeLines(as_raw_html(gt_pitch), 'data/cache/pitch_table.html')
  rm(gt_pitch, df_ps)

  ### ---- bat_table ----
  df_bs <-
    bat_st_gt %>%
    mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>%
    inner_join(teams_gt, by = 'team_id') %>%
    inner_join(exp_st_gt %>% select(team, batting_points), by = 'team') %>%
    arrange(-batting_points) %>%
    select(team, logo, avg, obp, slg, ops, woba, babip, k_rate, bb_rate) %>%
    bind_rows(tibble(
      'team'    = 'League Average', 'logo' = 'www/League.png',
      'avg'     = weighted.mean(bat_st_gt$avg,     bat_st_gt$h_pa),
      'obp'     = weighted.mean(bat_st_gt$obp,     bat_st_gt$h_pa),
      'slg'     = weighted.mean(bat_st_gt$slg,     bat_st_gt$h_pa),
      'ops'     = weighted.mean(bat_st_gt$ops,     bat_st_gt$h_pa),
      'woba'    = weighted.mean(bat_st_gt$woba,    bat_st_gt$h_pa),
      'babip'   = weighted.mean(bat_st_gt$babip,   bat_st_gt$h_pa),
      'k_rate'  = weighted.mean(bat_st_gt$k_rate,  bat_st_gt$h_pa),
      'bb_rate' = weighted.mean(bat_st_gt$bb_rate, bat_st_gt$h_pa),
    )) %>%
    mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0))

  gt_bat <-
    df_bs %>%
    gt() %>%
    fmt_number(columns = c(avg, obp, ops, slg, woba, babip), decimals = 3, sep_mark = '') %>%
    fmt_percent(columns = c(k_rate, bb_rate), decimals = 1) %>%
    sub_missing(columns = everything(), missing_text = '---') %>%
    cols_align(align = 'center', columns = everything()) %>%
    data_color(columns = c(avg),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$avg)), autocolor_text = F) %>%
    data_color(columns = c(obp),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$obp)), autocolor_text = F) %>%
    data_color(columns = c(ops),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$ops)), autocolor_text = F) %>%
    data_color(columns = c(slg),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$slg)), autocolor_text = F) %>%
    data_color(columns = c(babip),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$babip)), autocolor_text = F) %>%
    data_color(columns = c(woba),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$woba)), autocolor_text = F) %>%
    data_color(columns = c(k_rate),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T),
                                        domain = range(df_bs$k_rate)), autocolor_text = F) %>%
    data_color(columns = c(bb_rate),
               fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                        domain = range(df_bs$bb_rate)), autocolor_text = F) %>%
    tab_style(
      style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
      locations = list(cells_body(columns = c(logo)))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'top', color = 'black', weight = px(3))),
      locations = list(cells_body(rows = team == 'League Average'))
    ) %>%
    text_transform(
      locations = cells_body(c(logo)),
      fn = function(x) local_image(filename = x, height = 30)
    ) %>%
    cols_label(
      team = 'Team', logo = '', avg = 'AVG', obp = 'OBP', ops = 'OPS',
      slg = 'SLG', babip = 'BABIP', woba = 'wOBA', k_rate = 'K %', bb_rate = 'BB %'
    ) %>%
    tab_header(title = md('**Batting Stats**')) %>%
    tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold')

  writeLines(as_raw_html(gt_bat), 'data/cache/bat_table.html')
  rm(gt_bat, df_bs)

  ### ---- league_history_table ----
  league_hist_gt  <- read_csv('data/stats/league_history.csv', show_col_types = FALSE)
  df_mgrs_gt      <- read_csv('data/stats/manager_history.csv', show_col_types = FALSE)

  league_summary_gt <-
    league_hist_gt %>%
    left_join(df_mgrs_gt, by = c('team_id', 'season')) %>%
    group_by(manager) %>%
    summarise('seasons'        = n(),
              'playoffs'       = sum(playoffs),
              'champion'       = sum(champion),
              'runner_up'      = sum(runner_up),
              'third'          = sum(third),
              'points_champ'   = sum(points_champ),
              'ferry'          = sum(ferry),
              'avg_rs_finish'  = mean(rs_finish),
              'n_points'       = sum(n_points),
              'ppg'            = weighted.mean(ppg, n_wins + n_loss),
              'ppg_scaled'     = weighted.mean(ppg_scaled, n_wins + n_loss),
              'n_wins'         = sum(n_wins),
              'n_loss'         = sum(n_loss),
              'current'        = any(season >= 2025)) %>%
    mutate('win_pct' = n_wins / (n_wins + n_loss)) %>%
    arrange(-current, avg_rs_finish) %>%
    select(-current)

  gt_league_hist <-
    gt(league_summary_gt) %>%
    cols_align('center') %>%
    fmt_number(avg_rs_finish, decimals = 1) %>%
    fmt_number(c(n_points, n_wins, n_loss), decimals = 1, drop_trailing_zeros = T) %>%
    fmt_number(c(ppg, ppg_scaled), decimals = 1, drop_trailing_zeros = F) %>%
    fmt_number(win_pct, decimals = 3) %>%
    tab_spanner(c(playoffs, champion, runner_up, third, points_champ, ferry, avg_rs_finish), label = 'Standings') %>%
    tab_spanner(c(n_points, ppg, ppg_scaled), label = 'Points') %>%
    tab_spanner(c(n_wins, n_loss, win_pct), label = 'Record') %>%
    cols_label(manager = 'Manager', seasons = '# Seasons', playoffs = 'Playoffs',
               champion = 'Champ', runner_up = '2nd', third = '3rd',
               points_champ = 'Points Lead', ferry = 'Ferry', avg_rs_finish = 'Avg. Finish',
               n_points = '# of Points', ppg = 'PPG', ppg_scaled = 'Scaled PPG',
               n_wins = 'Wins', n_loss = 'Loss', win_pct = 'Win %') %>%
    tab_style(
      style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
      locations = list(cells_body(columns = c('seasons', 'avg_rs_finish', 'ppg_scaled')))
    ) %>%
    tab_footnote(footnote = "Average of playoff finish for 1-4 and regular season finish for 5-12, under current seeding rules (wins, w/ points tiebreaker; no divisions)",
                 locations = cells_column_labels(avg_rs_finish), placement = 'left') %>%
    tab_footnote(footnote = "PPG relative to league average over relevant seasons, to adjust for different rules/scoring environments",
                 locations = cells_column_labels(ppg_scaled), placement = 'left') %>%
    tab_header(title = md('**Millburnish Fantasy League History**'),
               subtitle = md('**2015-2026**')) %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 30, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22) %>%
    tab_style(style = list(cell_borders(sides = 'top', color = 'black', weight = px(3))),
              locations = list(cells_body(rows = 13))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = playoffs, rows = playoffs == max(playoffs))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = champion, rows = champion == max(champion))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = runner_up, rows = runner_up == max(runner_up))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = third, rows = third == max(third))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = n_points, rows = n_points == max(n_points))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = ppg, rows = ppg == max(ppg))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = win_pct, rows = win_pct == max(win_pct))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = ppg_scaled, rows = ppg_scaled == max(ppg_scaled))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = points_champ, rows = points_champ == max(points_champ))) %>%
    tab_style(style = cell_fill(color = 'lightgreen'),
              locations = cells_body(columns = avg_rs_finish, rows = avg_rs_finish == min(avg_rs_finish))) %>%
    tab_style(style = cell_fill(color = 'pink'),
              locations = cells_body(columns = ferry, rows = ferry == max(ferry)))

  writeLines(as_raw_html(gt_league_hist), 'data/cache/league_history_table.html')
  rm(gt_league_hist, league_hist_gt, df_mgrs_gt, league_summary_gt)

  ### ---- wl_mat ----
  win_mat_gt      <- read_csv('data/stats/wl_history.csv', show_col_types = FALSE)
  mgr_history_gt  <- read_csv('data/stats/manager_history.csv', show_col_types = FALSE)
  manager_order_gt <-
    mgr_history_gt %>%
    arrange(desc(season), manager) %>%
    pull(manager) %>%
    unique()

  gt_wl_mat <-
    win_mat_gt %>%
    mutate('record' = paste0(n_wins, '-', n_loss)) %>%
    mutate('manager'     = factor(manager, levels = manager_order_gt),
           'manager_opp' = factor(manager_opp, levels = manager_order_gt)) %>%
    arrange(manager, manager_opp) %>%
    pivot_wider(names_from = manager_opp,
                values_from = c('record', 'win_pct'),
                id_cols = 'manager') %>%
    relocate(c('manager',
               paste0('record_', intersect(manager_order_gt, win_mat_gt$manager_opp)),
               paste0('win_pct_', intersect(manager_order_gt, win_mat_gt$manager_opp)))) %>%
    gt() %>%
    cols_align('center') %>%
    cols_width(contains('record') ~ px(100)) %>%
    fmt_missing() %>%
    cols_hide(contains('win_pct')) %>%
    data_color(columns = contains('win_pct'),
               target_columns = contains('record'),
               fn = scales::col_numeric(palette = 'RdYlGn',
                                        domain = range(win_mat_gt$win_pct),
                                        na.color = 'white')) %>%
    tab_style(style = cell_text(weight = 'bold'),
              locations = cells_body(columns = 'manager')) %>%
    tab_style(style = cell_borders(sides = 'bottom', color = 'black', weight = px(6), style = 'solid'),
              locations = cells_body(rows = 12)) %>%
    tab_style(style = cell_borders(sides = 'right', color = 'black', weight = px(6), style = 'solid'),
              locations = cells_body(columns = 13)) %>%
    tab_header(title    = md('**Manager vs. Manager Win/Loss Matrix**'),
               subtitle = md('**2015-2026**')) %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 30, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22) %>%
    cols_label('manager' = '') %>%
    cols_label_with(contains('record'), fn = ~gsub('record_', '', .))

  writeLines(as_raw_html(gt_wl_mat), 'data/cache/wl_mat.html')
  rm(gt_wl_mat, win_mat_gt, manager_order_gt, mgr_history_gt)

  ### ---- sp_pen ----
  df_pen_gt <-
    read_csv('data/red_flags/penalties.csv', show_col_types = FALSE) %>%
    mutate('scoring_period_id' = as.numeric(scoring_period_id),
           'penalty'           = as.numeric(penalty),
           'matchup_id'        = as.numeric(matchup_id)) %>%
    inner_join(select(teams_gt, team, logo), by = 'team')

  gt_sp_pen <-
    df_pen_gt %>%
    select(team, logo, matchup_id, penalty) %>%
    group_by(team, logo, matchup_id) %>%
    summarise('penalty' = sum(penalty)) %>%
    ungroup() %>%
    arrange(matchup_id) %>%
    gt() %>%
    cols_align('center') %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo'))),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    cols_label('team' = 'Team', 'logo' = '', 'matchup_id' = 'Matchup', 'penalty' = 'Penalty') %>%
    tab_header(title = 'Start Cap Penalties') %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22)

  writeLines(as_raw_html(gt_sp_pen), 'data/cache/sp_pen.html')
  rm(gt_sp_pen, df_pen_gt)

  ### ---- rp_pen ----
  df_rp_pen_gt <-
    read_csv('data/red_flags/rp_penalties.csv', show_col_types = FALSE) %>%
    mutate('penalty' = as.numeric(penalty), 'matchup_id' = as.numeric(matchup_id)) %>%
    arrange(matchup_id) %>%
    inner_join(select(teams_gt, team, logo), by = 'team')

  gt_rp_pen <-
    df_rp_pen_gt %>%
    select(team, logo, matchup_id, penalty) %>%
    gt() %>%
    cols_align('center') %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo'))),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    cols_label('team' = 'Team', 'logo' = '', 'matchup_id' = 'Matchup', 'penalty' = 'Penalty') %>%
    tab_header(title = 'RP Cap/Usage Penalties') %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22)

  writeLines(as_raw_html(gt_rp_pen), 'data/cache/rp_pen.html')
  rm(gt_rp_pen, df_rp_pen_gt)

  ### ---- rp_start ----
  df_rp_start_gt <-
    read_csv('data/red_flags/relief_starts_flags.csv', show_col_types = FALSE) %>%
    mutate('team_id' = as.numeric(team_id)) %>%
    inner_join(teams_gt, by = 'team_id') %>%
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>%
    select(team, logo, player, player_url, matchup_id, ip, p_er, rule, bonus)

  gt_rp_start <-
    df_rp_start_gt %>%
    gt() %>%
    cols_align('center') %>%
    text_transform(
      locations = cells_body(contains(c('player_url'))),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo'))),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    cols_label('team' = 'Team', 'logo' = '', 'player' = 'Player', 'player_url' = '',
               'matchup_id' = 'Matchup', 'ip' = 'IP', 'p_er' = 'ER',
               'rule' = 'Rule', 'bonus' = 'Bonus') %>%
    tab_header(title = 'RP Starts') %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22)

  writeLines(as_raw_html(gt_rp_start), 'data/cache/rp_start.html')
  rm(gt_rp_start, df_rp_start_gt)

  ### ---- asg tables (only if current_matchup > 6) ----
  if(current_matchup > 6) {
    df_asg_lineup_gt <-
      change_logo(
        read_csv(glue('figures/top_performers/{season}/best_lineup/asg_lineups.csv'),
                 show_col_types = FALSE),
        cols      = c('logo_1', 'logo_2', 'logo_3'),
        team_cols = c('team_1', 'team_2', 'team_3')
      )

    gt_asg_lineup <-
      gt(df_asg_lineup_gt) %>%
      cols_align(align = 'center', columns = everything()) %>%
      fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>%
      fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>%
      tab_spanner(label = 'First Team',  columns = contains('_1')) %>%
      tab_spanner(label = 'Second Team', columns = contains('_2')) %>%
      tab_spanner(label = 'Third team',  columns = contains('_3')) %>%
      tab_style(
        style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
        locations = list(cells_column_labels(columns = gt::everything()))
      ) %>%
      text_transform(
        locations = cells_body(contains(c('player_url'))),
        fn = function(x) web_image(url = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(contains(c('logo'))),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      tab_style(
        style = list(cell_borders(sides = 'right', color = 'black', weight = px(6))),
        locations = list(cells_body(columns = contains('ppg')))
      ) %>%
      tab_style(
        style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
        locations = list(cells_body(rows = c(13, 19)))
      ) %>%
      cols_label(
        'position_1' = 'Position', 'player_1' = 'Player', 'player_url_1' = '',
        'team_1' = 'Team', 'logo_1' = '', 'points_1' = 'Points', 'ppg_1' = 'PPG',
        'position_2' = 'Position', 'player_2' = 'Player', 'player_url_2' = '',
        'team_2' = 'Team', 'logo_2' = '', 'points_2' = 'Points', 'ppg_2' = 'PPG',
        'position_3' = 'Position', 'player_3' = 'Player', 'player_url_3' = '',
        'team_3' = 'Team', 'logo_3' = '', 'points_3' = 'Points', 'ppg_3' = 'PPG'
      ) %>%
      tab_header(title = md(glue('**{season} Fantasy All-Stars**'))) %>%
      tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                  heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold') %>%
      tab_footnote(footnote = "Listed Team = team that player played most games for") %>%
      tab_footnote(footnote = "Players ranked by weighted average of Z-Scores of Points (67%) and PPG (33%) relative to position") %>%
      tab_footnote(footnote = "Min Games for Inclusion: Batter (30), SP (6), RP (10)") %>%
      tab_footnote(footnote = "Only includes games players in starting fantasy lineup")

    writeLines(as_raw_html(gt_asg_lineup), 'data/cache/asg_lineup.html')
    rm(gt_asg_lineup, df_asg_lineup_gt)

    df_asg_counts_gt <-
      change_logo(read_csv(glue('figures/top_performers/{season}/best_lineup/asg_counts.csv'),
                           show_col_types = FALSE))

    gt_asg_counts <-
      gt(df_asg_counts_gt) %>%
      cols_align(align = 'center', columns = everything()) %>%
      tab_style(
        style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
        locations = list(cells_column_labels(columns = gt::everything()))
      ) %>%
      text_transform(
        locations = cells_body(contains(c('logo'))),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      tab_style(
        style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
        locations = list(cells_body(columns = contains(c('logo', 'third_team'))))
      ) %>%
      cols_label('team' = 'Team', 'logo' = '', 'first_team' = '1st Team',
                 'second_team' = '2nd Team', 'third_team' = '3rd Team',
                 'star_points' = 'Total Points') %>%
      tab_header(title = md(glue('**{season} Fantasy All-Stars**'))) %>%
      tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                  heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold') %>%
      tab_footnote(footnote = "Star Points: 1st Team (3), 2nd Team (2), 3rd Team (1)")

    writeLines(as_raw_html(gt_asg_counts), 'data/cache/asg_counts.html')
    rm(gt_asg_counts, df_asg_counts_gt)
  }

  ### ---- gt_draft ----
  draft_gt <- readRDS('data/cache/draft_analysis.rds')
  dfd_gt <-
    draft_gt %>%
    arrange(-residual) %>%
    inner_join(select(teams_gt, team, logo), by = 'team') %>%
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'))

  dfd_bat_gt <-
    dfd_gt %>%
    filter(player_type == 'Batter') %>%
    head(20) %>%
    select(player, player_url, team, logo, pick_id, round_id,
           points_total, points_draft, ppg, ppg_draft, fit, residual)
  names(dfd_bat_gt) <- paste0(names(dfd_bat_gt), '_bat')

  dfd_pitch_gt <-
    dfd_gt %>%
    filter(player_type != 'Batter') %>%
    head(20) %>%
    select(player, player_url, team, logo, pick_id, round_id,
           points_total, points_draft, ppg, ppg_draft, fit, residual)
  names(dfd_pitch_gt) <- paste0(names(dfd_pitch_gt), '_pitch')

  gt_gt_draft <-
    bind_cols(dfd_bat_gt, dfd_pitch_gt) %>%
    gt() %>%
    cols_align('center') %>%
    tab_spanner(label = 'Batting',  columns = contains('_bat')) %>%
    tab_spanner(label = 'Pitching', columns = contains('_pitch')) %>%
    sub_missing(columns = everything(), missing_text = "---") %>%
    fmt_number(contains(c('ppg', 'fit', 'residual')), decimals = 1) %>%
    tab_style(
      style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    text_transform(
      locations = cells_body(contains(c('player_url'))),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo_pitch')), rows = (logo_pitch != '---')),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo_bat')), rows = (logo_bat != '---')),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    tab_style(
      style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
      locations = list(cells_body(columns = contains('residual')))
    ) %>%
    cols_label(
      'team_bat'         = 'Team',   'logo_bat'          = '', 'player_url_bat'    = '',
      'player_bat'       = 'Player', 'points_total_bat'  = 'Points',
      'points_draft_bat' = 'Points (Draft Team)', 'ppg_bat' = 'PPG',
      'ppg_draft_bat'    = 'PPG (Draft Team)', 'pick_id_bat' = 'Pick',
      'round_id_bat'     = 'Round',  'fit_bat'           = 'Exp. Pick Value',
      'residual_bat'     = 'Residual',
      'team_pitch'         = 'Team',   'logo_pitch'          = '', 'player_url_pitch'    = '',
      'player_pitch'       = 'Player', 'points_total_pitch'  = 'Points',
      'points_draft_pitch' = 'Points (Draft Team)', 'ppg_pitch' = 'PPG',
      'ppg_draft_pitch'    = 'PPG (Draft Team)', 'pick_id_pitch' = 'Pick',
      'round_id_pitch'     = 'Round',  'fit_pitch'           = 'Exp. Pick Value',
      'residual_pitch'     = 'Residual'
    ) %>%
    tab_header(title = md('**Top Draft Picks**')) %>%
    tab_options(column_labels.font.size = 12, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold')

  writeLines(as_raw_html(gt_gt_draft), 'data/cache/gt_draft.html')
  rm(gt_gt_draft, draft_gt, dfd_gt, dfd_bat_gt, dfd_pitch_gt)

  ### ---- whatif_table ----
  df_whatif_gt <-
    read_csv(glue('data/stats/{season}/whatif.csv'), show_col_types = FALSE) %>%
    mutate('record'  = paste0(n_win, '-', n_loss),
           'win_pct' = n_win / (n_win + n_loss + 1e-17)) %>%
    inner_join(select(teams_gt, team_id, logo), by = 'team_id') %>%
    inner_join(select(teams_gt, team_id, logo), by = c('schedule_id' = 'team_id'),
               suffix = c('_1', '_2')) %>%
    select(-team_id, -schedule_id, -n_win, -n_loss) %>%
    pivot_wider(names_from = 'logo_2', values_from = c('record', 'win_pct'))

  gt_whatif <-
    gt(df_whatif_gt) %>%
    cols_hide(contains('win_pct')) %>%
    cols_align('center') %>%
    data_color(columns = contains('win_pct'),
               target_columns = contains('record'),
               fn = scales::col_numeric(palette = 'RdYlGn',
                                        domain = range(df_whatif_gt %>% select(contains('win_pct'))))) %>%
    text_transform(locations = cells_body(c(logo_1)),
                   fn = function(x) local_image(filename = x, height = 30)) %>%
    cols_label_with(columns = 2:13, fn = function(x) {
      html(local_image(filename = gsub('record_', '', x), height = 30))
    }) %>%
    cols_label('logo_1' = 'Team') %>%
    tab_spanner(columns = contains('record'), label = "vs. This Team's Schedule") %>%
    tab_header(title = md('**Record vs. Each Team\'s Schedule**')) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 1, columns = 2)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 2, columns = 3)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 3, columns = 4)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 4, columns = 5)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 5, columns = 6)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 6, columns = 7)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 7, columns = 8)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 8, columns = 9)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 9, columns = 10)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 10, columns = 11)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 11, columns = 12)) %>%
    tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = 'solid'),
              locations = cells_body(rows = 12, columns = 13)) %>%
    tab_options(column_labels.font.size = 16, column_labels.font.weight = 'bold',
                heading.title.font.size = 50, heading.subtitle.font.size = 20,
                heading.title.font.weight = 'bold', heading.subtitle.font.weight = 'bold')

  writeLines(as_raw_html(gt_whatif), 'data/cache/whatif_table.html')
  rm(gt_whatif, df_whatif_gt)

  ### ---- fa_chart ----
  threshold_fa   <- pmin(20, (period - length(period_rm_cache)) / 2)
  threshold_fa_p <- pmin(5,  (period - length(period_rm_cache)) / 5)

  df_fa_gt <-
    trans_log %>%
    filter(!is.na(w_bat), !is.na(w_sp), !is.na(w_rp)) %>%
    filter(transaction_type == 'Free Agent') %>%
    mutate('ppg_vs_avg' =
             (ppg - exp_st_gt$batting_ppg[13]) * scale_factors_cache$n_bat * w_bat +
             (ppg - exp_st_gt$rp_ppg[13])      * scale_factors_cache$n_rp  * w_rp  +
             (ppg - exp_st_gt$sp_ppg[13])       * scale_factors_cache$n_sp  * w_sp) %>%
    mutate('total_value' = n_points - (end -
                                         case_when(
                                           start <= min(period_rm_cache) - 1 ~ start,
                                           start >= max(period_rm_cache) + 1 ~ start,
                                           start <= max(period_rm_cache)     ~ max(period_rm_cache)
                                         )
                                       + 1) / 7 *
             (exp_st_gt$batting_ppg[13] * scale_factors_cache$n_bat * w_bat +
                exp_st_gt$rp_ppg[13]   * scale_factors_cache$n_rp  * w_rp  +
                exp_st_gt$sp_ppg[13]   * scale_factors_cache$n_sp  * w_sp)) %>%
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>%
    inner_join(select(teams_gt, team, team_id, logo), by = 'team_id') %>%
    mutate('added'   = as.character(format.Date(as.Date(params$opening_day) - 1 + start, "%b, %d")),
           'dropped' = ifelse(end == max(end), NA,
                              as.character(format.Date(as.Date(params$opening_day) - 1 + end, "%b, %d"))))

  df1_fa <-
    df_fa_gt %>%
    arrange(-total_value) %>%
    filter(w_bat * n_games >= threshold_fa | w_sp * n_games >= threshold_fa_p | w_rp * n_games >= threshold_fa_p) %>%
    select(player, player_url, team, logo, added, dropped, n_points, n_games, ppg, total_value) %>%
    head(20)
  names(df1_fa) <- paste0(names(df1_fa), '_1')

  df2_fa <-
    df_fa_gt %>%
    arrange(-n_points) %>%
    select(player, player_url, team, logo, added, dropped, n_points, n_games, ppg, total_value) %>%
    head(20)
  names(df2_fa) <- paste0(names(df2_fa), '_2')

  df_fa_final_gt <- bind_cols(pad_rows(df1_fa, n = min(nrow(df2_fa), 20)), df2_fa)

  gt_fa_chart <-
    gt(df_fa_final_gt) %>%
    sub_missing(columns = everything(), missing_text = "---") %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>%
    fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>%
    fmt_number(columns = contains('value'), decimals = 2, sep_mark = '') %>%
    sub_missing(columns = everything(), missing_text = "---") %>%
    tab_spanner(label = 'Top 20 by Value ', columns = contains('_1')) %>%
    tab_spanner(label = 'Top 20 by Points', columns = contains('_2')) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
      locations = list(cells_column_labels(columns = gt::everything()))
    ) %>%
    tab_style(
      style = list(cell_borders(sides = "right", color = "black", weight = px(6))),
      locations = list(cells_body(columns = contains('total_value_1')))
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('player_url_1')), rows = (player_url_1 != '---')),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('player_url_2')), rows = (player_url_2 != '---')),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo_1')), rows = (logo_1 != '---')),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    text_transform(
      locations = cells_body(columns = contains(c('logo_2')), rows = (logo_2 != '---')),
      fn = function(x) local_image(filename = x, height = 50)
    ) %>%
    cols_label(
      'player_1' = 'Player', 'player_url_1' = '', 'logo_1' = '',
      'n_points_1' = 'Points', 'ppg_1' = 'PPG', 'team_1' = 'Team',
      'total_value_1' = 'Total Value', 'added_1' = 'Added',
      'dropped_1' = 'Dropped/Traded', 'n_games_1' = '# of Games',
      'player_2' = 'Player', 'player_url_2' = '', 'logo_2' = '',
      'n_points_2' = 'Points', 'ppg_2' = 'PPG', 'team_2' = 'Team',
      'total_value_2' = 'Total Value', 'added_2' = 'Added',
      'dropped_2' = 'Dropped/Traded', 'n_games_2' = '# of Games'
    ) %>%
    tab_header(title = 'Free Agent Analysis') %>%
    tab_footnote(footnote = "Total value= points scored vs. points that league average player at position would be expected to score in time since acquisition",
                 locations = cells_column_labels(columns = contains('value'))) %>%
    tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors_cache$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_st_gt$sp_ppg[13]))) %>%
    tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors_cache$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_st_gt$rp_ppg[13]))) %>%
    tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors_cache$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_st_gt$batting_ppg[13]))) %>%
    tab_source_note('Min Games for Inclusion [Value View]: Batter (20), SP (5), RP (5), or 50% of the season for batters and 20% of the season for pitchers up until day 40') %>%
    tab_source_note('NOTE: Additions prior to Opening Day do not count, as those are indistinguishable from draft picks in ESPN data') %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22)

  writeLines(as_raw_html(gt_fa_chart), 'data/cache/fa_chart.html')
  rm(gt_fa_chart, df_fa_gt, df1_fa, df2_fa, df_fa_final_gt)

  ### ---- fs_chart ----
  df_fs_gt <-
    trans_log %>%
    group_by(player, player_id) %>%
    summarise('n_stints' = n_distinct(stint),
              'n_teams'  = n_distinct(team_id),
              'points'   = sum(n_points)) %>%
    arrange(-n_stints, -n_teams, points) %>%
    head(10) %>%
    ungroup() %>%
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>%
    select(player, player_url, n_stints, n_teams, points)

  gt_fs_chart <-
    df_fs_gt %>%
    gt() %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>%
    text_transform(
      locations = cells_body(columns = contains(c('player_url'))),
      fn = function(x) web_image(url = x, height = 50)
    ) %>%
    cols_label('player' = 'Player', 'player_url' = '',
               'n_stints' = '# of Stints on Teams', 'n_teams' = '# of Teams',
               'points' = 'Points Scored While Rostered') %>%
    tab_header(title = 'Most Transacted Players') %>%
    tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold', row_group.font.size = 22)

  writeLines(as_raw_html(gt_fs_chart), 'data/cache/fs_chart.html')
  rm(gt_fs_chart, df_fs_gt)

  ### ---- trade_chart ----
  if(nrow(df_trades) > 0) {
    traded_players_cache <- trans_log %>% filter(transaction_type == 'Trade')

    trade_stats_cache <- NULL
    for(i in 1:nrow(traded_players_cache)) {
      old_team       <- traded_players_cache$team_from[i]
      new_team       <- traded_players_cache$team_id[i]
      scoring_period <- traded_players_cache$start[i]
      end_period     <- traded_players_cache$end[i]

      tmp_before <-
        df_daily %>%
        filter(player_id == traded_players_cache$player_id[i],
               team_id == old_team,
               scoring_period_id < scoring_period) %>%
        summarise('points' = sum(points * in_lineup),
                  'played' = sum(start * in_lineup) + sum(relief * in_lineup) + sum(played * in_lineup),
                  'sp'     = sum(start * in_lineup),
                  'rp'     = sum(relief * in_lineup)) %>%
        mutate('ppg' = points / played) %>%
        mutate('ppg_vs_avg' = case_when(
          rp == 0 & sp == 0 ~ (ppg - exp_standings$batting_ppg[13]) * scale_factors_cache$n_bat,
          rp > 0 & sp == 0  ~ (ppg - exp_standings$rp_ppg[13])      * scale_factors_cache$n_rp,
          sp > 0            ~ (ppg - exp_standings$sp_ppg[13])       * scale_factors_cache$n_sp)) %>%
        select(points, played, ppg, ppg_vs_avg)
      names(tmp_before) <- paste0(names(tmp_before), '_before')

      tmp_after <-
        df_daily %>%
        filter(player_id == traded_players_cache$player_id[i],
               team_id == new_team,
               scoring_period_id >= scoring_period,
               scoring_period_id <= end_period) %>%
        summarise('points' = sum(points * in_lineup),
                  'played' = sum(start * in_lineup) + sum(relief * in_lineup) + sum(played * in_lineup),
                  'sp'     = sum(start * in_lineup),
                  'rp'     = sum(relief * in_lineup),
                  'n_days' = n()) %>%
        mutate('ppg' = points / played) %>%
        mutate('ppg_vs_avg' = case_when(
          rp == 0 & sp == 0 ~ (ppg - exp_standings$batting_ppg[13]) * scale_factors_cache$n_bat,
          rp > 0 & sp == 0  ~ (ppg - exp_standings$rp_ppg[13])      * scale_factors_cache$n_rp,
          sp > 0            ~ (ppg - exp_standings$sp_ppg[13])       * scale_factors_cache$n_sp)) %>%
        mutate('total_value' = points - n_days / 7 *
                 case_when(
                   rp == 0 & sp == 0 ~ exp_standings$batting_ppg[13] * scale_factors_cache$n_bat,
                   rp > 0 & sp == 0  ~ exp_standings$rp_ppg[13]      * scale_factors_cache$n_rp,
                   sp > 0            ~ exp_standings$sp_ppg[13]       * scale_factors_cache$n_sp)) %>%
        select(points, played, ppg, ppg_vs_avg, total_value)
      names(tmp_after) <- paste0(names(tmp_after), '_after')

      trade_stats_cache <-
        bind_rows(trade_stats_cache,
                  bind_cols(tmp_before, tmp_after) %>%
                    mutate('player_id' = traded_players_cache$player_id[i],
                           'team_from' = old_team,
                           'team_to'   = new_team))
    }

    tmp_stats_cache <-
      traded_players_cache %>%
      inner_join(trade_stats_cache, by = c('player_id', 'team_from')) %>%
      inner_join(select(teams_gt, team_id, team, logo), by = c('team_to' = 'team_id')) %>%
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>%
      group_by(trade_id) %>%
      mutate('n_rec'   = map_dbl(team_to,   ~sum(team_to == .x)),
             'n_given' = map_dbl(team_from, ~sum(team_to == .x))) %>%
      group_by(trade_id, team_to) %>%
      group_split() %>%
      map(~{
        if(.x$n_given[1] > .x$n_rec[1]) {
          tmp_z <- NULL
          for(z in 1:(.x$n_given[1] - .x$n_rec[1])) {
            tmp <- .x[1, ]
            tmp[1, ] <- NA
            tmp$trade_id <- .x$trade_id[1]
            tmp_z <- bind_rows(tmp, tmp_z)
          }
          bind_rows(.x, tmp_z)
        } else {
          .x
        }
      })

    df_trades_cache <-
      map_dfr(1:(length(tmp_stats_cache) / 2), ~{
        x <- tmp_stats_cache[[2 * .x - 1]]
        names(x) <- paste0(names(x), '_1')
        y <- tmp_stats_cache[[2 * .x]]
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

    m_cache <-
      df_trades_cache %>%
      ungroup() %>%
      select(contains('avg')) %>%
      abs() %>%
      max(na.rm = T)

    gt_trade_chart <-
      df_trades_cache %>%
      select(-trade_id_1) %>%
      gt() %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_spanner(label = 'Before Trade ', columns = contains('_before_1')) %>%
      tab_spanner(label = 'Before Trade',  columns = contains('_before_2')) %>%
      tab_spanner(label = 'After Trade ',  columns = contains('_after_1')) %>%
      tab_spanner(label = 'After Trade',   columns = contains('_after_2')) %>%
      data_color(columns = contains('avg'),
                 colors = scales::col_quantile(palette = ggsci::rgb_gsea(),
                                               domain = c(-m_cache, m_cache),
                                               probs = c(0, seq(0.05, 0.95, 0.01), 1))) %>%
      fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>%
      fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>%
      fmt_number(columns = contains('value'), decimals = 2, sep_mark = '') %>%
      sub_missing(columns = everything(), missing_text = "---") %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(
        style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
        locations = list(cells_column_labels(columns = gt::everything()))
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('player_url_1')),
                               rows = (!is.na(logo_1) & !is.na(player_url_1))),
        fn = function(x) web_image(url = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('player_url_2')),
                               rows = (!is.na(logo_2) & !is.na(player_url_2))),
        fn = function(x) web_image(url = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('logo_2')),
                               rows = (!is.na(logo_2) & !is.na(player_url_2))),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('logo_1')),
                               rows = (!is.na(logo_1) & !is.na(player_url_1))),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      tab_style(
        style = list(cell_borders(sides = "right", color = "black", weight = px(8))),
        locations = list(cells_body(columns = contains('total_value_after_1')))
      ) %>%
      tab_style(
        style = list(cell_borders(sides = "right", color = "black", weight = px(4))),
        locations = list(cells_body(columns = c(contains('ppg_vs_avg_before'), contains('logo'))))
      ) %>%
      tab_style(
        style = list(cell_borders(sides = "right", color = "black", weight = px(2))),
        locations = list(cells_body(columns = c(contains('ppg_vs_avg_after'))))
      ) %>%
      tab_style(style = list(cell_fill(color = "white")),
                locations = cells_body(columns = contains('avg_after_1'),
                                       rows = (is.na(points_after_1) | played_after_1 == 0))) %>%
      tab_style(style = list(cell_fill(color = "white")),
                locations = cells_body(columns = contains('avg_after_2'),
                                       rows = (is.na(points_after_2) | played_after_2 == 0))) %>%
      tab_style(style = list(cell_fill(color = "springgreen")),
                locations = cells_body(columns = total_value_after_1,
                                       rows = total_value_after_1 > total_value_after_2)) %>%
      tab_style(style = list(cell_fill(color = "tomato")),
                locations = cells_body(columns = total_value_after_1,
                                       rows = total_value_after_1 < total_value_after_2)) %>%
      tab_style(style = list(cell_fill(color = "springgreen")),
                locations = cells_body(columns = total_value_after_2,
                                       rows = total_value_after_1 < total_value_after_2)) %>%
      tab_style(style = list(cell_fill(color = "tomato")),
                locations = cells_body(columns = total_value_after_2,
                                       rows = total_value_after_1 > total_value_after_2)) %>%
      tab_style(style = list(cell_fill(color = "white")),
                locations = cells_body(columns = contains('avg_before_1'),
                                       rows = (is.na(points_before_1) | played_before_1 == 0))) %>%
      tab_style(style = list(cell_fill(color = "white")),
                locations = cells_body(columns = contains('avg_before_2'),
                                       rows = (is.na(points_before_2) | played_before_2 == 0))) %>%
      tab_style(
        style = list(cell_borders(sides = "bottom", color = "black", weight = px(2))),
        locations = list(cells_body(
          rows = which(df_trades_cache$trade_id_1 != lead(df_trades_cache$trade_id_1) |
                         is.na(lead(df_trades_cache$trade_id_1)))
        ))
      ) %>%
      cols_label(
        'player_1' = 'Player', 'player_url_1' = '', 'team_1' = 'New Team', 'logo_1' = '',
        'points_before_1' = 'Points', 'points_after_1' = 'Points',
        'played_before_1' = 'Games',  'played_after_1' = 'Games',
        'ppg_before_1' = 'PPG',       'ppg_after_1' = 'PPG',
        'total_value_after_1' = 'Total Trade Value',
        'ppg_vs_avg_before_1' = 'Value/Matchup', 'ppg_vs_avg_after_1' = 'Value/Matchup',
        'player_2' = 'Player', 'player_url_2' = '', 'team_2' = 'New Team', 'logo_2' = '',
        'points_before_2' = 'Points', 'points_after_2' = 'Points',
        'played_before_2' = 'Games',  'played_after_2' = 'Games',
        'ppg_before_2' = 'PPG',       'ppg_after_2' = 'PPG',
        'total_value_after_2' = 'Total Trade Value',
        'ppg_vs_avg_before_2' = 'Value/Matchup', 'ppg_vs_avg_after_2' = 'Value/Matchup'
      ) %>%
      tab_header(title = 'Trade Analysis') %>%
      tab_footnote(footnote = "Value = Points/Week vs. League Average at Position (Only includes games played)",
                   locations = cells_column_labels(columns = contains('avg')), placement = 'left') %>%
      tab_footnote(footnote = "Total value accumulated of all players in trade (points scored vs. points that league average players at position(s) would be expected to score in time since trade)",
                   locations = cells_column_labels(columns = contains('value')), placement = 'left') %>%
      tab_source_note('Includes games until player dropped/traded in first stint with new team') %>%
      tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors_cache$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_standings$sp_ppg[13]))) %>%
      tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors_cache$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_standings$rp_ppg[13]))) %>%
      tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors_cache$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_standings$batting_ppg[13]))) %>%
      tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                  heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                  row_group.font.weight = 'bold', row_group.font.size = 22)

    writeLines(as_raw_html(gt_trade_chart), 'data/cache/trade_chart.html')
    rm(gt_trade_chart, df_trades_cache, trade_stats_cache, traded_players_cache, tmp_stats_cache)
  } else {
    gt_trade_empty <-
      gt(df_trades %>% dplyr::slice(0)) %>%
      tab_header(title = 'Trade Analysis') %>%
      tab_footnote(footnote = "Value = Points/Week vs. League Average at Position (Only includes games played)",
                   locations = cells_column_labels(columns = contains('avg'))) %>%
      tab_footnote(footnote = "Total value accumulated of all players in trade (points scored vs. points that league average players at position(s) would be expected to score in time since trade)",
                   locations = cells_column_labels(columns = contains('value'))) %>%
      tab_source_note('Includes games until player dropped/traded in first stint with new team') %>%
      tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors_cache$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_standings$sp_ppg[13]))) %>%
      tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors_cache$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_standings$rp_ppg[13]))) %>%
      tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors_cache$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_standings$batting_ppg[13]))) %>%
      tab_options(column_labels.font.size = 16, heading.title.font.size = 40,
                  heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold',
                  row_group.font.weight = 'bold', row_group.font.size = 22)

    writeLines(as_raw_html(gt_trade_empty), 'data/cache/trade_chart.html')
    rm(gt_trade_empty)
  }
})

### ---------------------------------------------------------------
### 5. PNG: Win Probability charts (per-week, cached by season_week)
###    Backfills any missing weeks; always refreshes current + previous.
### ---------------------------------------------------------------

library(ggimage)

withr::with_dir(app_dir, {
  teams_wp <- change_logo(read_csv(glue('data/stats/{season}/teams_{season}.csv'),
                                   show_col_types = FALSE))

  all_weeks       <- df_start$matchup_id[df_start$matchup_id <= current_matchup]
  png_exists      <- file.exists(glue('www/cache/wp_{season}_week_{all_weeks}.png'))
  weeks_to_render <- unique(c(
    all_weeks[!png_exists],           # backfill any missing week
    current_matchup,                  # always refresh current week
    max(1L, current_matchup - 1L)    # always refresh previous week
  ))

  for(w in sort(weeks_to_render)) {
    wp_path <- glue('data/win_prob/{season}/week_{w}.csv')
    if(!file.exists(wp_path)) next

    df_wp_w <-
      read_csv(wp_path, show_col_types = FALSE) %>%
      mutate('start_factor' = factor(
        case_when(start_advantage >= 4  ~ '> +3',
                  start_advantage <= -4 ~ '< -3',
                  start_advantage > 0   ~ paste0('+', start_advantage),
                  start_advantage < 0   ~ paste0('-', abs(start_advantage)),
                  T ~ '0'),
        levels = c('< -3', '-3', '-2', '-1', '0', '+1', '+2', '+3', '> +3')
      )) %>%
      select(day_of_matchup, win_prob, team_home, team_away, start_factor, days_left)

    df_image_w <-
      df_wp_w %>%
      left_join(teams_wp, by = c('team_home' = 'team')) %>%
      left_join(teams_wp, by = c('team_away' = 'team'), suffix = c('_home', '_away')) %>%
      distinct(team_home, team_away, logo_home, logo_away)

    all_levels <- c('< -3', '-3', '-2', '-1', '0', '+1', '+2', '+3', '> +3')
    df_phantom <- tibble(
      day_of_matchup = rep(-99, 9),
      win_prob       = rep(-99, 9),
      team_home      = df_wp_w$team_home[1],
      team_away      = df_wp_w$team_away[1],
      start_factor   = factor(all_levels, levels = all_levels),
      days_left      = df_wp_w$days_left[1]
    )

    p_wp <-
      ggplot(df_wp_w, aes(x = day_of_matchup, y = win_prob)) +
      facet_wrap(~paste(team_home, 'vs.', team_away)) +
      geom_point(data = df_phantom, aes(fill = start_factor),
                 size = 8, color = 'black', pch = 21) +
      geom_line() +
      geom_point(aes(fill = start_factor), size = 8, color = 'black', pch = 21) +
      ggimage::geom_image(
        data = df_image_w %>% select(team_home, team_away, logo_home),
        aes(x = 0.4, y = 0.95, image = logo_home), size = 0.15
      ) +
      ggimage::geom_image(
        data = df_image_w %>% select(team_away, team_home, logo_away),
        aes(x = 0.4, y = 0.05, image = logo_away), size = 0.15
      ) +
      theme_bw() +
      theme(plot.title         = element_text(size = 24, hjust = 0.5),
            axis.title         = element_text(size = 16),
            strip.text         = element_text(size = 14),
            plot.subtitle      = element_text(size = 18, hjust = 0.5),
            panel.grid.minor.x = element_blank(),
            legend.position    = 'bottom') +
      labs(x    = 'Day of Matchup',
           y    = 'Win Probability',
           title = 'Win Probability Charts',
           subtitle = paste('Week:', w),
           fill = 'Start Advantage') +
      scale_y_continuous(labels = function(x) paste0(100 * pmax(x, 1 - x), '%')) +
      scale_x_continuous(breaks = 0:max(df_wp_w$days_left)) +
      coord_cartesian(xlim = c(0, max(df_wp_w$days_left)), ylim = c(0, 1)) +
      scale_fill_manual(
        values = c(
          '< -3' = '#d73027', '-3' = '#f46d43', '-2' = '#fdae61', '-1' = '#fee08b',
          '0'    = '#ffffbf',
          '+1'   = '#d9ef8b', '+2' = '#a6d96a', '+3' = '#66bd63', '> +3' = '#1a9850'
        )
      ) +
      guides(fill = guide_legend(nrow = 3)) +
      geom_label(data = filter(df_wp_w, day_of_matchup == max(day_of_matchup)),
                 aes(x = 2.5, y = 1, label = paste0(sprintf('%0.1f', 100 * win_prob), '%')),
                 size = 6) +
      geom_label(data = filter(df_wp_w, day_of_matchup == max(day_of_matchup)),
                 aes(x = 2.5, y = 0, label = paste0(sprintf('%0.1f', 100 * (1 - win_prob)), '%')),
                 size = 6)

    ggsave(glue('www/cache/wp_{season}_week_{w}.png'), p_wp,
           width = 4800, height = 2700, units = 'px', dpi = 300)
    rm(df_wp_w, df_image_w, p_wp)
    gc()
  }

  rm(teams_wp)
})

### ---------------------------------------------------------------
### 6. HTML: Top Performers tables (per-week, cached by season_week)
###    Backfills any missing weeks; always refreshes current + previous.
### ---------------------------------------------------------------

### Pre-compute per-player appearance counts (needs all weeks to compute 'n')
bat_top_all <-
  df_daily %>%
  filter(in_lineup, batter) %>%
  group_by(player_id, team_id, player, matchup_id) %>%
  summarise('n_games'  = sum(played),
            'n_points' = sum(points), .groups = 'drop') %>%
  filter(n_games > 0) %>%
  group_by(matchup_id) %>%
  arrange(-n_points) %>%
  dplyr::slice(1:10) %>%
  ungroup() %>%
  group_by(player) %>%
  mutate('n' = n()) %>%
  ungroup() %>%
  mutate(
    'player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
    'player' = paste0(player, ' (', n, ')')
  ) %>%
  select(matchup_id, team_id, player, player_url, n_points)

sp_top_all <-
  df_daily %>%
  filter(in_lineup, pitcher) %>%
  group_by(player_id, team_id, player, matchup_id) %>%
  summarise('n_games'  = sum(start),
            'n_points' = sum(points[start]), .groups = 'drop') %>%
  filter(n_games > 0) %>%
  mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
         'ppg' = n_points / n_games) %>%
  group_by(matchup_id) %>%
  arrange(-ppg, n_games) %>%
  dplyr::slice(1:10) %>%
  ungroup() %>%
  group_by(player) %>%
  mutate('n' = n()) %>%
  ungroup() %>%
  mutate('player' = paste0(player, ' (', n, ')')) %>%
  select(matchup_id, team_id, player, player_url, n_games, ppg)

rp_top_all <-
  df_daily %>%
  filter(in_lineup, pitcher) %>%
  group_by(player_id, team_id, player, matchup_id) %>%
  summarise('n_games'  = sum(relief),
            'n_points' = sum(points[relief & !start]), .groups = 'drop') %>%
  filter(n_games > 0, !is.na(n_games)) %>%
  mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
         'ppg' = n_points / n_games) %>%
  group_by(matchup_id) %>%
  arrange(-n_points, n_games) %>%
  dplyr::slice(1:10) %>%
  ungroup() %>%
  group_by(player) %>%
  mutate('n' = n()) %>%
  ungroup() %>%
  mutate('player' = paste0(player, ' (', n, ')')) %>%
  select(matchup_id, team_id, player, player_url, n_games, n_points)

withr::with_dir(app_dir, {
  library(gt)
  source('helpers.R')
  teams_top <- change_logo(read_csv(glue('data/stats/{season}/teams_{season}.csv'),
                                    show_col_types = FALSE))

  all_weeks_tp    <- df_start$matchup_id[df_start$matchup_id <= current_matchup]
  html_exists_tp  <- file.exists(glue('data/cache/top_performers_{season}_week_{all_weeks_tp}.html'))
  weeks_to_render_tp <- unique(c(
    all_weeks_tp[!html_exists_tp],
    current_matchup,
    max(1L, current_matchup - 1L)
  ))

  for(w in sort(weeks_to_render_tp)) {
    df_bat_w <-
      bat_top_all %>%
      filter(matchup_id == w) %>%
      left_join(select(teams_top, team_id, team, logo), by = 'team_id') %>%
      select(player, player_url, team, logo, n_points)
    if(nrow(df_bat_w) < 10) df_bat_w[max(1, nrow(df_bat_w) + 1):10, ] <- NA
    names(df_bat_w) <- paste0(names(df_bat_w), '_bat')

    df_sp_w <-
      sp_top_all %>%
      filter(matchup_id == w) %>%
      left_join(select(teams_top, team_id, team, logo), by = 'team_id') %>%
      select(player, player_url, team, logo, n_games, ppg)
    if(nrow(df_sp_w) < 10) df_sp_w[max(1, nrow(df_sp_w) + 1):10, ] <- NA
    names(df_sp_w) <- paste0(names(df_sp_w), '_sp')

    df_rp_w <-
      rp_top_all %>%
      filter(matchup_id == w) %>%
      left_join(select(teams_top, team_id, team, logo), by = 'team_id') %>%
      select(player, player_url, team, logo, n_games, n_points)
    if(nrow(df_rp_w) < 10) df_rp_w[max(1, nrow(df_rp_w) + 1):10, ] <- NA
    names(df_rp_w) <- paste0(names(df_rp_w), '_rp')

    df_top_w <- bind_cols(df_bat_w, df_sp_w, df_rp_w)

    gt_top_w <-
      gt(df_top_w) %>%
      cols_align(align = 'center', columns = everything()) %>%
      tab_spanner(label = 'Batting',          columns = contains('_bat')) %>%
      tab_spanner(label = 'Starting Pitching', columns = contains('_sp')) %>%
      tab_spanner(label = 'Relief Pitching',  columns = contains('_rp')) %>%
      sub_missing(columns = everything(), missing_text = '---') %>%
      tab_style(
        style = list(cell_borders(sides = 'bottom', color = 'black', weight = px(3))),
        locations = list(cells_column_labels(columns = gt::everything()))
      ) %>%
      text_transform(
        locations = cells_body(contains(c('player_url'))),
        fn = function(x) web_image(url = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('logo_rp')), rows = (logo_rp != '---')),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('logo_sp')), rows = (logo_sp != '---')),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      text_transform(
        locations = cells_body(columns = contains(c('logo_bat')), rows = (logo_bat != '---')),
        fn = function(x) local_image(filename = x, height = 50)
      ) %>%
      tab_style(
        style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
        locations = list(cells_body(columns = contains('points')))
      ) %>%
      tab_style(
        style = list(cell_borders(sides = 'right', color = 'black', weight = px(3))),
        locations = list(cells_body(columns = contains('ppg')))
      ) %>%
      cols_label(
        'team_bat' = 'Team', 'logo_bat' = '', 'player_url_bat' = '', 'player_bat' = 'Player',
        'n_points_bat' = 'Points',
        'team_sp' = 'Team', 'logo_sp' = '', 'player_url_sp' = '', 'player_sp' = 'Player',
        'ppg_sp' = 'Points/Start', 'n_games_sp' = '# of Starts',
        'team_rp' = 'Team', 'logo_rp' = '', 'player_url_rp' = '', 'player_rp' = 'Player',
        'n_points_rp' = 'Points', 'n_games_rp' = '# of Appearances'
      ) %>%
      tab_header(title    = md('**Top Performances**'),
                 subtitle = md(glue('**Week {w}**'))) %>%
      tab_options(column_labels.font.size = 20, heading.title.font.size = 40,
                  heading.subtitle.font.size = 40, heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold', column_labels.font.weight = 'bold') %>%
      tab_source_note('Numbers next to player denotes the # of appearances in the weekly top 10')

    writeLines(as_raw_html(gt_top_w),
               glue('data/cache/top_performers_{season}_week_{w}.html'))
    rm(df_bat_w, df_sp_w, df_rp_w, df_top_w, gt_top_w)
  }

  rm(teams_top)
})

rm(bat_top_all, sp_top_all, rp_top_all)

cat('cache_files.R complete\n')
