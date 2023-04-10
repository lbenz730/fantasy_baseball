library(tidyverse)
library(glue)
library(lubridate)
library(ggridges)
library(ggimage)
library(rsvg)
library(purrr)
library(gt)
library(stringr)
library(ggbump)
library(patchwork)

source('figures/wp_graphics.R')


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



change_logo <- function(df) {
  df$logo <- paste0('www/', df$team, str_sub(df$logo, -4, -1))
  df$logo <- gsub('=254', '.png', df$logo) 
  return(df)
}


theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  plot.subtitle = element_text(size = 16, hjust = 0.5),
                  strip.text = element_text(size = 12),
                  legend.position = "none")
)


### Ferry Logo
ferry <- '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Spirit_of_America_-_Staten_Island_Ferry.jpg/1280px-Spirit_of_America_-_Staten_Island_Ferry.jpg" style="height:30px;">'

### Parameters
params <- 
  list('season' = 2023,
       'opening_day' = as.Date('2023-03-30'))

period <- as.numeric(as.Date(substring(as.POSIXct(Sys.time(), tz="EST") - 5 * 60 * 60, 1, 10)) - params$opening_day) + 1
print(period)


df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season == params$season) 

params$current_matchup <- max(df_start$matchup_id[df_start$start_period <= period])


#### Read in Data Sets
teams <- change_logo(read_csv(glue('data/stats/{params$season}/teams_{params$season}.csv')))
exp_standings <- change_logo(read_csv(glue('data/stats/{params$season}/exp_standings.csv')))
sim_results <- 
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>% 
  filter(matchup_id == params$current_matchup)

batter_points <- read_csv(glue('data/stats/{params$season}/batting_weekly_{params$season}.csv'))
sp_points <- read_csv(glue('data/stats/{params$season}/sp_weekly_{params$season}.csv'))
rp_points <- read_csv(glue('data/stats/{params$season}/rp_weekly_{params$season}.csv'))
df_daily <- read_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))
df_log <- read_csv(glue('figures/top_performers/{params$season}/best_lineup/best_lineups.csv'))
team_points <- read_csv(glue('data/stats/{params$season}/team_points.csv'))
history <- read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv'))
df_penalty <- read_csv('data/red_flags/penalties.csv') %>% 
  mutate('scoring_period_id' = as.numeric(scoring_period_id),
         'penalty' = as.numeric(penalty),
         'matchup_id' = as.numeric(matchup_id))

df_rp_penalty <- 
  read_csv('data/red_flags/rp_penalties.csv') %>% 
  mutate('penalty' = as.numeric(penalty),
         'matchup_id' = as.numeric(matchup_id))

pitch_stats <- 
  read_csv(glue('data/stats/{params$season}/pitch_stats.csv')) %>% 
  inner_join(select(teams, team, team_id, logo))

df_trades <- read_csv(glue('data/stats/{params$season}/trades_{params$season}.csv'))
if(nrow(df_trades) > 0) {
  traded_players <- read_csv(glue('data/stats/{params$season}/traded_players_{params$season}.csv'))
}

trans_log <- read_csv(glue('data/stats/{params$season}/transaction_log_{params$season}.csv'))


distributions <- 
  read_csv('data/playoff_odds/distributions.csv') %>% 
  inner_join(
    sim_results %>% 
      select(team, mean_wins, mean_pts)
  )

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
  inner_join(teams) %>% 
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
  filter(in_lineup) %>% 
  filter(matchup_id < params$current_matchup) %>% 
  group_by(team_id, matchup_id) %>% 
  summarise('n_bat' = sum(played),
            'n_rp' = sum(relief & !start)) %>% 
  inner_join(df_start) %>% 
  mutate('n_rp' = n_rp/duration * 7,
         'n_bat' = n_bat/duration * 7) %>% 
  ungroup() %>% 
  summarise('n_sp' = 8/6,
            'n_rp' = mean(n_rp)/3,
            'n_bat' = mean(n_bat)/13)



plot_k_avg <- function(k) {
  df_bat_stocks <- 
    df_daily %>% 
    filter(in_lineup) %>% 
    filter(batter) %>% 
    group_by(team_id, scoring_period_id) %>% 
    summarise('n_points' = sum(points),
              'n_games' = sum(played) + sum(start),
              'ppg' = n_points/(n_games + 0.000001)) %>% 
    mutate('roll_points' = zoo::rollsum(n_points, k = k, na.pad = T, align = 'right'),
           'roll_games' = zoo::rollsum(n_games, k = k, na.pad = T, align = 'right'),
           'roll_ppg' = roll_points/roll_games) %>% 
    group_split() %>% 
    map_dfr(~{
      x <- .x$scoring_period_id
      y <- .x$roll_ppg
      y[x >= k & is.na(y)] <- 0
      tmp <- approx(x, y, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_ppg' = tmp$y)
    }) %>% 
    ungroup() %>% 
    inner_join(teams)
  
  
  
  df_pitch_stocks <- 
    df_daily %>% 
    filter(in_lineup) %>% 
    filter(pitcher) %>% 
    filter(!relief) %>% 
    group_by(team_id, scoring_period_id) %>% 
    summarise('n_points' = sum(points),
              'n_games' =  sum(start),
              'n_qs' = sum(qs),
              'ppg' = n_points/n_games) %>% 
    mutate('roll_points' = zoo::rollsum(n_points, k = k, na.pad = T, align = 'right'),
           'roll_games' = zoo::rollsum(n_games, k = k, na.pad = T, align = 'right'),
           'roll_qs' = zoo::rollsum(n_qs, k = k, na.pad = T, align = 'right'),
           'roll_ppg' = roll_points/roll_games,
           'roll_qs_pct' = roll_qs/roll_games) %>% 
    group_split() %>% 
    map_dfr(~{
      x <- .x$scoring_period_id
      y <- .x$roll_ppg
      z <- .x$roll_qs_pct
      y[x >= k & is.na(y)] <- 0
      z[x >= k & is.na(z)] <- 0
      tmp <- approx(x, y, n = 500)
      tmp2 <- approx(x, z, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_ppg' = tmp$y,
             'roll_qs_pct' = tmp2$y)
    }) %>% 
    ungroup() %>% 
    inner_join(teams) 
  
  df_rp_stocks <- 
    df_daily %>% 
    filter(in_lineup) %>% 
    filter(pitcher) %>% 
    filter(!start) %>% 
    group_by(team_id, scoring_period_id) %>% 
    summarise('n_points' = sum(points),
              'n_games' =  sum(relief),
              'n_qs' = sum(qs),
              'ppg' = n_points/n_games) %>% 
    mutate('roll_points' = zoo::rollsum(n_points, k = k, na.pad = T, align = 'right'),
           'cum_points' = cumsum(n_points),
           'roll_games' = zoo::rollsum(n_games, k = k, na.pad = T, align = 'right'),
           'roll_ppg' = roll_points/roll_games) %>% 
    filter(scoring_period_id >= k) %>% 
    group_split() %>% 
    map_dfr(~{
      x <- .x$scoring_period_id
      y <- .x$roll_points
      y[x >= k & is.na(y)] <- 0
      tmp <- approx(x, y, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_points' = tmp$y)
    }) %>% 
    ungroup() %>%
    inner_join(teams) 
  
  p1 <-
    ggplot(df_bat_stocks, aes(x = scoring_period_id, y = roll_ppg)) + 
    facet_wrap(~team) +
    geom_hline(yintercept = exp_standings$batting_ppg[13], lty = 2, alpha = 0.3) +
    geom_line(aes(col = roll_ppg), lwd = 1.2, lineend = 'round') + 
    scale_color_gradient2(low = 'blue', mid = 'grey', high = 'red', midpoint = exp_standings$batting_ppg[13]) +
    theme(strip.text = element_text(size = 10)) + 
    labs(x = 'Day of Season',
         y = 'Batting Points/Game',
         title = glue('Batting PPG {k} Day Rolling Average'))
  
  
  p2 <-
    ggplot(df_pitch_stocks, aes(x = scoring_period_id, y = roll_ppg)) + 
    facet_wrap(~team) +
    geom_hline(yintercept = exp_standings$sp_ppg[13], lty = 2, alpha = 0.3) + 
    geom_line(aes(col = roll_ppg), lwd = 1.2, lineend = 'round') + 
    scale_color_gradient2(low = 'blue', mid = 'grey', high = 'red', midpoint = exp_standings$sp_ppg[13]) +
    theme(strip.text = element_text(size = 10)) + 
    labs(x = 'Day of Season',
         y = 'SP Points/Game',
         title = glue('Starting Pitching PPG {k} Day Rolling Average'))
  
  p3 <-
    ggplot(df_pitch_stocks, aes(x = scoring_period_id, y = roll_qs_pct)) + 
    facet_wrap(~team) +
    geom_hline(yintercept = exp_standings$qs_pct[13], lty = 2, alpha = 0.3) + 
    geom_line(aes(col = roll_qs_pct), lwd = 1.2, lineend = 'round') + 
    scale_color_gradient2(low = 'blue', mid = 'grey', high = 'red', midpoint = exp_standings$qs_pct[13]) +
    theme(strip.text = element_text(size = 10)) + 
    labs(x = 'Day of Season',
         y = 'QS %',
         title = glue('QS% {k} Day Rolling Average')) + 
    scale_y_continuous(labels = scales::percent)
  
  p4 <-
    ggplot(df_rp_stocks, aes(x = scoring_period_id, y = roll_points)) + 
    facet_wrap(~team) +
    geom_hline(yintercept = scale_factors$n_rp/7 * k * 3 * exp_standings$rp_ppg[13], lty = 2, alpha = 0.3) + 
    geom_line(aes(col = roll_points), lwd = 1.2, lineend = 'round') + 
    scale_color_gradient2(low = 'blue', mid = 'grey', high = 'red', midpoint = scale_factors$n_rp/7 * k * 3 * exp_standings$rp_ppg[13]) +
    theme(strip.text = element_text(size = 10)) + 
    labs(x = 'Day of Season',
         y = glue('RP Points/{k} Days'),
         title = glue('RP Points in Prev. {k} Days')) 
  
  return( (p1 + p2)/(p3 + p4) )
  
}

### Trades
if(nrow(df_trades) > 0) {
  traded_players <- 
    trans_log %>%
    filter(transaction_type == 'Trade')
} else {
  traded_players <- df_trades
}

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
    inner_join(trade_stats) %>% 
    inner_join(select(teams, team_id, team, logo), by = c('team_to' = 'team_id')) %>% 
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
    group_by(trade_id) %>% 
    mutate('n_rec' = map_dbl(team_to, ~sum(team_to == .x)))  %>% 
    mutate('n_given' = map_dbl(team_from, ~sum(team_to == .x)))  %>% 
    group_by(trade_id, team_to) %>% 
    group_split() %>% 
    map(~{ 
      if(.x$n_given[1]  > .x$n_rec[1]) {
        tmp <- .x[1,]
        tmp[1,] <- NA
        tmp$trade_id <- .x$trade_id
        bind_rows(.x, tmp)
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
  
  gt_trades <-
    df_trades %>% 
    select(-trade_id_1) %>% 
    gt() %>% 
    ### Align Columns
    cols_align(align = "center", columns = everything()) %>%
    
    tab_spanner(label = 'Before Trade ', columns = contains('_before_1')) %>%
    tab_spanner(label = 'Before Trade', columns = contains('_before_2')) %>%
    tab_spanner(label = 'After Trade ', columns = contains('_after_1')) %>%
    tab_spanner(label = 'After Trade', columns = contains('_after_2')) %>% 
    
    data_color(columns = contains('avg'),
               colors = scales::col_quantile(palette = ggsci::rgb_gsea(), 
                                             domain = c(-m, m),
                                             probs = c(0, seq(0.05, 0.95, 0.01), 1))
    ) %>%
    
    ### Round Numbers
    fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>% 
    fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
    fmt_number(columns = contains('value'), decimals = 2, sep_mark = '') %>% 
    sub_missing(columns = everything(), missing_text = "---") %>%
    
    
    
    ### Align Columns
    cols_align(align = "center", columns = everything()) %>%
    
    
    ### Borders
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
    
    ### Logos
    text_transform(
      locations = cells_body(columns = contains(c('player_url_1')),
                             rows = (!is.na(logo_1) & !is.na(player_url_1))),
      fn = function(x) {
        web_image(
          url = x,
          height = 50
        )
      }
    ) %>% 
    text_transform(
      locations = cells_body(columns = contains(c('player_url_2')),
                             rows = (!is.na(logo_2) & !is.na(player_url_2))),
      fn = function(x) {
        web_image(
          url = x,
          height = 50
        )
      }
    ) %>% 
    
    text_transform(
      locations = cells_body(columns = contains(c('logo_2')),
                             rows = (!is.na(logo_2) & !is.na(player_url_2))),
      fn = function(x) {
        local_image(
          filename = x,
          height = 50
        )
      }
    ) %>% 
    
    text_transform(
      locations = cells_body(columns = contains(c('logo_1')),
                             rows = (!is.na(logo_1) & !is.na(player_url_1))),
      fn = function(x) {
        local_image(
          filename = x,
          height = 50
        )
      }
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "black",
          weight = px(8)
        )
      ),
      locations = list(
        cells_body(
          columns = contains('total_value_after_1')
        )
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "black",
          weight = px(4)
        )
      ),
      locations = list(
        cells_body(
          columns = c(contains('ppg_vs_avg_before'), contains('logo'))
        )
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "black",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          columns = c(contains('ppg_vs_avg_after'))
        )
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = contains('avg_after_1'),
        rows = (is.na(points_after_1) | played_after_1 == 0)
      )
    ) %>%
    tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = contains('avg_after_2'),
        rows = (is.na(points_after_2) | played_after_2 == 0)
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "springgreen")),
      locations = cells_body(
        columns = total_value_after_1,
        rows = total_value_after_1 > total_value_after_2
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "tomato")),
      locations = cells_body(
        columns = total_value_after_1,
        rows = total_value_after_1 < total_value_after_2
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "springgreen")),
      locations = cells_body(
        columns = total_value_after_2,
        rows = total_value_after_1 < total_value_after_2
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "tomato")),
      locations = cells_body(
        columns = total_value_after_2,
        rows = total_value_after_1 > total_value_after_2
      )
    ) %>%
    
    tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = contains('avg_before_1'),
        rows = (is.na(points_before_1) | played_before_1 == 0)
      )
    ) %>%
    tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = contains('avg_before_2'),
        rows = (is.na(points_before_2) | played_before_2 == 0)
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "black",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          rows = which(df_trades$trade_id_1 != lead(df_trades$trade_id_1) | is.na(lead(df_trades$trade_id_1)))
        )
      )
    ) %>%
    
    ### Names
    cols_label(
      'player_1' = 'Player',
      'player_2' = 'Player',
      'player_url_1' = '',
      'player_url_2' = '',
      'team_1' = 'New Team',
      'team_2' = 'New Team',
      'logo_1' = '',
      'logo_2' = '',
      'points_before_1' = 'Points',
      'points_before_2' = 'Points',
      'points_after_1' = 'Points',
      'points_after_2' = 'Points',
      'played_before_1' = 'Games',
      'played_before_2' = 'Games',
      'played_after_1' = 'Games',
      'played_after_2' = 'Games',
      'ppg_before_1' = 'PPG',
      'ppg_before_2' = 'PPG',
      'ppg_after_1' = 'PPG',
      'ppg_after_2' = 'PPG',
      'total_value_after_1' = 'Total Trade Value',
      'total_value_after_2' = 'Total Trade Value',
      'ppg_vs_avg_before_1' = 'Value/Matchup',
      'ppg_vs_avg_before_2' = 'Value/Matchup',
      'ppg_vs_avg_after_1' = 'Value/Matchup',
      'ppg_vs_avg_after_2' = 'Value/Matchup') %>% 
    
    tab_header(title = 'Trade Analysis') %>%
    tab_footnote(footnote = "Value = Points/Week vs. League Average at Position (Only includes games played)",
                 locations = cells_column_labels(columns = contains('avg'))) %>% 
    tab_footnote(footnote = "Total value accumulated of all players in trade (points scored vs. points that league average players at position(s) would be expected to score in time since trade)",
                 locations = cells_column_labels(columns = contains('value'))) %>% 
    tab_source_note('Includes games until player dropped/traded in first stint with new team') %>% 
    tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_standings$sp_ppg[13]))) %>% 
    tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_standings$rp_ppg[13]))) %>% 
    tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_standings$batting_ppg[13]))) %>% 
    tab_options(column_labels.font.size = 16,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 40,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold',
                row_group.font.size  = 22)
} else {
  gt_trades <-
    gt(df_trades %>%dplyr::slice(0)) %>% 
    cols_label('team_id' = '',
               'scoring_period_id' = '',
               'matchup_id' = '') %>% 
    tab_header(title = 'Trade Analysis') %>%
    tab_footnote(footnote = "Value = Points/Week vs. League Average at Position (Only includes games played)",
                 locations = cells_column_labels(columns = contains('avg'))) %>% 
    tab_footnote(footnote = "Total value accumulated of all players in trade (points scored vs. points that league average players at position(s) would be expected to score in time since trade)",
                 locations = cells_column_labels(columns = contains('value'))) %>% 
    tab_source_note('Includes games until player dropped/traded in first stint with new team') %>% 
    tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_standings$sp_ppg[13]))) %>% 
    tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_standings$rp_ppg[13]))) %>% 
    tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_standings$batting_ppg[13]))) %>% 
    tab_options(column_labels.font.size = 16,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 40,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold',
                row_group.font.weight = 'bold',
                row_group.font.size  = 22)
  
}




threshold <- pmin(20, period/2)
threshold_p <- pmin(5, period/5)

df_fa <- 
  trans_log %>% 
  filter(!is.na(w_bat), !is.na(w_sp), !is.na(w_rp)) %>% 
  filter(transaction_type == 'Free Agent') %>% 
  filter(w_bat * n_games >= threshold | w_sp * n_games >= threshold_p  | w_rp * n_games >= threshold_p) %>% 
  mutate('ppg_vs_avg' = 
           (ppg - exp_standings$batting_ppg[13]) * scale_factors$n_bat * w_bat + 
           (ppg - exp_standings$rp_ppg[13]) * scale_factors$n_rp * w_rp + 
           (ppg - exp_standings$sp_ppg[13]) * scale_factors$n_sp * w_sp) %>% 
  mutate('total_value' = n_points - (end - start + 1)/7 * 
           (exp_standings$batting_ppg[13] * scale_factors$n_bat * w_bat + 
              exp_standings$rp_ppg[13] * scale_factors$n_rp * w_rp + 
              exp_standings$sp_ppg[13] * scale_factors$n_sp * w_sp)) %>% 
  mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
  inner_join(select(teams, team, team_id, logo)) %>% 
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

gt_fa <- 
  
  gt(df_fa) %>% 
  ### Align Columns
  cols_align(align = "center", columns = everything()) %>%
  
  ### Round Numbers
  fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>% 
  fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
  fmt_number(columns = contains('value'), decimals = 2, sep_mark = '') %>% 
  sub_missing(columns = everything(), missing_text = "---") %>%
  
  tab_spanner(label = 'Top 20 by Value ', columns = contains('_1')) %>%
  tab_spanner(label = 'Top 20 by Points', columns = contains('_2')) %>%
  
  
  ### Align Columns
  cols_align(align = "center", columns = everything()) %>%
  
  
  ### Borders
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
        weight = px(6)
      )
    ),
    locations = list(
      cells_body(
        columns = contains('total_value_1')
      )
    )
  ) %>%
  
  ### Logos
  text_transform(
    locations = cells_body(columns = contains(c('player_url'))),
    fn = function(x) {
      web_image(
        url = x,
        height = 50
      )
    }
  ) %>% 
  
  text_transform(
    locations = cells_body(columns = contains(c('logo'))),
    fn = function(x) {
      local_image(
        filename = x,
        height = 50
      )
    }
  ) %>% 
  
  ### Names
  cols_label(
    'player_1' = 'Player',
    'player_url_1' = '',
    'logo_1' = '',
    'n_points_1' = 'Points',
    'ppg_1' = 'PPG',
    'team_1' = 'Team',
    'total_value_1' = 'Total Value',
    'added_1' = 'Added',
    'dropped_1' = 'Dropped/Traded',
    'n_games_1' = '# of Games',
    
    'player_2' = 'Player',
    'player_url_2' = '',
    'logo_2' = '',
    'n_points_2' = 'Points',
    'ppg_2' = 'PPG',
    'team_2' = 'Team',
    'total_value_2' = 'Total Value',
    'added_2' = 'Added',
    'dropped_2' = 'Dropped/Traded',
    'n_games_2' = '# of Games'
  ) %>% 
  
  tab_header(title = 'Free Agent Analysis') %>%
  tab_footnote(footnote = "Total value= points scored vs. points that league average player at position would be expected to score in time since acquisition",
               locations = cells_column_labels(columns = contains('value'))) %>% 
  tab_source_note(paste("SP:", sprintf('%0.2f', scale_factors$n_sp), 'Games/Week | Average SP Game:', sprintf('%0.2f', exp_standings$sp_ppg[13]))) %>% 
  tab_source_note(paste("RP:", sprintf('%0.2f', scale_factors$n_rp), 'Games/Week | Average RP Game:', sprintf('%0.2f', exp_standings$rp_ppg[13]))) %>% 
  tab_source_note(paste("Batter:", sprintf('%0.2f', scale_factors$n_bat), 'Games/Week | Average Batter Game:', sprintf('%0.2f', exp_standings$batting_ppg[13]))) %>% 
  tab_source_note('Min Games for Inclusion: Batter (20), SP (5), RP (5), or 50% of the season for batters and 20% of the season for pitchers up until day 40') %>% 
  tab_source_note('NOTE: Additions prior to Opening Day do not count, as those are indistinguishable from draft picks in ESPN data') %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22)


### Bounce Around
df_sl <- 
  trans_log %>% 
  group_by(player, player_id) %>% 
  summarise('n_stints' = n_distinct(stint),
            'n_teams' = n_distinct(team_id),
            'points' = sum(n_points)) %>% 
  arrange(-n_stints, -n_teams, points) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
  select(player, player_url, n_stints, n_teams, points)

gt_fs <- 
  df_sl %>% 
  gt() %>% 
  ### Align Columns
  cols_align(align = "center", columns = everything()) %>% 
  fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
  ### Logos
  text_transform(
    locations = cells_body(columns = contains(c('player_url'))),
    fn = function(x) {
      web_image(
        url = x,
        height = 50
      )
    }
  ) %>% 
  cols_label(
    'player' = 'Player',
    'player_url' = '',
    'n_stints' = '# of Stints on Teams',
    'n_teams' = '# of Teams',
    'points' = 'Points Scored While Rostered'
  ) %>% 
  tab_header(title = 'Most Transacted Players') %>%
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22)

pitch_matrix <- 
  df_daily %>% 
  filter(in_lineup) %>% 
  filter(start | relief_start) %>% 
  mutate('ip' = case_when(p_cg > 0 ~ 'CG', 
                          p_outs < 9 ~ '< 3',
                          p_outs > 21 ~ '> 7',
                          T ~ paste(floor(p_outs/3), p_outs %% 3, sep = '.'))) %>% 
  mutate('earned_runs' = ifelse(p_er > 5, '5+', as.character(p_er))) %>% 
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
  inner_join(teams %>% select(team, team_id))


df_ps <- 
  pitch_stats %>% 
  arrange(era) %>% 
  select(team, logo, era, fip, k9, bb9, k_per_bb, hr9, qs, blue_balls) %>% 
  bind_rows(tibble('team' = 'League Average',
                   'logo' = 'www/League.png',
                   'era' = weighted.mean(pitch_stats$era, pitch_stats$outs),
                   'fip' = weighted.mean(pitch_stats$fip, pitch_stats$outs),
                   'k9' = weighted.mean(pitch_stats$k9, pitch_stats$outs),
                   'bb9' = weighted.mean(pitch_stats$bb9, pitch_stats$outs),
                   'hr9' = weighted.mean(pitch_stats$hr9, pitch_stats$outs),
                   'k_per_bb' = weighted.mean(pitch_stats$k_per_bb, pitch_stats$outs),
                   'qs' = mean(pitch_stats$qs),
                   'blue_balls' = mean(pitch_stats$blue_balls)))

### GT for Penaltys
df_penalty <- 
  df_penalty %>% 
  inner_join(select(teams, team, logo))

df_rp_penalty <- 
  df_rp_penalty %>% 
  inner_join(select(teams, team, logo))

gt_start_cap <- 
  df_penalty %>% 
  select(team, logo, matchup_id, penalty) %>% 
  gt() %>% 
  cols_align('center') %>% 
  text_transform(
    locations = cells_body(columns = contains(c('logo'))),
    fn = function(x) {
      local_image(
        filename = x,
        height = 50
      )
    }
  ) %>% 
  cols_label('team' = 'Team',
             'logo' = '',
             'matchup_id'  = 'Matchup',
             'penalty' = 'Penalty') %>% 
  tab_header(title = 'Start Cap Penalties') %>%
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22)

gt_rp_cap <- 
  df_rp_penalty %>% 
  select(team, logo, matchup_id, penalty) %>% 
  gt() %>% 
  cols_align('center') %>% 
  text_transform(
    locations = cells_body(columns = contains(c('logo'))),
    fn = function(x) {
      local_image(
        filename = x,
        height = 50
      )
    }
  ) %>% 
  cols_label('team' = 'Team',
             'logo' = '',
             'matchup_id'  = 'Matchup',
             'penalty' = 'Penalty') %>% 
  tab_header(title = 'RP Transaction Penalties') %>%
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 40,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold',
              row_group.font.weight = 'bold',
              row_group.font.size  = 22)

