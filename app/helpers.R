### helpers.R
### Helper Functions for Shiny App
### Updated Dec 2023

### Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### Function to help w/ logos in gt tables
change_logo <- function(df, team_cols = 'team', cols = 'logo') {
  for(i in 1:length(cols)) {
    df[[ cols[i] ]] <- ifelse(grepl('mystique', df[[ cols[i] ]]), paste0(df[[ cols[i] ]], '.jpg'), df[[ cols[i] ]])
    df[[ cols[i] ]] <- ifelse(grepl('lm-static', df[[ cols[i] ]]), paste0(df[[ cols[i] ]], '.jpg'), df[[ cols[i] ]])
    df[[ cols[i] ]] <- paste0('www/', df[[ team_cols[i] ]], str_sub(df[[ cols[i] ]], -4, -1))
    df[[ cols[i] ]] <- gsub('=254', '.png', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('\\:', '', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('Blasphemous Hot Takes.png', 'Blasphemous Hot Takes.jpg', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('Elly De La Snooze.png', 'Elly De La Snooze.jpg', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('All That is Wright.gif', 'All That is Wright.jpg', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('Takin\' Care of Rizzness.png', 'Takin\' Care of Rizzness.jpg', df[[ cols[i] ]])
    df[[ cols[i] ]] <- gsub('Tucker\\?', 'Tucker', df[[ cols[i] ]])
    
    ### Check images that don't exist
    ix <- df[[ cols[i] ]]  %in% dir('www', full.names = T)
    df[[ cols[i] ]][!ix] <- 'www/error.jpg'
    
    
  }
  return(df)
}

### Clean HTML
html_clean <- function(html) {
  html <- tools::toTitleCase(gsub('\\+', ' ', gsub('%27s', '\'', html)))
  print(html)
  return(html)
}


pad_rows <- function(df, n) {
  rows_needed <- n - nrow(df)
  if (rows_needed > 0) {
    df <- bind_rows(df, tibble::tibble(.rows = rows_needed))
  }
  df
}

### Rolling K avg on plots
plot_k_avg <- function(k) {
  offset <- as.numeric(params$opening_day_chart - params$opening_day)
  df_bat_stocks <- 
    df_daily %>%
    filter(scoring_period_id <= 2 | scoring_period_id >= offset + 1) %>%
    filter(in_lineup) %>% 
    filter(batter) %>% 
    select(team_id, scoring_period_id, points, played, start) %>% 
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
      y[x >= (k + offset) & is.na(y)] <- 0
      tmp <- approx(x, y, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_ppg' = tmp$y)
    }) %>% 
    ungroup() %>% 
    inner_join(teams %>% select(team, team_id), by = 'team_id')
  
  
  
  df_pitch_stocks <- 
    df_daily %>% 
    filter(scoring_period_id <= 2 | scoring_period_id >= offset + 1) %>%
    filter(in_lineup) %>% 
    filter(pitcher) %>% 
    filter(!relief) %>% 
    select(team_id, scoring_period_id, points, qs, start) %>% 
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
      y[x >= (k + offset) & is.na(y)] <- 0
      z[x >= (k + offset) & is.na(z)] <- 0
      tmp <- approx(x, y, n = 500)
      tmp2 <- approx(x, z, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_ppg' = tmp$y,
             'roll_qs_pct' = tmp2$y)
    }) %>% 
    ungroup() %>% 
    inner_join(teams %>% select(team, team_id), by = 'team_id')
  
  df_rp_stocks <- 
    df_daily %>% 
    filter(scoring_period_id <= 2 | scoring_period_id >= offset + 1) %>% 
    filter(in_lineup) %>% 
    filter(pitcher) %>% 
    filter(!start) %>% 
    
    select(team_id, scoring_period_id, points, relief, relief_start) %>% 
    group_by(team_id, scoring_period_id) %>% 
    summarise('n_points' = sum(points),
              'n_games' =  sum(relief & !relief_start),
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
      y[x >= (k + offset) & is.na(y)] <- 0
      tmp <- approx(x, y, n = 500)
      tibble('team_id' = .x$team_id[1],
             'scoring_period_id' = tmp$x,
             'roll_points' = tmp$y)
    }) %>% 
    ungroup() %>%
    inner_join(teams %>% select(team, team_id), by = 'team_id')
  
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
  
  rm(df_bat_stocks)
  rm(df_pitch_stocks)
  rm(df_rp_stocks)
  gc()
  
  return( (p1 + p2)/(p3 + p4) )
  
}





roll_avg_plot <- function(total_stats, date_range, player, player_id, stat_col, rolling_sum, starter_only, roll_k, batter) {
  
  if(!rolling_sum) {
    roller <- zoo::rollmean
  } else {
    roller <- zoo::rollsum
  }
  
  
  ### League Stats in Date range
  total_stats <-
    total_stats %>%
    filter(starter | !starter_only) %>%
    filter(date >= date_range[1],
           date <= date_range[2])
  
  if(!batter & !starter_only) {
    total_stats <-
      total_stats %>%
      filter(!starter)
  }
  
  
  ### Player Stats
  ### Filter by mlbam_id because some players have the same name
  player_stats <-
    total_stats %>%
    filter(mlbam_id == player_id)
  
  ### Player Rolling Average
  player_avg <-
    player_stats %>%
    mutate(across(.cols = stat_col,
                  .names = 'roll_stat',
                  .fns =  ~roller(.x, k = roll_k, na.pad = T, align = 'right'))) %>%
    rename_at(.vars = vars(stat_col), ~'stat_col') 
  
  
  ### League Rolling Average
  league_avg <-
    total_stats %>%
    group_by(date) %>%
    summarise_at(.vars = stat_col,
                 .funs = list('stat_sum' = ~sum(.x, na.rm = T),
                              'n_games' = ~length(.x))) %>%
    mutate('roll_sum' =  zoo::rollsum(stat_sum, k = roll_k, na.pad = T, align = 'right'),
           'roll_games' = zoo::rollsum(n_games, k = roll_k, na.pad = T, align = 'right')) %>%
    mutate('roll_stat' =
             case_when(rolling_sum ~ roll_sum/roll_games * roll_k,
                       !rolling_sum ~ roll_sum/roll_games))
  
  player_avg <-
    player_avg %>% 
    full_join(league_avg %>% distinct(date), by = 'date')
  
  
  ggplot(league_avg, aes(x = date, y = roll_stat))  +
    geom_col(data = player_avg, aes(y = stat_col), fill = 'steelblue', alpha = 0.4) +
    geom_line(aes(lty = 'league', color = 'league')) +
    geom_point(data = player_avg %>% filter(!is.na(stat_col)), aes(color = 'player')) +
    geom_line(data = player_avg %>% filter(!is.na(stat_col)), aes(lty = 'player', color = 'player')) +
    scale_linetype_manual(values = c(2,1), labels = c('Rolling League Average/Sum', 'Rolling Player Average/Sum')) +
    scale_color_manual(values = c('black', 'firebrick'), labels = c('Rolling League Average/Sum', 'Rolling Player Average/Sum')) +
    theme(legend.position = 'bottom',
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 16)) + 
    labs(x = 'Date',
         y = 'Daily Total',
         color = '',
         linetype = '',
         title = paste(roll_k, 'Game Rolling', ifelse(rolling_sum, 'Total:', 'Average:'), ifelse(batter, names(bat_stat_cols)[bat_stat_cols == stat_col], names(pitch_stat_cols)[pitch_stat_cols == stat_col])),
         subtitle = player_avg$player[1]
    )
}
