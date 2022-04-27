# tp <- team_points 
df_all <- NULL
for(i in 1:14) {
  team_points <- 
    tp %>% 
    mutate('adj_pts' = ifelse(matchup_id > i, NA, adj_pts))
  
  ### playoff simulations
  mu <- mean(team_points$adj_pts, na.rm = T)
  sigma <- sd(team_points$adj_pts, na.rm = T)
  
  team_mus <- 
    group_by(team_points, team) %>% 
    summarise("mean_pts" = mean(adj_pts, na.rm = T),
              "games_played" = max(matchup_id[!is.na(adj_pts)])) %>% 
    mutate("team_mu" = min(1, games_played/20) * mean_pts  + max(0, (1 - games_played/20)) * mu) %>% 
    pull(team_mu)
  
  team_sigmas <- 
    group_by(team_points, team) %>% 
    summarise("sd_pts" = sd(adj_pts, na.rm = T),
              "games_played" = max(matchup_id[!is.na(adj_pts)])) %>% 
    mutate("team_sigma" = min(1, games_played/20) * sd_pts  + max(0, (1 - games_played/20)) * sigma) %>% 
    pull(team_sigma)
  team_sigmas[is.na(team_sigmas)] <- sigma
  
  names(team_mus) <- sort(unique(team_points$team))
  names(team_sigmas) <- sort(unique(team_points$team))
  
  na_ix <- schedule$matchup_id > i
  
  
  df_sims <- future_map_dfr(1:params$nsims, sim_season)
  
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
              suffix = c("", "_opp")) %>% 
    filter(team != team_opp)  %>% 
    left_join(select(teams, team_id, division_id), by  = 'team_id')
  
  
  
  x <- 
    df_sims %>% 
    group_by(team, division_id, sim_id) %>% 
    summarise('wins' = sum(total_points > total_points_opp),
              'points' = sum(total_points)) %>% 
    ungroup() 
  
  x <- 
    group_by(x, sim_id, division_id) %>% 
    mutate("division_winner" = get_division_winner(wins, points)) %>% 
    ungroup() %>% 
    group_by(sim_id) %>% 
    mutate("wild_card" = get_wild_card(wins, points, division_winner))
  
  
  sim_results <- 
    group_by(x, team, division_id) %>% 
    summarise("mean_wins" = round(mean(wins), 1),
              "mean_pts" = round(mean(points)),
              "win_div" = mean(division_winner),
              "wild_card" = mean(wild_card)) %>% 
    ungroup() %>% 
    mutate("playoffs" = win_div + wild_card,
           'division_id' = as.character(division_id)) %>% 
    arrange(desc(playoffs), desc(mean_wins)) %>% 
    mutate('matchup_id' = i)
  
  df_all <- bind_rows(df_all, sim_results)
}
