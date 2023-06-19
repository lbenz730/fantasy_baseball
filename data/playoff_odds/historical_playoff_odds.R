tp <- team_points
df_all <-  
  tibble('team' = teams$team,
         'playoffs' = 4/12,
         'last_place' = 1/12,
         'champ' = 1/12,
         'matchup_id' = 0,
         'mean_pts' = NA)
for(i in 1:11) {
  cat(i)
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
  
  
  df_sims <- future_map_dfr(1:params$nsims, sim_season, .options = furrr_options(seed = 12))
  
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
      if(params$matchup_id < 22) {
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
              'champ' = mean(champions == team),
              'matchup_id' = i)
  
  df_all <- bind_rows(df_all, sim_results)
}
