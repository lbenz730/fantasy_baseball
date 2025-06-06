stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

batting_points_ix <- as.character(c(3, 4, 5, 70, 7, 72, 10, 11, 12, 20, 21, 23, 26, 27))

### Helper Functions
get_batting_points <- function(row) {
  pitchers <- map_lgl(row$playerPoolEntry$player$eligibleSlots, ~(any(13:15 %in% .x)))
  shohei_ix <- which(row$playerPoolEntry$player$fullName == 'Shohei Ohtani')
  if(length(shohei_ix) > 0) {
    shohei_stats <- row$playerPoolEntry$player$stats[[shohei_ix]] 
    shohei_batting_points <- sum(shohei_stats$appliedStats %>% select(any_of(batting_points_ix)))
    return(sum(row$playerPoolEntry$appliedStatTotal[!pitchers]) + shohei_batting_points)
  }
  
  return(sum(row$playerPoolEntry$appliedStatTotal[!pitchers]))
}

get_pitching_points <- function(row) {
  pitchers <- map_lgl(row$playerPoolEntry$player$eligibleSlots, ~(any(13:15 %in% .x)))
  shohei_ix <- which(row$playerPoolEntry$player$fullName == 'Shohei Ohtani')
  if(length(shohei_ix) > 0) {
    shohei_stats <- row$playerPoolEntry$player$stats[[shohei_ix]] 
    shohei_batting_points <- sum(shohei_stats$appliedStats %>% select(any_of(batting_points_ix)))
    return(sum(row$playerPoolEntry$appliedStatTotal[pitchers]) - shohei_batting_points)
  }
  
  return(sum(row$playerPoolEntry$appliedStatTotal[pitchers]))
}

rp_points_by_game <- function(roster, team_ix) {
  if(length(roster) == 0) {
    return(tibble('team_ix' = team_ix))
  }
  totals <- roster$playerPoolEntry$appliedStatTotal
  player <- roster$playerPoolEntry$player$fullName
  player_id <- roster$playerPoolEntry$player$id
  games  <- map_dbl(roster$playerPoolEntry$player$stats, ~ifelse(is.null(.x$stats$`32`), NA, ifelse(.x$stats$`32` - .x$stats$`33` == 0,  NA, .x$stats$`32`)))
  
  shohei_ix <- which(roster$playerPoolEntry$player$fullName == 'Shohei Ohtani')
  totals[shohei_ix] <- NA
  
  tibble('team_ix' = team_ix,
         'matchup_id' = i,
         'player' = player,
         'player_id' = player_id,
         'n_games' = games,
         'n_points' = totals)
}

sp_points_by_game <- function(roster, team_ix) {
  if(length(roster) == 0) {
    return(tibble('team_ix' = team_ix))
  }
  totals <- roster$playerPoolEntry$appliedStatTotal
  player <- roster$playerPoolEntry$player$fullName
  player_id <- roster$playerPoolEntry$player$id
  games  <- map_dbl(roster$playerPoolEntry$player$stats, ~ifelse(is.null(.x$stats$`33`), NA, ifelse(.x$stats$`33` == 0, NA, .x$stats$`33`)))
  qs <- map_dbl(roster$playerPoolEntry$player$stats, ~ifelse(is.null(.x$stats$`63`), NA, .x$stats$`63`))
  
  shohei_ix <- which(roster$playerPoolEntry$player$fullName == 'Shohei Ohtani')
  if(length(shohei_ix) > 0) {
    shohei_stats <- roster$playerPoolEntry$player$stats[[shohei_ix]] 
    shohei_batting_points <- sum(shohei_stats$appliedStats %>% select(any_of(batting_points_ix)))
    totals[shohei_ix] <- totals[shohei_ix] - shohei_batting_points
  }
  
  tibble('team_ix' = team_ix,
         'matchup_id' = i,
         'player' = player,
         'player_id' = player_id,
         'n_games' = games,
         'n_points' = totals,
         'n_qs' = qs)
}


batting_points_by_game <- function(roster, team_ix) {
  if(length(roster) == 0) {
    return(tibble('team_ix' = team_ix))
  }
  totals <- roster$playerPoolEntry$appliedStatTotal
  player <- roster$playerPoolEntry$player$fullName
  player_id <- roster$playerPoolEntry$player$id
  games  <- map_dbl(roster$playerPoolEntry$player$stats, ~ifelse(is.null(.x$stats$`81`), NA, .x$stats$`81`))
  
  shohei_ix <- which(roster$playerPoolEntry$player$fullName == 'Shohei Ohtani')
  if(length(shohei_ix) > 0) {
    shohei_stats <- roster$playerPoolEntry$player$stats[[shohei_ix]] 
    shohei_batting_points <- sum(shohei_stats$appliedStats %>% select(any_of(batting_points_ix)))
    totals[shohei_ix] <- shohei_batting_points
  }
  
  tibble('team_ix' = team_ix,
         'matchup_id' = i,
         'player' = player,
         'player_id' = player_id,
         'n_games' = games,
         'n_points' = totals)
}


robust_scrape <- function(file) {
  fails <- 0
  while(fails < 1000) {
    json <- suppressWarnings(try(fromJSON(read_lines(file))))
    if(all(class(json) != 'try-error')) {
      return(json)
    } else {
      fails <- fails + 1
    }
  }
}



### Playoffs
### Simulation Helper Functions
sim_season <- function(i) {
  df_sim <- schedule
  # df_sim$home_total_points[na_ix] <- map_dbl(team_mus[df_sim$home_team[na_ix]], ~rnorm(1, .x, sigma))
  # df_sim$away_total_points[na_ix] <- map_dbl(team_mus[df_sim$away_team[na_ix]], ~rnorm(1, .x, sigma))
  
  df_sim$home_total_points[na_ix] <- map2_dbl(team_mus[df_sim$home_team[na_ix]],
                                              team_sigmas[df_sim$home_team[na_ix]],
                                              ~rnorm(1, .x, .y))
  df_sim$away_total_points[na_ix] <- map2_dbl(team_mus[df_sim$away_team[na_ix]],
                                              team_sigmas[df_sim$away_team[na_ix]],
                                              ~rnorm(1, .x, .y))
  
  df_sim$sim_id <- i
  return(df_sim)
}

get_playoffs <- function(wins, points) {
  zeros <- rep(0, 12)
  ix <- order(wins, points, decreasing = T)[1:4]
  zeros[ix] <- 1
  return(zeros)
}

get_last_place <- function(wins, points) {
  zeros <- rep(0, 12)
  ix <- order(wins, points, decreasing = F)[1]
  zeros[ix] <- 1
  return(zeros)
}

get_playoff_seed <- function(wins, points) {
  zeros <- rep(NA, 12)
  ix <- order(wins, points, decreasing = T)[1:4]
  zeros[ix] <- 1:4
  return(zeros)
}

edit_wp <- function(df_sims, df_wp, team_mus, team_sigmas) {
  df <- 
    df_sims %>% 
    filter(matchup_id == params$matchup_id) %>% 
    inner_join(df_wp %>% select('home_team' = team_home,
                                'away_team' = team_away,
                                'win_prob' = win_prob)) %>% 
    mutate('winner' = ifelse(runif(nrow(.)) <= win_prob, home_team, away_team)) %>% 
    mutate('mu' = ifelse(winner == home_team,
                         team_mus[home_team],
                         team_mus[away_team])) %>% 
    mutate('sigma' = ifelse(winner == home_team,
                            team_sigmas[home_team],
                            team_sigmas[away_team])) %>% 
    mutate('mu_l' = ifelse(winner != home_team,
                           team_mus[home_team],
                           team_mus[away_team])) %>% 
    mutate('sigma_l' = ifelse(winner != home_team,
                              team_sigmas[home_team],
                              team_sigmas[away_team])) %>% 
    mutate('winning_score' = rtruncnorm(n = nrow(.), 
                                        a = pmax(home_total_points, away_total_points, 0, na.rm = T),
                                        b = Inf,
                                        mean = mu,
                                        sd = sigma)) %>% 
    mutate('losing_score' = rtruncnorm(n = nrow(.), 
                                       a = pmin(home_total_points, away_total_points, 0, na.rm = T),
                                       b = winning_score,
                                       mean = mu_l,
                                       sd = sigma_l)) %>% 
    mutate('home_total_points' = ifelse(winner == home_team,
                                        winning_score,
                                        losing_score),
           'away_total_points' = ifelse(winner != home_team,
                                        winning_score,
                                        losing_score)) %>% 
    select(all_of(names(df_sims)))
  return(df)
}

championship_sim <- function(playoff_teams, team_mus, team_sigmas, matchup_id, wp) {
  if(matchup_id <= reg_season) {
    p1 <- 
      pnorm(q = 0, 
            mean = 2 * (team_mus[playoff_teams[1]] - team_mus[playoff_teams[4]]),
            sd = 2 * (team_sigmas[playoff_teams[1]] + team_sigmas[playoff_teams[4]]),
            lower.tail = F)
    
    p2 <- 
      pnorm(q = 0, 
            mean = 2 * (team_mus[playoff_teams[2]] - team_mus[playoff_teams[3]]),
            sd = 2 * (team_sigmas[playoff_teams[2]] + team_sigmas[playoff_teams[3]]),
            lower.tail = F)
    
    if(runif(1) <= p1) {
      winner1 <- playoff_teams[1]
    } else {
      winner1 <- playoff_teams[4]
    }
    
    if(runif(1) <= p2) {
      winner2 <- playoff_teams[2]
    } else {
      winner2 <- playoff_teams[3]
    }
    
    p3 <- 
      pnorm(q = 0, 
            mean = 2 * (team_mus[winner1] - team_mus[winner2]),
            sd = 2 * (team_sigmas[winner1] + team_sigmas[winner2]),
            lower.tail = F)
    
    if(runif(1) <= p3) {
      winner <- winner1
    } else {
      winner <- winner2
    }
    
    return(winner)
    
  } else if(matchup_id == (reg_season + 1)) {
    p1 <- wp[1]
    p2 <- wp[2]
    
    if(runif(1) <= p1) {
      winner1 <- playoff_teams[1]
    } else {
      winner1 <- playoff_teams[4]
    }
    
    if(runif(1) <= p2) {
      winner2 <- playoff_teams[2]
    } else {
      winner2 <- playoff_teams[3]
    }
    
    p3 <- 
      pnorm(q = 0, 
            mean = 2 * (team_mus[winner1] - team_mus[winner2]),
            sd = 2 * (team_sigmas[winner1] + team_sigmas[winner2]),
            lower.tail = F)
    
    if(runif(1) <= p3) {
      winner <- winner1
    } else {
      winner <- winner2
    }
    
  } else if(matchup_id == (reg_season + 2)) {
    if(runif(1) <= wp) {
      winner <- playoff_teams[1]
    } else {
      winner <- playoff_teams[2]
    }
  }
  return(winner)
}


ferry <- '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Spirit_of_America_-_Staten_Island_Ferry.jpg/1280px-Spirit_of_America_-_Staten_Island_Ferry.jpg" style="height:30px;">'

update_daily_stats <- function() {
  df_daily <- 
    read_csv(here(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))) %>% 
    filter(matchup_id < params$matchup_id) %>%  
    bind_rows(get_matchup_stats(params$matchup_id, season = params$season)) 
  write_csv(df_daily, glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))
  return(df_daily)
  
}

publish <- function() {
  rsconnect::rpubsUpload(title = 'Millburn Fantasy', 
                         contentFile = 'fantasy.html',
                         originalDoc = 'fantasy.Rmd',
                         id = 'https://api.rpubs.com/api/v1/document/905951/1f88063ceeae401cb4bf80f1450a6961') 
}

remove_postcap <- function(df_daily) {
  df_cap <- 
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
    mutate('over_start_cap' = total_starts > start_cap & lag(total_starts) >= start_cap & lag(total_starts, 2) <= start_cap ) %>% 
    filter(over_start_cap) %>% 
    ungroup() %>% 
    select(matchup_id, team_id, scoring_period_id) %>% 
    mutate('lineup_id' = 14,
           'start' = T)
  
  df_daily <- 
    df_daily %>% 
    anti_join(df_cap)
  
  return(df_daily)
  
}

sp_remove_postcap <- function(sp_points) {
  df_cap <- 
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
    mutate('over_start_cap' = total_starts > start_cap & lag(total_starts) >= start_cap & lag(total_starts, 2) <= start_cap ) %>% 
    filter(over_start_cap) %>% 
    ungroup() %>% 
    select(matchup_id, team_id, scoring_period_id) %>% 
    mutate('lineup_id' = 14,
           'start' = T)
  
  df_edit <- 
    df_daily %>% 
    inner_join(df_cap) %>% 
    select(matchup_id, player, player_id, 'n_points' = points, 'n_qs' = qs)
  
  for(i in 1:nrow(df_edit)) {
    ix <- which(sp_points$player_id == df_edit$player_id[i] & sp_points$matchup_id == df_edit$matchup_id[i])
    sp_points$n_games[ix] <- sp_points$n_games[ix] - 1
    sp_points$n_points[ix] <- sp_points$n_points[ix] - df_edit$n_points[i]
    sp_points$n_qs[ix] <- sp_points$n_qs[ix] - as.numeric(df_edit$n_qs[i])
  }
  return(sp_points)
  
}
