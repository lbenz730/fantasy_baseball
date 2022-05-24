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

ferry <- '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Spirit_of_America_-_Staten_Island_Ferry.jpg/1280px-Spirit_of_America_-_Staten_Island_Ferry.jpg" style="height:30px;">'

update_daily_stats <- function() {
  df_daily <- 
    read_csv(here(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))) %>% 
    filter(matchup_id < params$matchup_id) %>%  
    bind_rows(get_matchup_stats(params$matchup_id, season = params$season)) 
  write_csv(df_daily, glue('data/stats/{params$season}/daily_stats_{params$season}.csv'))
  return(df_daily)
  
}
