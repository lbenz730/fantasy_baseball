library(tidyverse)
library(jsonlite)
library(gt)
library(furrr)
library(glue)
library(lubridate)
library(here)
plan(multisession(workers = parallel::detectCores() - 1))
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2000)
source(here('helpers.R'))

# https://github.com/cwendt94/espn-api/blob/master/espn_api/baseball/constant.py#L69 [STAT MAP]

get_daily_stats <- function(x, y, index, team, df_schedule) {
  if(team == 'home') {
    x <- x$schedule$home$rosterForCurrentScoringPeriod
    y <- y$schedule$home$rosterForCurrentScoringPeriod
  } else {
    x <- x$schedule$away$rosterForCurrentScoringPeriod
    y <- y$schedule$away$rosterForCurrentScoringPeriod
  }

  df <-
    map_dfr(x$entries[[index]]$playerPoolEntry$player$stats, ~{
      if(length(.x) > 0) {
        tibble('points' = sum(.x$appliedTotal, na.rm = T),
               'played' = sum(.x$stats$`81`, na.rm = T),
               'start' = as.logical(sum(.x$stats$`33` > 0 | .x$stats$`34` >= 12, na.rm = T)),
               'relief' = as.logical(sum(.x$stats$`32` > 0 & .x$stats$`33` == 0, na.rm = T)),
               'relief_start' = as.logical(sum(.x$stats$`33` == 0 & .x$stats$`34` >= 12, na.rm = T)),
               'qs' = as.logical(sum(.x$stats$`34` >= 18 & .x$stats$`45` <= 3, na.rm = T)),
               'save' = as.logical(sum(.x$stats$`57`, na.rm = T)),
               'home_runs' = sum(.x$stats$`5`, na.rm = T),
               'blue_balls' = as.logical(sum(.x$stats$`34` == 17 & .x$stats$`45` <= 3, na.rm = T)),
               'hr_allowed' = sum(.x$stats$`46`, na.rm = T),
               'p_walks' = sum(.x$stats$`39`, na.rm = T),
               'p_ibb' = sum(.x$stats$`40`, na.rm = T),
               'p_hbp' = sum(.x$stats$`42`, na.rm = T),
               'p_outs' = sum(.x$stats$`34`, na.rm = T),
               'p_er' = sum(.x$stats$`45`, na.rm = T),
               'p_win' = sum(.x$stats$`53`, na.rm = T),
               'p_lose' = sum(.x$stats$`54`, na.rm = T),
               'p_blsv' = sum(.x$stats$`58`, na.rm = T),
               'p_hold' = sum(.x$stats$`60`, na.rm = T),
               'p_k' = sum(.x$stats$`48`, na.rm = T),
               'p_cg' = sum(.x$stats$`62`, na.rm = T),
               'h_1b' = sum(.x$stats$`7`, na.rm = T),
               'h_2b' = sum(.x$stats$`3`, na.rm = T),
               'h_3b' = sum(.x$stats$`4`, na.rm = T),
               'b_ab' = sum(.x$stats$`0`, na.rm = T),
               'b_pa' = sum(.x$stats$`16`, na.rm = T),
               'b_walks' =  sum(.x$stats$`10`, na.rm = T),
               'b_hpb' =  sum(.x$stats$`12`, na.rm = T),
               'b_ibb' =  sum(.x$stats$`11`, na.rm = T),
               'b_sf' =  sum(.x$stats$`13`, na.rm = T),
               'b_runs' = sum(.x$stats$`20`, na.rm = T),
               'b_rbi' = sum(.x$stats$`21`, na.rm = T),
               'b_k' = sum(.x$stats$`27`, na.rm = T)
        )
      } else {
        tibble('points' = 0,
               'played' = 0,
               'start' = F,
               'relief' = F,
               'relief_start' = F,
               'qs' = F,
               'save' = F,
               'home_runs' = 0,
               'hr_allowed' = 0,
               'blue_balls' = F,  
               'p_walks' = 0,
               'p_ibb' = 0,
               'p_win' = 0,
               'p_hold' = 0,
               'p_blsv' = 0,
               'p_loss' = 0,
               'p_hbp' = 0,
               'p_outs' = 0,
               'p_er' = 0,
               'p_k' = 0,
               'p_cg' = 0,
               'h_1b' = 0,
               'h_2b' = 0,
               'h_3b' = 0,
               'b_ab' = 0,
               'b_pa' = 0,
               'b_walks' = 0,
               'b_hpb' =  0,
               'b_ibb' =  0,
               'b_sf' =  0,
               'b_runs' = 0,
               'b_rbi' = 0,
               'b_k' = 0)
        
      } }) %>%
    mutate('player' = y$entries[[index]]$playerPoolEntry$player$fullName,
           'player_id' = y$entries[[index]]$playerPoolEntry$player$id) %>%
    mutate('team_id' = ifelse(team == 'home', df_schedule$home_team_id[index], df_schedule$away_team_id[index])) %>%
    mutate('game_id' = index)
  
  ### Christian Javier Rule
  df$points[df$relief_start & df$qs] <- df$points[df$relief_start & df$qs] + 5
  
  ### Yu Darvish Rule
  df$points[df$p_outs >= 27 & df$p_cg == 0] <- df$points[df$p_outs >= 27 & df$p_cg == 0] + 5
  df$points[df$p_outs >= 27 & df$p_cg == 0 & df$p_er == 0] <- df$points[df$p_outs >= 27 & df$p_cg == 0 & df$p_er == 0] + 5
  df$p_cg[df$p_outs >= 27 & df$p_cg == 0] <- 1
  
  return(df)

}


get_matchup_stats <- function(week, season = 2023) {

  df_schedule <- read_csv(glue('data/stats/{season}/schedule_{season}.csv'))
  df_start <- read_csv('data/df_start.csv')
  df_start <- df_start[df_start$season == season,]
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]

  df <-
    future_map_dfr(start:end, ~{
      x <- robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchupScore'))
      y <- robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchup'))
      z <- robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mRoster'))
      # w <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=kona_player_info'))


      roster_status <-
        map2_dfr(z$teams$roster$entries, z$teams$id, ~{
          tibble('player' = .x$playerPoolEntry$player$fullName,
                 'player_id' = .x$playerPoolEntry$player$id,
                 'eligible_slots' = map_chr(.x$playerPoolEntry$player$eligibleSlots, paste, collapse = '/'),
                 'lineup_id' = .x$lineupSlotId,
                 'team_id' = .y)
        })

      indices <- which(!map_lgl(x$schedule$home$rosterForCurrentScoringPeriod$entries, is.null))

      tmp1 <- map_dfr(indices, function(i) get_daily_stats(x, y, i, 'home', df_schedule))
      tmp2 <- map_dfr(indices, function(i) get_daily_stats(x, y, i, 'away', df_schedule))
      if(nrow(tmp1) > 0) {
        df <-
          bind_rows(tmp1, tmp2) %>%
          mutate('scoring_period_id' = .x) %>%
          mutate('matchup_id' = week) %>%
          left_join(roster_status, by = c("player", "player_id", "team_id")) %>%
          mutate('in_lineup' = lineup_id <= 15,
                 'pitcher' = lineup_id %in% c(14, 15),
                 'batter' = lineup_id <= 12) %>%
          mutate('season' = season) %>%
          mutate('playoffs' = df_start$playoffs[week])


        df
      } else {
        NULL
      }

    })

  return(df)
}
