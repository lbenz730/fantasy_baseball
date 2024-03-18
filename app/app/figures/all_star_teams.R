library(tidyverse)
library(glue)
library(gt)

### Define Positions + # on team
df_constraints <- 
  tibble('lineup_id' = c(0, 1, 2, 3, 4, 5, 6, 7, 12, 14, 15),
         'position' = c('C', '1B', '2B', '3B', 'SS', 'OF', 
                        '2B/SS', '1B/3B', 'UTIL', 'SP', 'RP'),
         'quantity' = c(1, 1, 1, 1, 1, 5, 1, 1, 1, 6, 3))

z_score <- function(x) {
  (x - mean(x))/sd(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


make_asg_graphics <- function(season = 2023, save = T) {
  ### Rules
  # Only count games in active lineup
  # 67% Weight to Total Points, 33% Weight to PPG
  # Min Games: 30 (Batter), 8 (SP), 10 (RP)
  # 3 points for 1st team, 2 points for 2nd team, 1 point for 3rd team
  w_total <- 0.67
  w_ppg <- 0.33
  
  
  df_daily <- read_csv(glue('data/stats/{season}/daily_stats_{season}.csv'))
  teams <- read_csv(glue('data/stats/{season}/teams_{season}.csv'))
  
  df_bat <- 
    df_daily %>% 
    filter(in_lineup, batter) %>% 
    group_by(player, player_id) %>% 
    mutate(eligible_slots = last(eligible_slots)) %>% 
    ungroup() %>% 
    group_by(player, player_id, eligible_slots) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(played),
              'ppg' = n_points/n_games) %>% 
    mutate('eligible_slots' = str_split(eligible_slots, '/')) %>% 
    ungroup() %>% 
    filter(n_games >= 30)
  
  
  df_rp <- 
    df_daily %>% 
    filter(in_lineup, relief, !relief_start) %>% 
    group_by(player, player_id) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(relief),
              'ppg' = n_points/n_games) %>% 
    ungroup() %>% 
    filter(n_games >= 10)
  
  
  
  df_sp <- 
    df_daily %>% 
    filter(in_lineup, start) %>% 
    group_by(player, player_id) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(start),
              'n_qs' = sum(qs),
              'ppg' = n_points/n_games,
              'qs_rate' = n_qs/n_games) %>% 
    ungroup() %>% 
    filter(n_games >= 8)
  
  
  ### Make All Star Teams
  df_asg <- 
    tibble('lineup_id' = rep(rep(df_constraints$lineup_id, df_constraints$quantity), 3),
           'position' = rep(rep(df_constraints$position, df_constraints$quantity), 3),
           'player' = NA,
           'player_id' = NA,
           'team_id' = NA,
           'points' = NA,
           'ppg' = NA,
           'asg_team' = rep(1:3, each = 22))
  
  for(i in 1:3) {
    for(pos_id in df_constraints$lineup_id) {
      if(pos_id < 14) {
        df_stat <- 
          df_bat %>% 
          filter(map_lgl(eligible_slots, ~{pos_id %in% .x})) 
      } else if(pos_id == 14) {
        df_stat <- df_sp 
      } else {
        df_stat <- df_rp
      }
      
      df_asg[df_asg$lineup_id == pos_id & df_asg$asg_team == i, setdiff(names(df_asg), c('lineup_id', 'position', 'asg_team'))] <- 
        df_stat %>% 
        filter(!player_id %in% c(df_asg$player_id)) %>% 
        mutate('z_total' = z_score(n_points),
               'z_ppg' = z_score(ppg),
               'rank' = w_ppg * z_ppg + w_total * z_total) %>% 
        arrange(-rank) %>% 
        head(df_constraints$quantity[df_constraints$lineup_id == pos_id]) %>% 
        select(player, player_id, team_id, 'points' = n_points, ppg)
    }
  }
  
  df_stars <- 
    df_asg %>% 
    group_by(team_id) %>% 
    summarise('first_team' = sum(asg_team == 1),
              'second_team' = sum(asg_team == 2),
              'third_team' = sum(asg_team == 3)) %>% 
    mutate('star_points' = 3 * first_team + 2 * second_team + third_team) %>% 
    inner_join(select(teams, team, team_id, logo)) %>% 
    select(team, logo, contains('_team'), star_points) %>% 
    arrange(-star_points, -first_team, -second_team, -third_team)
  
  df_asg_wide <- 
    df_asg %>% 
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
    mutate('lineup_id' = ifelse(lineup_id == 5, 11, lineup_id)) %>% 
    inner_join(select(teams, team, team_id, logo)) %>% 
    group_by(asg_team) %>% 
    arrange(lineup_id) %>% 
    group_split() %>% 
    map_dfc(~{
      df <- 
        .x %>% 
        select(position, player, player_url, team, logo, points, ppg) 
      names(df) <- paste0(names(df), '_', .x$asg_team[1])
      df
    })
  
  
  gt_asg <- 
    gt(df_asg_wide) %>%
    
    ### Align Columns
    cols_align(align = "center", columns = everything()) %>%
    
    fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>% 
    fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
    
    
    tab_spanner(label = 'First Team', columns = contains('_1')) %>%
    tab_spanner(label = 'Second Team', columns = contains('_2')) %>%
    tab_spanner(label = 'Third team', columns = contains('_3')) %>%
    
    
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
      locations = cells_body(contains(c('logo', 'player_url'))),
      fn = function(x) {
        web_image(
          url = x,
          height = 50
        )
      }
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
          columns = contains('ppg')
        )
      )
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          rows = c(13, 19)
        )
      )
    ) %>%
    
    
    
    ### Names
    cols_label(
      'position_1' = 'Position',
      'player_1' = 'Player',
      'player_url_1' = '', 
      'team_1' = 'Team',
      'logo_1' = '',
      'points_1' = 'Points',
      'ppg_1' = 'PPG',
      
      'position_2' = 'Position',
      'player_2' = 'Player',
      'player_url_2' = '', 
      'team_2' = 'Team',
      'logo_2' = '',
      'points_2' = 'Points',
      'ppg_2' = 'PPG',
      
      'position_3' = 'Position',
      'player_3' = 'Player',
      'player_url_3' = '', 
      'team_3' = 'Team',
      'logo_3' = '',
      'points_3' = 'Points',
      'ppg_3' = 'PPG'
      
      
    ) %>%
    tab_header(
      title = md(glue('**{season} Fantasy All-Stars**')),
    ) %>%
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 40,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold'
                
    ) %>% 
    tab_footnote(footnote = "Listed Team = team that player played most games for") %>% 
    tab_footnote(footnote = "Players ranked by weighted average of Z-Scores of Points (67%) and PPG (33%) relative to position") %>% 
    tab_footnote(footnote = "Min Games for Inclusion: Batter (30), SP (8), RP (10)") %>% 
    tab_footnote(footnote = "Only includes games players in starting fantasy lineup") 
  
  
  
  gt_stars <- 
    gt(df_stars) %>% 
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
      locations = cells_body(contains(c('logo'))),
      fn = function(x) {
        web_image(
          url = x,
          height = 50
        )
      }
    ) %>%
    
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          columns = contains(c('logo', 'third_team'))
        )
      )
    ) %>%
    
    
    ### Names
    cols_label(
      'team' = 'Team',
      'logo' = '',
      'first_team' = '1st Team',
      'second_team' = '2nd Team',
      'third_team' = '3rd Team',
      'star_points' = 'Total Points'
      
    ) %>%
    tab_header(
      title = md(glue('**{season} Fantasy All-Stars**')),
    ) %>%
    tab_options(column_labels.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 40,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold'
                
    ) %>% 
    tab_footnote(footnote = "Star Points: 1st Team (3), 2nd Team (2), 3rd Team (1)") 
  
  if(save) {
    gtExtras::gtsave_extra(gt_asg , glue('figures/top_performers/{season}/all_stars/asg_{season}.png'), vwidth = 3000, selector = 'table')
    gtExtras::gtsave_extra(gt_stars , glue('figures/top_performers/{season}/all_stars/asg_{season}_count.png'), vwidth = 3000, selector = 'table')
  }
  
  pkg <- 
    list('stars' = df_stars,
         'lineups' = df_asg_wide)
}


best_lineup <- function(season = 2023, week, save = T) {
  df_daily <- read_csv(glue('data/stats/{season}/daily_stats_{season}.csv'))
  teams <- read_csv(glue('data/stats/{season}/teams_{season}.csv'))
  
  df_bat <- 
    df_daily %>% 
    filter(in_lineup, batter) %>% 
    filter(matchup_id == week) %>% 
    group_by(player, player_id, eligible_slots) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(played),
              'ppg' = n_points/n_games) %>% 
    mutate('eligible_slots' = str_split(eligible_slots, '/')) %>% 
    ungroup() 
  
  
  df_rp <- 
    df_daily %>% 
    filter(in_lineup, relief, !relief_start) %>% 
    group_by(player, player_id) %>% 
    filter(matchup_id == week) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(relief),
              'ppg' = n_points/n_games) %>% 
    ungroup() 
  
  
  
  df_sp <- 
    df_daily %>% 
    filter(in_lineup, start) %>% 
    filter(matchup_id == week) %>% 
    group_by(player, player_id) %>% 
    summarise('team_id' = getmode(team_id),
              'n_points' = sum(points),
              'n_games' = sum(start),
              'n_qs' = sum(qs),
              'ppg' = n_points/n_games,
              'qs_rate' = n_qs/n_games) %>% 
    ungroup() 
  
  
  ### Make All Star Teams
  df_best <- 
    tibble('lineup_id' = rep(df_constraints$lineup_id, df_constraints$quantity),
           'position' = rep(df_constraints$position, df_constraints$quantity),
           'player' = NA,
           'player_id' = NA,
           'team_id' = NA,
           'points' = NA,
           'ppg' = NA)
  
  if(!all(df_bat$n_games == 0)) {
    for(pos_id in df_constraints$lineup_id) {
      if(pos_id < 14) {
        df_stat <- 
          df_bat %>% 
          filter(map_lgl(eligible_slots, ~{pos_id %in% .x})) 
      } else if(pos_id == 14) {
        df_stat <- df_sp 
      } else {
        df_stat <- df_rp
      }
      
      tmp_stat <- 
        df_stat %>% 
        filter(!player_id %in% c(df_best$player_id)) %>% 
        arrange(-n_points) %>% 
        head(df_constraints$quantity[df_constraints$lineup_id == pos_id]) %>% 
        select(player, player_id, team_id, 'points' = n_points, ppg)
      
      if(nrow(tmp_stat) > 0) {
        df_best[which(df_best$lineup_id == pos_id)[1:nrow(tmp_stat)], setdiff(names(df_best), c('lineup_id', 'position', 'asg_team'))] <- tmp_stat
      }
      
    }
  }
  
  file_ <- glue('figures/top_performers/{season}/best_lineup/best_lineups.csv')
  if(file.exists(file_)) {
    df_log <- 
      read_csv(file_) %>% 
      filter(matchup_id != week) %>% 
      bind_rows( df_best %>% 
                   mutate('matchup_id' = week)) %>% 
      arrange(matchup_id)
  } else {
    df_log <- 
      df_best %>% 
      mutate('matchup_id' = week)
  }
  
  write_csv(df_log, file_)
  
  df_best <- 
    df_best %>% 
    mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
    mutate('lineup_id' = ifelse(lineup_id == 5, 11, lineup_id)) %>% 
    mutate('n_times' = map_dbl(player_id, ~sum(.x == df_log$player_id[df_log$matchup_id <= week]))) %>% 
    mutate('player' = paste0(player, ' (', n_times, ')')) %>% 
    inner_join(select(teams, team, team_id, logo)) %>% 
    arrange(lineup_id)  %>% 
    select(position, player, player_url, team, logo, points, ppg)
  
  if(save) {
    gt_best <- 
      gt(df_best) %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>%
      
      fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>% 
      fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
      
      
      
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
        locations = cells_body(contains(c('logo', 'player_url'))),
        fn = function(x) {
          web_image(
            url = x,
            height = 50
          )
        }
      ) %>%
      
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            rows = c(13, 19)
          )
        )
      ) %>%
      
      
      
      ### Names
      cols_label(
        'position' = 'Position',
        'player' = 'Player',
        'player_url' = '', 
        'team' = 'Team',
        'logo' = '',
        'points' = 'Points',
        'ppg' = 'PPG'
        
      ) %>%
      tab_header(
        title = md('**Lineup of the Week**'),
        subtitle = md(glue('**Week: {week}**'))
      ) %>%
      tab_options(column_labels.font.size = 20,
                  heading.title.font.size = 40,
                  heading.subtitle.font.size = 40,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold',
                  column_labels.font.weight = 'bold'
                  
      ) %>% 
      tab_footnote(footnote = "Listed Team = team that player played most games for") %>% 
      tab_footnote(footnote = "Only includes games players in starting fantasy lineup") %>% 
      tab_footnote(footnote = 'Numbers in parenthesis indicate # of times a player was in best lineup')
    
    
    gtExtras::gtsave_extra(gt_best , glue('figures/top_performers/{season}/best_lineup/week_{week}.png'), vwidth = 3000, selector = 'table')
    gtExtras::gtsave_extra(gt_best , glue('figures/best_lineup.png'), vwidth = 3000, selector = 'table')
  }
  
}
