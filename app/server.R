library(lubridate)
library(ggridges)
library(ggimage)
library(rsvg)
library(purrr)

html_clean <- function(html) {
  html <- tools::toTitleCase(gsub('\\+', ' ', gsub('%27s', '\'', html)))
  print(html)
  return(html)
}

shinyServer(function(input, output, session) {
  updateSelectInput(session, inputId = "matchup_id", 
                    choices = (params$current_matchup:1), selected = params$current_matchup)
  
  
  ### Advanced Stats
  output$stats_table <- render_gt({
    
    df <-
      select(exp_standings,
             team,
             logo,
             win,
             loss,
             exp_win,
             exp_loss,
             batting_ppg,
             sp_ppg,
             rp_ppg,
             adj_batting_pts,
             adj_pitching_pts,
             adj_pts,
             batting_points,
             pitching_points,
             total_points,
             qs_pct,
             rank) %>%
      left_join(sim_results) %>%
      select(-matchup_id) %>% 
      arrange(-exp_win, -total_points)
    
    df$team[1:12] <-  paste0(df$team[1:12], ' (', df$rank[1:12], ')')
    df$logo[13] <- 'https://i.imgur.com/h3Vd6b8.png'
    df$mean_pts[13] <- mean(df$mean_pts[1:12])
    df <- select(df, -rank)
    
    
    gt1 <-
      gt(df) %>%
      
      ### Round Numbers
      fmt_number(columns = c(exp_win, exp_loss, mean_wins), decimals = 1, sep_mark = '') %>%
      fmt_number(columns = c(win, loss), decimals = 1, sep_mark = '', drop_trailing_zeros = T, drop_trailing_dec_mark = T) %>%
      fmt_number(columns = c(mean_pts), decimals = 0, sep_mark = '') %>%
      fmt_number(columns = c(sp_ppg, rp_ppg, adj_pts, adj_batting_pts, adj_pitching_pts, batting_points, pitching_points, total_points), decimals = 1, sep_mark = '') %>%
      fmt_number(columns = c(batting_ppg), decimals = 2, sep_mark = '') %>%
      fmt_percent(columns = c(qs_pct, playoffs, last_place, champ), decimals = 1, sep_mark = '') %>%
      sub_missing(columns = everything(), missing_text = "---") %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>%
      
      ### Colors
      data_color(columns = c(win, loss, exp_win, exp_loss),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, max(df$win + df$loss)))) %>%
      data_color(columns = c(batting_points),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$batting_points, na.rm = T))) %>%
      data_color(columns = c(pitching_points),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$pitching_points, na.rm = T))) %>%
      data_color(columns = c(total_points),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$total_points, na.rm = T))) %>%
      data_color(columns = c(batting_ppg),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$batting_ppg, na.rm = T))) %>%
      data_color(columns = c(rp_ppg),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$rp_ppg, na.rm = T))) %>%
      data_color(columns = c(sp_ppg),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$sp_ppg, na.rm = T))) %>%
      data_color(columns = c(adj_pts),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_pts, na.rm = T))) %>%
      data_color(columns = c(adj_batting_pts),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_batting_pts, na.rm = T))) %>%
      data_color(columns = c(adj_pitching_pts),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_pitching_pts, na.rm = T))) %>%
      data_color(columns = c(qs_pct),
                 colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$qs_pct, na.rm = T))) %>%
      data_color(columns = c(mean_pts), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$mean_pts, na.rm = T))) %>%
      data_color(columns = c(mean_wins), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$mean_wins, na.rm = T))) %>%
      data_color(columns = c(last_place, playoffs), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0,1))) %>%
      data_color(columns = c(champ), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0,1))) %>%
      
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
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = c(logo, exp_loss, qs_pct, adj_pts, loss, total_points)
          )
        )
      ) %>%
      tab_style(
        style = list(
          cell_borders(
            sides = "top",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            rows = team == 'League Average'
          )
        )
      ) %>%
      
      tab_spanner(label = 'Season Simulations', columns = c('mean_pts', 'mean_wins', 'playoffs', 'champ', 'last_place')) %>%
      tab_spanner(label = 'Total Points', columns = c('batting_points', 'pitching_points', 'total_points')) %>%
      tab_spanner(label = 'Expected Record', columns = c('exp_win', 'exp_loss')) %>%
      tab_spanner(label = 'Record', columns = c('win', 'loss')) %>%
      tab_spanner(label = 'Points Per Player Appearance', columns = c('batting_ppg', 'rp_ppg', 'sp_ppg', 'qs_pct')) %>%
      tab_spanner(label = 'Points Per Matchup', columns = c('adj_batting_pts', 'adj_pitching_pts', 'adj_pts')) %>%
      
      ### Logos
      text_transform(
        locations = cells_body(c(logo)),
        fn = function(x) {
          web_image(
            url = x,
            height = 30
          )
        }
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "white")
        ),
        locations = cells_body(
          columns = c(team, win, loss, exp_win, exp_loss, mean_wins, playoffs, last_place, champ),
          rows = team == 'League Average'
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(team, win, loss, exp_win, exp_loss, mean_wins, playoffs, last_place),
          rows = team == 'League Average'
        )
      ) %>%
      
      ### Check
      text_transform(
        locations = cells_body(
          columns = c(playoffs),
          rows = playoffs == 1
        ),
        fn = function(x) {
          emo::ji('check')
        }
      ) %>%
      
      text_transform(
        locations = cells_body(
          columns = c(playoffs),
          rows = playoffs == 0
        ),
        fn = function(x) {
          emo::ji('x')
        }
      ) %>%
      
      text_transform(
        locations = cells_body(
          columns = c(last_place),
          rows = last_place == 0
        ),
        fn = function(x) {
          emo::ji('x')
        }
      ) %>%
      
      text_transform(
        locations = cells_body(
          columns = c(last_place),
          rows = last_place == 1
        ),
        fn = function(x) {
          emo::ji('check')
        }
      ) %>%
      
      text_transform(
        locations = cells_body(
          columns = c(champ),
          rows = champ == 0
        ),
        fn = function(x) {
          emo::ji('x')
        }
      ) %>%
      
      text_transform(
        locations = cells_body(
          columns = c(champ),
          rows = champ == 1
        ),
        fn = function(x) {
          emo::ji('check')
        }
      ) %>%
      
      ### Names
      cols_label(
        team = 'Team (Seed)',
        logo = '',
        win = 'Wins',
        loss = 'Losses',
        exp_win = 'Wins',
        exp_loss = 'Losses',
        batting_ppg = 'Batter',
        sp_ppg = 'SP',
        rp_ppg = 'RP',
        adj_pts = 'Total',
        adj_batting_pts = 'Batting',
        adj_pitching_pts = 'Pitching',
        batting_points = 'Batting',
        pitching_points = 'Pitching',
        total_points = 'Total',
        qs_pct = 'QS %',
        mean_wins = 'Avg. Wins',
        mean_pts = 'Avg. Points',
        playoffs = 'Playoffs',
        champ = 'Champion',
        last_place = html(ferry),
      ) %>%
      tab_header(
        subtitle = md('**Millburnish Fantansy Baseball League Advanced Stats Table**'),
        title = md('<img src=https://i.imgur.com/h3Vd6b8.png style="height:200px;">')
      ) %>%
      tab_options(column_labels.font.size = 20,
                  heading.title.font.size = 40,
                  heading.subtitle.font.size = 40,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold',
                  column_labels.font.weight = 'bold'
                  
      ) %>%
      tab_footnote(
        footnote = "Expected Wins in a given week = (# of teams you scored higher than)/(11 teams you could've played)",
        locations = cells_column_spanners(spanners = "Expected Record")
      ) %>%
      tab_footnote(
        footnote = "Points Per Matchup adjusted as average per 7-day matchup",
        locations = cells_column_spanners(spanners = "Points Per Matchup")
      ) %>%
      tab_footnote(
        footnote = "100,000 simulations of remainder of season",
        locations = cells_column_spanners(spanners = "Season Simulations")
      )
    
    gt1
    
  })
  
  week <- eventReactive(input$matchup_id, {
    input$matchup_id
  })
  
  ### Top Performers
  df_top <- eventReactive(input$matchup_id, {
    
    
    
    ### Top Performers
    df_bat <-
      batter_points %>%
      filter(!is.na(n_games)) %>%
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
             'ppg' = n_points/n_games) %>%
      filter(matchup_id <= input$matchup_id) %>%
      group_by(matchup_id) %>% 
      arrange(-n_points, n_games) %>%
      dplyr::slice(1:10) %>% 
      left_join(teams) %>%
      group_by(player) %>% 
      mutate('n' = n()) %>% 
      ungroup() %>% 
      mutate(player = paste0(player, ' (', n, ')')) %>% 
      filter(matchup_id == input$matchup_id) %>%
      select(player, player_url, team, logo, n_points)
    
    names(df_bat) <- paste0(names(df_bat), '_bat')
    
    
    ### Top Performers
    df_sp <-
      sp_points %>%
      filter(!is.na(n_games)) %>%
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
             'ppg' = n_points/n_games) %>%
      filter(matchup_id <= input$matchup_id) %>%
      group_by(matchup_id) %>% 
      arrange(-ppg, n_games) %>%
      dplyr::slice(1:10) %>% 
      left_join(teams) %>%
      group_by(player) %>% 
      mutate('n' = n()) %>% 
      ungroup() %>% 
      mutate(player = paste0(player, ' (', n, ')')) %>% 
      filter(matchup_id == input$matchup_id) %>%
      select(player, player_url, team, logo, n_games, ppg)
    if(nrow(df_sp) < 10) {
      df_sp[max(1, nrow(df_sp) + 1):10,] <- NA
    }
    
    names(df_sp) <- paste0(names(df_sp), '_sp')
    
    ### Top Performers
    df_rp <-
      rp_points %>%
      filter(!is.na(n_games)) %>%
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
             'ppg' = n_points/n_games) %>%
      filter(matchup_id <= input$matchup_id) %>%
      group_by(matchup_id) %>% 
      arrange(-n_points, n_games) %>%
      dplyr::slice(1:10) %>% 
      left_join(teams) %>%
      group_by(player) %>% 
      mutate('n' = n()) %>% 
      ungroup() %>% 
      mutate(player = paste0(player, ' (', n, ')')) %>% 
      filter(matchup_id == input$matchup_id) %>%
      select(player, player_url, team, logo, n_games, n_points)
    
    if(nrow(df_rp) < 10) {
      df_rp[max(1,nrow(df_rp) + 1):10,] <- NA
    }
    
    
    names(df_rp) <- paste0(names(df_rp), '_rp')
    bind_cols(df_bat, df_sp, df_rp)
    
  })
  
  output$top_performers <- render_gt({
    
    gt_top <-
      gt(df_top()) %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>%
      
      
      tab_spanner(label = 'Relief Pitching', columns = contains('_rp')) %>%
      tab_spanner(label = 'Starting Pitching', columns = contains('_sp')) %>%
      tab_spanner(label = 'Batting', columns = contains('_bat')) %>%
      sub_missing(columns = everything(), missing_text = "---") %>%
      
      
      
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
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = contains('points')
          )
        )
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
            columns = contains('ppg')
          )
        )
      ) %>%
      
      
      ### Names
      cols_label(
        'team_bat' = 'Team',
        'logo_bat' = '',
        'player_url_bat' = '',
        'player_bat' = 'Player',
        'n_points_bat' = 'Points',
        
        'team_sp' = 'Team',
        'logo_sp' = '',
        'player_url_sp' = '',
        'player_sp' = 'Player',
        'ppg_sp' = 'Points/Start',
        'n_games_sp' = '# of Starts',
        
        'team_rp' = 'Team',
        'logo_rp' = '',
        'player_url_rp' = '',
        'player_rp' = 'Player',
        'n_points_rp' = 'Points',
        'n_games_rp' = '# of Appearances'
        
      ) %>%
      tab_header(
        title = md('**Top Performances**'),
        subtitle = md(glue('**Week {week()}**'))
      ) %>%
      tab_options(column_labels.font.size = 20,
                  heading.title.font.size = 40,
                  heading.subtitle.font.size = 40,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold',
                  column_labels.font.weight = 'bold'
                  
      ) %>% 
      tab_source_note('Numbers next to player denotes the # of appearances in the weekly top 10')
    
    gt_top
    
  })
  
  
})



