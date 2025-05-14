library(lubridate)
library(ggridges)
library(ggimage)
library(rsvg)


# screen <-
#   div(
#     waiter::spin_3(),
#     h1("Millburnish Fantasy Baseball"),
#     h3("Site Loading")
#   )

shinyServer(function(input, output, session) {
  
  # waiter::waiter_show(html = screen,
  # image = 'https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Spirit_of_America_-_Staten_Island_Ferry.jpg/800px-Spirit_of_America_-_Staten_Island_Ferry.jpg')
  
  
  
  
  
  
  ######################
  ### Advanced Stats ###
  ######################
  output$stats_table <- render_gt({
    cat('Rendering Advanced Stats Table\n')
    cat('Processing Data for Advanced Stats Table\n')
    league_avg <- 
      exp_standings[13,] %>% 
      select(
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
        adj_sp_pts,
        adj_rp_pts,
        adj_pts,
        batting_points,
        sp_points,
        rp_points,
        total_points,
        qs_pct,
        rank)
    
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
             adj_sp_pts,
             adj_rp_pts,
             adj_pts,
             batting_points,
             sp_points,
             rp_points,
             total_points,
             qs_pct,
             rank) %>%
      dplyr::slice(1:12) %>% 
      left_join(sim_results, by = 'team') %>%
      select(-matchup_id, -team_id)
    
    df <- 
      df %>% 
      mutate_at(vars(contains('adj_')), ~replace(.x, is.na(.x), 0))
    
    
    df <- bind_rows(df, league_avg)
    
    df$team[1:12] <-  paste0(df$team[1:12], ' (', df$rank[1:12], ')')
    # df$logo[13] <- 'https://i.imgur.com/h3Vd6b8.png'
    df$logo[13] <- 'www/League.png'
    df$mean_pts[13] <- mean(df$mean_pts[1:12])
    df <- select(df, -rank)
    
    cat('Creating Advanced Stats Table\n')
    gt(df) %>%
      
      ### Add this once more updates have happened.
      # opt_interactive(use_pagination = F) %>%
      
      ### Round Numbers
      fmt_number(columns = c(exp_win, exp_loss, mean_wins), decimals = 1, sep_mark = '') %>%
      fmt_number(columns = c(win, loss), decimals = 1, sep_mark = '', drop_trailing_zeros = T, drop_trailing_dec_mark = T) %>%
      fmt_number(columns = c(mean_pts), decimals = 0, sep_mark = '') %>%
      fmt_number(columns = c(sp_ppg, rp_ppg, adj_pts, adj_batting_pts, adj_sp_pts, adj_rp_pts, batting_points, sp_points, rp_points, total_points), decimals = 1, sep_mark = '') %>%
      fmt_number(columns = c(batting_ppg), decimals = 2, sep_mark = '') %>%
      fmt_percent(columns = c(qs_pct, playoffs, last_place, champ), decimals = 1, sep_mark = '') %>%
      sub_missing(columns = everything(), missing_text = "---") %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>%
      
      ### Colors
      data_color(columns = c(win, loss, exp_win, exp_loss),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, max(df$win + df$loss, na.rm = T))),
                 autocolor_text = F) %>%
      data_color(columns = c(batting_points),
                 fn= scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$batting_points, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(sp_points),
                 fn= scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$sp_points, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(rp_points),
                 fn= scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$rp_points, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(total_points),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$total_points, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(batting_ppg),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$batting_ppg, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(rp_ppg),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$rp_ppg, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(sp_ppg),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$sp_ppg, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(adj_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_pts, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(adj_batting_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_batting_pts, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(adj_sp_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_sp_pts, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(adj_rp_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_rp_pts, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(qs_pct),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$qs_pct, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(mean_pts), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), 
                                                                     domain = range(df$mean_pts, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(mean_wins), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), 
                                                                      domain = range(df$mean_wins, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(last_place, playoffs), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0,1)),
                 autocolor_text = F) %>%
      data_color(columns = c(champ), colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0,1)),
                 autocolor_text = F) %>%
      
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
      tab_spanner(label = 'Total Points', columns = c('batting_points', 'sp_points', 'rp_points', 'total_points')) %>%
      tab_spanner(label = 'Expected Record', columns = c('exp_win', 'exp_loss')) %>%
      tab_spanner(label = 'Record', columns = c('win', 'loss')) %>%
      tab_spanner(label = 'Points Per Player Appearance', columns = c('batting_ppg', 'rp_ppg', 'sp_ppg', 'qs_pct')) %>%
      tab_spanner(label = 'Points Per Matchup', columns = c('adj_batting_pts', 'adj_sp_pts', 'adj_rp_pts', 'adj_pts')) %>%
      
      ### Logos
      text_transform(
        locations = cells_body(c(logo)),
        fn = function(x) {
          local_image(
            filename = x,
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
        adj_sp_pts = 'SP',
        adj_rp_pts = 'RP',
        batting_points = 'Batting',
        sp_points = 'SP',
        rp_points = 'RP',
        total_points = 'Total',
        qs_pct = 'QS %',
        mean_wins = 'Avg. Wins',
        mean_pts = 'Avg. Points',
        playoffs = 'Playoffs',
        champ = 'Champion',
        last_place = md(ferry),
      ) %>%
      tab_header(
        subtitle = md('**Millburnish Fantansy Baseball League Advanced Stats Table**'),
        title = md('<img src="www/League.png" style="height:200px;">')
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
        locations = cells_column_spanners(spanners = "Expected Record"),
        placement = 'left'
      ) %>%
      tab_footnote(
        footnote = "Points Per Matchup adjusted as average per 7-day matchup",
        locations = cells_column_spanners(spanners = "Points Per Matchup"),
        placement = 'left'
      ) %>%
      tab_footnote(
        footnote = "10,000 simulations of remainder of season",
        locations = cells_column_spanners(spanners = "Season Simulations"),
        placement = 'left'
      )
    
    
  })
  
  output$pitch_table <- render_gt({
    cat('Creating Pitching Table\n')
    
    df_ps <- 
      pitch_stats %>% 
      mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>% 
      mutate_at(vars(everything()), ~replace(.x, .x == Inf, 0)) %>% 
      inner_join(teams, by = 'team_id') %>% 
      inner_join(
        exp_standings %>% 
          select(team, pitching_points),
        by = 'team') %>% 
      arrange(-pitching_points) %>% 
      select(team, logo, 
             era, fip, k9, bb9, k_per_bb, hr9, 
             era_sp, fip_sp, k9_sp, bb9_sp, k_per_bb_sp, hr9_sp, 
             era_rp, fip_rp, k9_rp, bb9_rp, k_per_bb_rp, hr9_rp, 
             qs, blue_balls, langes, bednars) %>% 
      bind_rows(tibble('team' = 'League Average',
                       'logo' = 'www/League.png',
                       'era' = weighted.mean(pitch_stats$era, pitch_stats$outs),
                       'fip' = weighted.mean(pitch_stats$fip, pitch_stats$outs),
                       'k9' = weighted.mean(pitch_stats$k9, pitch_stats$outs),
                       'bb9' = weighted.mean(pitch_stats$bb9, pitch_stats$outs),
                       'hr9' = weighted.mean(pitch_stats$hr9, pitch_stats$outs),
                       'k_per_bb' = weighted.mean(pitch_stats$k_per_bb, pitch_stats$outs),
                       
                       'era_sp' = weighted.mean(pitch_stats$era_sp, pitch_stats$outs_sp),
                       'fip_sp' = weighted.mean(pitch_stats$fip_sp, pitch_stats$outs_sp),
                       'k9_sp' = weighted.mean(pitch_stats$k9_sp, pitch_stats$outs_sp),
                       'bb9_sp' = weighted.mean(pitch_stats$bb9_sp, pitch_stats$outs_sp),
                       'hr9_sp' = weighted.mean(pitch_stats$hr9_sp, pitch_stats$outs_sp),
                       'k_per_bb_sp' = weighted.mean(pitch_stats$k_per_bb_sp, pitch_stats$outs_sp),
                       
                       
                       'era_rp' = weighted.mean(pitch_stats$era_rp, pitch_stats$outs_rp),
                       'fip_rp' = weighted.mean(pitch_stats$fip_rp, pitch_stats$outs_rp),
                       'k9_rp' = weighted.mean(pitch_stats$k9_rp, pitch_stats$outs_rp),
                       'bb9_rp' = weighted.mean(pitch_stats$bb9_rp, pitch_stats$outs_rp),
                       'hr9_rp' = weighted.mean(pitch_stats$hr9_rp, pitch_stats$outs_rp),
                       'k_per_bb_rp' = weighted.mean(pitch_stats$k_per_bb_rp, pitch_stats$outs_rp),
                       
                       'qs' = mean(pitch_stats$qs),
                       'blue_balls' = mean(pitch_stats$blue_balls),
                       'langes' = mean(pitch_stats$langes),
                       'bednars' = mean(pitch_stats$bednars),
      )) %>% 
      mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>% 
      mutate_at(vars(everything()), ~replace(.x, .x == Inf, 0))
    
    df_ps %>% 
      gt() %>% 
      
      ### Round Numbers
      fmt_number(columns = c(era, era_sp, era_rp, 
                             fip, fip_sp, fip_rp, 
                             k9, k9_sp, k9_rp, 
                             bb9, bb9_sp, bb9_rp, 
                             k_per_bb, k_per_bb_sp, k_per_bb_rp,
                             hr9, hr9_sp, hr9_rp), decimals = 2, sep_mark = '') %>% 
      fmt_number(columns = c(qs, blue_balls, langes, bednars), decimals = 2, drop_trailing_zeros = T) %>% 
      sub_missing(columns = everything(), missing_text = "---") %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>% 
      
      ### Headers
      tab_spanner(columns = c('era', 'fip', 'k9', 'bb9', 'k_per_bb', 'hr9'), label = 'All Pitchers') %>% 
      tab_spanner(columns = c('era_sp', 'fip_sp', 'k9_sp', 'bb9_sp', 'k_per_bb_sp', 'hr9_sp'), label = 'Starting Pitchers') %>% 
      tab_spanner(columns = c('era_rp', 'fip_rp', 'k9_rp', 'bb9_rp', 'k_per_bb_rp', 'hr9_rp'), label = 'Relief Pitchers') %>% 
      
      ### Colors
      data_color(columns = c(era, fip),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(c(df_ps$era, df_ps$fip))),
                 autocolor_text = F) %>% 
      data_color(columns = c(era_sp, fip_sp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(c(df_ps$era_sp, df_ps$fip_sp))),
                 autocolor_text = F) %>% 
      data_color(columns = c(era_rp, fip_rp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(c(df_ps$era_rp, df_ps$fip_rp))),
                 autocolor_text = F) %>% 
      data_color(columns = c(k9),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k9)),
                 autocolor_text = F) %>% 
      data_color(columns = c(bb9),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$bb9)),
                 autocolor_text = F) %>% 
      data_color(columns = c(hr9),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$hr9)),
                 autocolor_text = F) %>% 
      data_color(columns = c(k_per_bb),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k_per_bb)),
                 autocolor_text = F) %>% 
      
      data_color(columns = c(k9_sp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k9_sp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(bb9_sp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$bb9_sp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(hr9_sp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$hr9_sp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(k_per_bb_sp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k_per_bb_sp)),
                 autocolor_text = F) %>% 
      
      data_color(columns = c(k9_rp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k9_rp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(bb9_rp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$bb9_rp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(hr9_rp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_ps$hr9_rp)),
                 autocolor_text = F) %>% 
      data_color(columns = c(k_per_bb_rp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$k_per_bb_rp)),
                 autocolor_text = F) %>% 
      
      data_color(columns = c(qs),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$qs)),
                 autocolor_text = F) %>% 
      data_color(columns = c(blue_balls),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$blue_balls)),
                 autocolor_text = F) %>% 
      data_color(columns = c(langes),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$langes)),
                 autocolor_text = F) %>% 
      data_color(columns = c(bednars),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_ps$bednars)),
                 autocolor_text = F) %>% 
      
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
            columns = c(logo, contains('hr9'))
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
      ### Logos
      text_transform(
        locations = cells_body(c(logo)),
        fn = function(x) {
          local_image(
            filename = x,
            height = 30
          )
        }
      ) %>% 
      ### Names
      cols_label(
        team = 'Team',
        logo = '',
        era = 'ERA',
        fip = 'FIP',
        era_sp = 'ERA',
        fip_sp = 'FIP',
        era_rp = 'ERA',
        fip_rp = 'FIP',
        k9 = 'K/9',
        k9_sp = 'K/9',
        k9_rp = 'K/9',
        bb9 = 'BB/9',
        bb9_sp = 'BB/9',
        bb9_rp = 'BB/9',
        k_per_bb = 'K/BB',
        k_per_bb_sp = 'K/BB',
        k_per_bb_rp = 'K/BB',
        hr9 = 'HR/9',
        hr9_sp = 'HR/9',
        hr9_rp = 'HR/9',
        qs = 'QS',
        blue_balls = html('Blue<br>Balls'),
        langes = 'LNG',
        bednars = 'BED',
      ) %>%
      tab_header(
        title = md('**Pitching Stats**'),
      ) %>%
      tab_options(column_labels.font.size = 20,
                  heading.title.font.size = 40,
                  heading.subtitle.font.size = 40,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold',
                  column_labels.font.weight = 'bold'
                  
      ) %>%
      tab_footnote(
        footnote = "Blue Balls = 5.2 IP and 3 or fewer earned runs",
        locations = cells_column_labels('blue_balls'),
        placement = 'left'
      ) %>% 
      tab_footnote(
        footnote = "RP appearances of -5 or worse, named after Alex \"Scrub\" Lange",
        locations = cells_column_labels('langes'),
        placement = 'left'
      ) %>% 
      tab_footnote(
        footnote = "RP appearances of -10 or worse, named after David \"Shit the\" Bednar",
        locations = cells_column_labels('bednars'),
        placement = 'left'
      ) 
    
  })
  
  output$bat_table <- render_gt({
    cat('Rendering Batting Table\n')
    df_bs <- 
      bat_stats %>% 
      mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0)) %>% 
      inner_join(teams, by = 'team_id') %>% 
      inner_join(
        exp_standings %>% 
          select(team, batting_points),
        by = 'team') %>% 
      arrange(-batting_points) %>% 
      select(team, logo, 
             avg, obp, slg, ops, woba, babip, k_rate, bb_rate) %>% 
      bind_rows(tibble('team' = 'League Average',
                       'logo' = 'www/League.png',
                       'avg' = weighted.mean(bat_stats$avg, bat_stats$h_pa),
                       'obp' = weighted.mean(bat_stats$obp, bat_stats$h_pa),
                       'slg' = weighted.mean(bat_stats$slg, bat_stats$h_pa),
                       'ops' = weighted.mean(bat_stats$ops, bat_stats$h_pa),
                       'woba' = weighted.mean(bat_stats$woba, bat_stats$h_pa),
                       'babip' = weighted.mean(bat_stats$babip, bat_stats$h_pa),
                       'k_rate' = weighted.mean(bat_stats$k_rate, bat_stats$h_pa),
                       'bb_rate' = weighted.mean(bat_stats$bb_rate, bat_stats$h_pa)))  %>% 
      mutate_at(vars(everything()), ~replace(.x, is.na(.x), 0))
    df_bs %>% 
      gt() %>% 
      
      ### Round Numbers
      fmt_number(columns = c(avg, obp, ops, slg, woba, babip), decimals = 3, sep_mark = '') %>% 
      fmt_percent(columns = c(k_rate, bb_rate), decimals = 1) %>% 
      sub_missing(columns = everything(), missing_text = "---") %>%
      
      ### Align Columns
      cols_align(align = "center", columns = everything()) %>% 
      
      ### Colors
      data_color(columns = c(avg),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$avg)),
                 autocolor_text = F) %>% 
      
      data_color(columns = c(obp),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$obp)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(ops),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$ops)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(slg),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$slg)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(babip),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$babip)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(woba),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$woba)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(k_rate),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100, reverse = T), domain = range(df_bs$k_rate)),
                 autocolor_text = F) %>%
      
      data_color(columns = c(bb_rate),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_bs$bb_rate)),
                 autocolor_text = F) %>%
      
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
            columns = c(logo)
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
      ### Logos
      text_transform(
        locations = cells_body(c(logo)),
        fn = function(x) {
          local_image(
            filename = x,
            height = 30
          )
        }
      ) %>% 
      ### Names
      cols_label(
        team = 'Team',
        logo = '',
        avg = 'AVG',
        obp = 'OBP',
        ops = 'OPS',
        slg = 'SLG',
        babip = 'BABIP',
        woba = 'wOBA',
        k_rate = 'K %',
        bb_rate = 'BB %'
        
      ) %>%
      tab_header(
        title = md('**Batting Stats**'),
      ) %>%
      tab_options(column_labels.font.size = 20,
                  heading.title.font.size = 40,
                  heading.subtitle.font.size = 40,
                  heading.title.font.weight = 'bold',
                  heading.subtitle.font.weight = 'bold',
                  column_labels.font.weight = 'bold'
                  
      ) 
    
  })
  
  output$sp_matrix <- renderPlot({
    cat('Rendering Plot Matrix\n')
    ggplot(pitch_matrix, aes(x = ip, y = earned_runs))  + 
      facet_wrap(~team) + 
      annotate(geom = 'rect', 
               xmin = '< 3', 
               xmax = 'CG',
               ymin = '4',
               ymax = '6+',
               col = NA,
               fill = 'red',
               alpha = 0.1) +
      annotate(geom = 'rect', 
               xmin = '< 3',
               xmax = '5.0',
               ymin = '0', 
               ymax = '3',
               col = NA,
               fill = 'red',
               alpha = 0.1) +
      annotate(geom = 'rect', 
               xmin = '< 3', 
               xmax = '6.0',
               ymin = '3',
               ymax = '4',
               col = NA,
               fill = 'red',
               alpha = 0.1) +
      annotate(geom = 'rect', 
               xmin = '5.0', 
               xmax = '6.0',
               ymin = '0',
               ymax = '3',
               col = 'orange',
               fill = 'orange',
               alpha = 0.1) +
      annotate(geom = 'rect', 
               xmin = '6.0', 
               xmax = 'CG',
               ymin = '3', 
               ymax = '4',
               col = 'orange',
               fill = 'orange',
               alpha = 0.1) +
      annotate(geom = 'rect', 
               xmin = '6.0', 
               xmax = 'CG',
               ymin = '0', 
               ymax = '3',
               col = 'seagreen',
               fill = 'mediumspringgreen',
               alpha = 0.1) +
      geom_label(aes(label = n, fill = start_type)) + 
      scale_x_discrete(limits = c('< 3', '3.0', '3.1', '3.2', '4.0', '4.1', '4.2', '5.0', '5.1', '5.2', '6.0', 
                                  '6.1', '6.2', '7.0', '> 7', 'CG')) +
      # drop = F) + 
      scale_fill_manual(values = c('salmon', 'lightskyblue', 'orange',  'seagreen3', 'violet'), drop = F) + 
      theme(legend.position = 'bottom') + 
      labs(x = 'Innings Pitched',
           y = 'Earned Runs',
           fill = '',
           title = 'Classification of SP Performances', 
           caption = 
             paste('Numbers denote frequency of (ER, IP) Combination',
                   'Blue Ball = 5.2 IP and 3 or fewer earned runs',
                   'QS Potential = 5.0/5.1 IP and 3 or fewer earned runs OR 6+ IP and 4 earned runs',
                   'Green Box = "QS Zone"',
                   'Orange Boxes = "Potential QS Zone"',
                   'Red Boxes = "Bad Start Zone"',
                   sep = '\n')) + 
      theme(axis.text.x = element_text(size = 10),
            plot.caption = element_text(size = 12),
            plot.title = element_text(size = 24),
            axis.title = element_text(size = 20),
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 12)
      )
  },
  height = 900,
  width = 1600)
  
  output$start_buckets <- 
    renderPlot({
      ggplot(start_buckets, aes(x = start_bucket, y = pct_start)) + 
        facet_wrap(~team) + 
        
        geom_col(aes(fill = start_bucket)) + 
        geom_col(data = start_buckets_avg, aes(x = start_bucket, y = league_avg), alpha = 0, lty = 2, col = 'black') +
        geom_label(aes(label = paste0(sprintf('%0.1f', 100 * pct_start), '%')),
                   vjust = -0.2) + 
        
        labs(x = 'Points',
             y = '% of Starts',
             fill = 'Start Points',
             title = 'Distribution of SP Points',
             caption = 'Dashed Line = League Average') + 
        scale_y_continuous(labels = scales::percent, limits = c(0, max(start_buckets$pct_start) + 0.05)) +
        scale_x_discrete(limits = c('<= 0', '1-5', '6-10', '11-15', '16-20',
                                    '21-25', '26-30', '> 30')) + 
        scale_fill_manual(values = c('red3', 'salmon','orange', 'violet', 'lightskyblue', 'aquamarine2', 'seagreen3', 'forestgreen'), 
                          drop = F,
                          limits = c('<= 0', '1-5', '6-10', '11-15', '16-20',
                                     '21-25', '26-30', '> 30')) + 
        theme(legend.position = 'bottom',
              plot.caption = element_text(size = 16),
              plot.title = element_text(size = 24),
              axis.title = element_text(size = 20),
              strip.text = element_text(size = 14),
              legend.text = element_text(size = 12))
    },
    height = 900,
    width = 1600)
  
  
  
  ######################
  ### Top Performers ###
  ######################
  
  week <- eventReactive(input$matchup_id, {
    input$matchup_id
  })
  
  gt_top <- 
    eventReactive(input$matchup_id, {
      cat('Computing Top Performers of the Week\n')
      
      
      ### Top Performers
      df_bat <-
        df_daily %>%
        filter(in_lineup, batter) %>%
        group_by(player_id, team_id, player, matchup_id) %>%
        summarise('n_games' = sum(played),
                  'n_points' = sum(points)) %>%
        ungroup() %>% 
        mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
               'ppg' = n_points/n_games) %>%
        filter(n_games > 0) %>% 
        group_by(matchup_id) %>% 
        arrange(-n_points, n_games) %>%
        dplyr::slice(1:10) %>% 
        left_join(teams, by = 'team_id') %>%
        group_by(player) %>% 
        mutate('n' = n()) %>% 
        ungroup() %>% 
        mutate(player = paste0(player, ' (', n, ')')) %>% 
        filter(matchup_id == input$matchup_id) %>%
        select(player, player_url, team, logo, n_points)
      
      if(nrow(df_bat) < 10) {
        df_bat[max(1, nrow(df_bat) + 1):10,] <- NA
      }
      
      names(df_bat) <- paste0(names(df_bat), '_bat')
      
      
      ### Top Performers
      df_sp <-
        df_daily %>%
        filter(in_lineup, pitcher) %>%
        group_by(player_id, team_id, player, matchup_id) %>%
        summarise('n_games' = sum(start),
                  'n_points' = sum(points[start])) %>%
        filter(n_games > 0) %>% 
        ungroup() %>% 
        mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
               'ppg' = n_points/n_games) %>%
        group_by(matchup_id) %>% 
        arrange(-ppg, n_games) %>%
        dplyr::slice(1:10) %>% 
        left_join(teams, by = 'team_id') %>%
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
        df_daily %>%
        filter(in_lineup, pitcher) %>%
        group_by(player_id, team_id, player, matchup_id) %>%
        summarise('n_games' = sum(relief),
                  'n_points' = sum(points[relief])) %>%
        filter(n_games > 0) %>% 
        filter(!is.na(n_games)) %>%
        mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
               'ppg' = n_points/n_games) %>%
        
        group_by(matchup_id) %>% 
        arrange(-n_points, n_games) %>%
        dplyr::slice(1:10) %>% 
        left_join(teams, by = 'team_id') %>%
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
      df_top <- bind_cols(df_bat, df_sp, df_rp)
      
      gt_top <- 
        gt(df_top) %>%
        
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
        )  %>% 
        
        ### Logos
        text_transform(
          locations = cells_body(contains(c('player_url'))),
          fn = function(x) {
            web_image(
              url = x,
              height = 50
            )
          }
        ) %>%
        
        text_transform(
          locations = cells_body(columns = contains(c('logo_rp')), 
                                 rows = (logo_rp != '---')),
          fn = function(x) {
            local_image(
              filename = x,
              height = 50
            )
          }
        ) %>%
        
        text_transform(
          locations = cells_body(columns = contains(c('logo_sp')), 
                                 rows = (logo_sp != '---')),
          fn = function(x) {
            local_image(
              filename = x,
              height = 50
            )
          }
        ) %>%
        
        text_transform(
          locations = cells_body(columns = contains(c('logo_bat')), 
                                 rows = (logo_bat != '---')),
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
  
  gt_best <- eventReactive(input$matchup_id, {
    cat('Computing Lineup of the Week\n')
    df_best <- 
      df_log %>% 
      filter(matchup_id == input$matchup_id) %>% 
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
      mutate('lineup_id' = ifelse(lineup_id == 5, 11, lineup_id)) %>% 
      mutate('n_times' = map_dbl(player_id, ~sum(.x == df_log$player_id, na.rm = T))) %>% 
      mutate('player' = ifelse(is.na(player), NA, paste0(player, ' (', n_times, ')'))) %>% 
      left_join(select(teams, team, team_id, logo), by = 'team_id') %>% 
      arrange(lineup_id)  %>% 
      select(position, player, player_url, team, logo, points, ppg)
    
    if(nrow(df_best) > 0) {
      
      gt_best <- 
        gt(df_best) %>%
        
        ### Align Columns
        cols_align(align = "center", columns = everything()) %>%
        
        fmt_number(columns = contains('ppg'), decimals = 2, sep_mark = '') %>% 
        fmt_number(columns = contains('points'), decimals = 1, sep_mark = '') %>% 
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
          locations = cells_body(contains(c('player_url'))),
          fn = function(x) {
            web_image(
              url = x,
              height = 50
            )
          }
        ) %>%
        
        text_transform(
          locations = cells_body(columns = contains(c('logo')), 
                                 rows = !is.na(logo)),
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
          subtitle = md(glue('**Week: {week()}**'))
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
    } else {
      rv$gt_best <- 
        gt(df_best) %>%
        
        ### Align Columns
        cols_align(align = "center", columns = everything()) %>%
        
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
          subtitle = md(glue('**Week: {week()}**'))
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
      
    }
    gt_best
  })
  
  
  
  
  
  output$best_lineup <- render_gt({
    cat('Rendering Best Lineup of the Week\n')
    gt_best()
  })
  
  output$top_performers <- render_gt({
    cat('Rendering Top Performers of the Week\n')
    gt_top()
  })
  
  
  ################
  ### Playoffs ###
  ################
  output$playoff_history <- renderPlot({
    cat('Rendering Playoff + ferry History\n')
    read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>% 
      rename('ferry' = 'last_place') %>% 
      pivot_longer(cols = c('playoffs', 'ferry', 'champ'),
                   names_to = 'type',
                   values_to = 'odds') %>% 
      select(matchup_id, odds, type, team) %>% 
      ggplot(aes(x = matchup_id, y = odds, color = type, fill = type)) +
      facet_wrap(~team) +
      geom_line(lwd = 1.2) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = seq(0, params$current_matchup, 2)) +
      scale_color_discrete(labels = c('Champion', 'Ferry', 'Playoffs')) + 
      scale_fill_discrete(labels = c('Champion', 'Ferry', 'Playoffs')) + 
      labs(x = 'Week',
           y = 'Odds',
           color = '',
           fill = '',
           title = 'League Odds Over Time') +
      theme(axis.text = element_text(size = 12),
            legend.position = 'bottom')
  },
  height = 600,
  width = 1067)
  
  output$win_dist <- renderPlot({
    cat('Rendering Win Distributions\n')
    distributions %>% 
      mutate('team_' = fct_relevel(team, sim_results$team[order(sim_results$mean_wins)])) %>% 
      select(team, team_, wins) %>% 
      ggplot(aes(x = wins, y = team_, fill = team)) +
      ggridges::geom_density_ridges(stat = "binline", scale = 0.7, binwidth = 1, rel_min_height = 0.02) +
      scale_x_continuous(breaks = 0:21) +
      labs(x = "# of Wins",
           y = "Team",
           title = "Distribution of Wins",
           subtitle = 'Across 10,000 Season Simulation') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))
  },
  height = 600,
  width = 1067)
  
  output$points_dist <- renderPlot({
    cat('Rendering Points Distributions\n')
    
    distributions %>% 
      mutate('team_' = fct_relevel(team, sim_results$team[order(sim_results$mean_pts)])) %>% 
      select(team, team_, points) %>% 
      ggplot(aes(x = points, y = team_, fill = team)) +
      ggridges::geom_density_ridges(scale = 0.9, quantiles = 2, quantile_lines = T, rel_min_height = 0.02) +
      labs(x = "# of Points",
           y = "Team",
           title = "Distribution of Points",
           subtitle = 'Across 10,000 Season Simulation') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))
  },
  height = 600,
  width = 1067)
  
  
  ##############################
  ### Win Probability Charts ###
  ##############################
  df_wp <- eventReactive(input$matchup_id_wp, {
    cat('Computing WP\n')
    read_csv(glue('data/win_prob/{params$season}/week_{input$matchup_id_wp}.csv')) %>% 
      mutate('start_factor' = factor(case_when(start_advantage >= 4 ~ '> +3',
                                               start_advantage <= -4 ~ '< -3',
                                               start_advantage > 0 ~ paste0('+', start_advantage),
                                               start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                               T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                    '+1', '+2', '+3', '> +3'))) %>% 
      select(day_of_matchup, win_prob, team_home, team_away, start_factor, days_left)
    
  })
  
  output$wp_plot <- renderPlot({
    cat('Rendering WP Plot\n')
    
    df_image <-
      df_wp() %>% 
      left_join(teams, by = c('team_home' = 'team')) %>% 
      left_join(teams, by = c('team_away' = 'team'), suffix = c('_home', '_away')) %>% 
      distinct(team_home, team_away, logo_home, logo_away)
    
    
    plot <-
      ggplot(df_wp(), aes(x = day_of_matchup, y = win_prob)) + 
      facet_wrap(~paste(team_home, 'vs.', team_away)) + 
      geom_line() +
      geom_point(aes(fill = start_factor), size = 8, color = 'black', pch = 21) +
      geom_image(data = df_image %>% select(team_home, team_away, logo_home), aes(x = 0.4, y = 0.95, image = logo_home), size = 0.15) +
      geom_image(data = df_image %>% select(team_away, team_home, logo_away), aes(x = 0.4, y = 0.05, image = logo_away), size = 0.15) +
      theme_bw() + 
      theme(plot.title = element_text(size = 24, hjust = 0.5),
            axis.title = element_text(size = 16),
            strip.text =  element_text(size = 14),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom')  + 
      labs(x = 'Day of Matchup',
           y = 'Win Probability',
           title = 'Win Probability Charts',
           subtitle = paste('Week:', input$matchup_id_wp),
           fill = 'Start Advantage') +
      scale_y_continuous(limits = c(0,1), labels = function(x){ paste0(100 * pmax(x, 1-x), '%') }) +
      scale_x_continuous(limits = c(0, max(df_wp()$days_left)), breaks = 0:max(df_wp()$days_left)) +
      scale_fill_brewer(palette = 'RdYlGn', drop = FALSE) + 
      guides(fill = guide_legend(nrow = 3)) + 
      geom_label(data = filter(df_wp(), day_of_matchup == max(day_of_matchup)) ,
                 aes(x = 2.5, y = 1, label = paste0(sprintf('%0.1f', 100 * (win_prob)), '%')),
                 size = 8) +
      geom_label(data = filter(df_wp(), day_of_matchup == max(day_of_matchup)) ,
                 aes(x = 2.5, y = 0, label = paste0(sprintf('%0.1f', 100 * (1-win_prob)), '%')),
                 size = 8) 
    # waiter::waiter_hide()
    plot
    
  },
  width = 1600,
  height = 900)
  
  output$leverage_plot <- 
    plotly::renderPlotly({
      
      p <- 
        ggplot(leverage_long, aes(x = prob, y = fct_reorder(label, leverage))) + 
        facet_wrap(~fct_rev(tools::toTitleCase(event)), ncol = 1, scales = 'free_y') + 
        geom_line() +
        geom_point(aes(color = win_loss, alpha = win_loss, text = text), size = 4) + 
        scale_x_continuous(limits = c(0,1), labels = scales::percent) + 
        scale_color_manual(values = c('darkgrey', 'red', 'seagreen'), labels = c('Current', 'Loss', 'Win')) + 
        scale_alpha_manual(values = c(0.6, 1, 1), guide = 'none') +
        theme(legend.position = 'none',
              axis.title = element_text(size = 24),
              axis.text = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 24),
              plot.title = element_text(hjust = 0.5, size = 28)) + 
        labs(x = 'Probability', 
             y = '',
             color = 'Result This Week',
             alpha = 'Result This Week',
             title = 'Playoff + Ferry Leverage')
      
      plotly::ggplotly(p, tooltip = 'text',
                       'height' = 900,
                       'width' = 1600) %>% 
        plotly::layout('legend' = list(
          xanchor='center',
          x = 0.5,
          yanchor='bottom',
          orientation='h')
        )
      
      
      
      
    })
    
    ######################
    ### Points by Week ###
    ######################
    output$ppw_1 <- renderPlot({
      
      ggplot(team_points,  aes(x = matchup_id, y = adj_pts)) +
        facet_wrap(~team) +
        geom_point(data = select(team_points, matchup_id, adj_pts),
                   aes(x = matchup_id, y = adj_pts), fill = "grey", alpha = 0.2) +
        geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
        geom_point(aes(color = team), size = 2) +
        labs(x = "Week",
             y = "Points",
             subtitle = "Total Points (Normalized to 7 Day Matchup)",
             title = "Fantasy Points by Week") +
        scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
    },
    height = 750,
    width = 1334)
    
    output$ppw_2 <- renderPlot({
      ggplot(team_points,  aes(x = matchup_id, y = adj_batting_pts)) +
        facet_wrap(~team) +
        geom_point(data = select(team_points, matchup_id, adj_batting_pts),
                   aes(x = matchup_id, y = adj_batting_pts), fill = "grey", alpha = 0.2) +
        geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
        geom_point(aes(color = team), size = 2) +
        labs(x = "Week",
             y = "Points",
             subtitle = "Batting Points (Normalized to 7 Day Matchup)",
             title = "Fantasy Points by Week") +
        scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
    },
    height = 750,
    width = 1334)
    
    output$ppw_3 <- renderPlot({
      ggplot(team_points,  aes(x = matchup_id, y = adj_pitching_pts)) +
        facet_wrap(~team) +
        geom_point(data = select(team_points, matchup_id, adj_pitching_pts),
                   aes(x = matchup_id, y = adj_pitching_pts), fill = "grey", alpha = 0.2) +
        geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
        geom_point(aes(color = team), size = 2) +
        labs(x = "Week",
             y = "Points",
             subtitle = "Pitching Points (Normalized to 7 Day Matchup)",
             title = "Fantasy Points by Week") +
        scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
    },
    height = 750,
    width = 1334)
    
    output$bump <- renderPlot({
      
      ggplot(df_points, aes(x = scoring_period_id, y = rank)) + 
        facet_wrap(~team) +
        geom_vline(data = df_start %>% filter(matchup_id <= params$current_matchup), aes(xintercept = end_period), lty = 2, alpha = 0.5) +
        # geom_line(data = rename(df_points, 'team2' = team), aes(group = team2), col = 'grey', alpha = 0.5, lineend = 'round') +
        ggbump::geom_bump(aes(col = team), lwd = 1.2, lineend = 'round') +
        scale_y_reverse(limits = c(12, 1), breaks = 12:1) + 
        labs(x = 'Day of Season', 
             y = 'Rank by Points Scored',
             title = 'Scoring Rank Over Time') 
    }, 
    height = 750,
    width = 1334)
    
    ########################
    ### Rolling Averages ###
    ########################
    
    k_chart <- eventReactive(input$k, {
      cat('Computing K-Avg Chart\n')
      plot_k_avg(input$k)
    })
    
    output$roll_k <- renderPlot({
      cat('Rendering K-Avg Chart\n')
      k_chart()
    },
    height = 900, 
    width = 1600)
    
    ##########################
    ### Trades/Free Agents ###
    ##########################
    
    output$trade_chart <- render_gt({
      cat('Rendering Trade Chart\n')
      if(nrow(df_trades) > 0) {
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
                       locations = cells_column_labels(columns = contains('avg')),
                       placement = 'left') %>% 
          tab_footnote(footnote = "Total value accumulated of all players in trade (points scored vs. points that league average players at position(s) would be expected to score in time since trade)",
                       locations = cells_column_labels(columns = contains('value')),
                       placement = 'left') %>% 
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
      
    })
    output$fa_chart <- render_gt({
      cat('Rendering FA Chart\n')
      
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
    })
    
    output$fs_chart <- render_gt({
      cat('Rendering FS Chart\n')
      trans_log %>% 
        group_by(player, player_id) %>% 
        summarise('n_stints' = n_distinct(stint),
                  'n_teams' = n_distinct(team_id),
                  'points' = sum(n_points)) %>% 
        arrange(-n_stints, -n_teams, points) %>% 
        head(10) %>% 
        ungroup() %>% 
        mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
        select(player, player_url, n_stints, n_teams, points) %>% 
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
    })
    
    #################
    ### Penalties ###
    #################
    
    output$sp_pen <- render_gt({
      cat('Rendering SP Penalties\n')
      
      df_penalty %>% 
        select(team, logo, matchup_id, penalty) %>% 
        group_by(team, logo, matchup_id) %>%
        summarise('penalty' = sum(penalty)) %>%
        ungroup() %>%
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
      
      
    })
    output$rp_pen <- render_gt({
      cat('Rendering RP Penalties\n')
      
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
        tab_header(title = 'RP Cap/Usage Penalties') %>%
        tab_options(column_labels.font.size = 16,
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 40,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold',
                    row_group.font.weight = 'bold',
                    row_group.font.size  = 22)
      
    })
    
    ##################
    ### All Starts ###
    ##################
    output$asg_lineup <- render_gt({
      cat('Rendering All-Star Chart\n')
      
      gt(df_asg_lineup) %>%
        
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
          locations = cells_body(contains(c('player_url'))),
          fn = function(x) {
            web_image(
              url = x,
              height = 50
            )
          }
        ) %>%
        
        text_transform(
          locations = cells_body(contains(c('logo'))),
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
          title = md(glue('**{params$season} Fantasy All-Stars**')),
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
        tab_footnote(footnote = "Min Games for Inclusion: Batter (30), SP (6), RP (10)") %>% 
        tab_footnote(footnote = "Only includes games players in starting fantasy lineup") 
      
      
    })
    output$asg_counts <- render_gt({
      cat('Rendering All-Star Chart Counts\n')
      
      
      gt(df_asg_counts) %>% 
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
          title = md(glue('**{params$season} Fantasy All-Stars**')),
        ) %>%
        tab_options(column_labels.font.size = 20,
                    heading.title.font.size = 40,
                    heading.subtitle.font.size = 40,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold',
                    column_labels.font.weight = 'bold'
                    
        ) %>% 
        tab_footnote(footnote = "Star Points: 1st Team (3), 2nd Team (2), 3rd Team (1)") 
    })
    
    
    ### Draft 
    output$draft_plot <- 
      plotly::renderPlotly({
        p <-
          ggplot(draft_analysis, aes(x = pick_id, y = points_total)) + 
          facet_wrap(~team) +
          geom_smooth(data = select(draft_analysis, -team), col = 'black', alpha = 0.2, se = F) +
          geom_hline(yintercept = 0, lty = 2, col = 'grey') + 
          geom_point(aes(color = player_type, text = text), size = 5) + 
          theme(legend.position = 'bottom') + 
          labs(x = 'Pick Number',
               y = 'Points Scoring for Drafting Team',
               title = 'Draft Curves',
               color = 'Player Type'
          )
        
        plotly::ggplotly(p, tooltip = 'text') %>% 
          plotly::layout('legend' = list(
            xanchor='center',
            x = 0.5,
            yanchor='bottom',
            orientation='h'),
            'height' = 900,
            'width' = 1600
          )
      })
    
    
    output$gt_draft <- 
      render_gt({
        dfd <- 
          draft_analysis %>% 
          arrange(-residual) %>% 
          mutate('pitcher' = player_type == 'batter') %>% 
          inner_join(teams %>% select(team, logo)) %>% 
          change_logo() %>% 
          mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'))
        
        dfd_bat <- 
          dfd %>% 
          filter(player_type == 'Batter') %>% 
          head(20) %>% 
          select(player, player_url, team, logo, pick_id, round_id, points_total, points_draft, ppg, ppg_draft, fit, residual)
        
        dfd_pitch <- 
          dfd %>% 
          filter(player_type != 'Batter') %>% 
          head(20) %>% 
          select(player, player_url, team, logo, pick_id, round_id, points_total, points_draft, ppg, ppg_draft, fit, residual)
        
        names(dfd_bat) <- paste0(names(dfd_bat), '_bat')
        names(dfd_pitch) <- paste0(names(dfd_pitch), '_pitch')
        
        # gt_draft <- 
        bind_cols(dfd_bat, dfd_pitch) %>% 
          gt() %>% 
          cols_align('center') %>% 
          tab_spanner(label = 'Pitching', columns = contains('_pitch')) %>%
          tab_spanner(label = 'Batting', columns = contains('_bat')) %>%
          sub_missing(columns = everything(), missing_text = "---") %>% 
          fmt_number(contains(c('ppg', 'fit', 'residual')), decimals = 1) %>% 
          
          
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
          )  %>% 
          
          ### Logos
          text_transform(
            locations = cells_body(contains(c('player_url'))),
            fn = function(x) {
              web_image(
                url = x,
                height = 50
              )
            }
          ) %>%
          
          text_transform(
            locations = cells_body(columns = contains(c('logo_pitch')), 
                                   rows = (logo_pitch != '---')),
            fn = function(x) {
              local_image(
                filename = x,
                height = 50
              )
            }
          ) %>%
          
          text_transform(
            locations = cells_body(columns = contains(c('logo_bat')), 
                                   rows = (logo_bat != '---')),
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
                weight = px(3)
              )
            ),
            locations = list(
              cells_body(
                columns = contains('residual')
              )
            )
          ) %>% 
          
          
          ### Names
          cols_label(
            'team_bat' = 'Team',
            'logo_bat' = '',
            'player_url_bat' = '',
            'player_bat' = 'Player',
            'points_total_bat' = 'Points',
            'points_draft_bat' = 'Points (Draft Team)',
            'ppg_bat' = 'PPG',
            'ppg_draft_bat' = 'PPG (Draft Team)',
            'pick_id_bat' = 'Pick',
            'round_id_bat' = 'Round',
            'fit_bat' = 'Exp. Pick Value',
            'residual_bat' = 'Residual',
            
            'team_pitch' = 'Team',
            'logo_pitch' = '',
            'player_url_pitch' = '',
            'player_pitch' = 'Player',
            'points_total_pitch' = 'Points',
            'points_draft_pitch' = 'Points (Draft Team)',
            'ppg_pitch' = 'PPG',
            'ppg_draft_pitch' = 'PPG (Draft Team)',
            'pick_id_pitch' = 'Pick',
            'round_id_pitch' = 'Round',
            'fit_pitch' = 'Exp. Pick Value',
            'residual_pitch' = 'Residual'
            
            
            
          ) %>%
          tab_header(
            title = md('**Top Draft Picks**')
          ) %>%
          tab_options(column_labels.font.size = 12,
                      heading.title.font.size = 40,
                      heading.subtitle.font.size = 40,
                      heading.title.font.weight = 'bold',
                      heading.subtitle.font.weight = 'bold',
                      column_labels.font.weight = 'bold')
        
        
        
        
      })
    
    output$whatif_table <- render_gt({
      gt(df_whatif) %>% 
        cols_hide(contains('win_pct')) %>% 
        cols_align('center') %>% 
        data_color(columns = contains('win_pct'),
                   target_columns = contains('record'),
                   fn = scales::col_numeric(palette = 'RdYlGn', domain = range(df_whatif %>% select(contains('win_pct'))))) %>% 
        text_transform(locations = cells_body(c(logo_1)),
                       fn = function(x) {
                         local_image(filename = x, height = 30)
                       }) %>% 
        cols_label_with(columns = 2:13, fn = function(x) {
          html(local_image(filename = gsub('record_', '', x), height = 30))
        }) %>% 
        cols_label('logo_1' = 'Team') %>% 
        tab_spanner(columns = contains('record'), label = 'vs. This Team\'s Schedule') %>% 
        tab_header(title = md('**Record vs. Each Team\'s Schedule**')) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 1, columns = 2)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 2, columns = 3)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 3, columns = 4)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 4, columns = 5)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 5, columns = 6)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 6, columns = 7)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 7, columns = 8)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 8, columns = 9)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 9, columns = 10)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 10, columns = 11)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 11, columns = 12)) %>% 
        tab_style(style = cell_borders(sides = 'all', color = 'black', weight = px(3), style = "solid"), locations = cells_body(rows = 12, columns = 13)) %>% 
        tab_options(column_labels.font.size = 16,
                    column_labels.font.weight = 'bold',
                    heading.title.font.size = 50,
                    heading.subtitle.font.size = 20,
                    heading.title.font.weight = 'bold',
                    heading.subtitle.font.weight = 'bold'
        ) 
      
      
      
      
    })
    
    
    output$positional_ppg <- renderPlot({
      
      ggplot(lineup_stats, aes(x = ppg, y = fct_rev(lineup_id))) + 
        facet_wrap(~team) +
        geom_point(data = lineup_avg, size = 3, shape = 18) + 
        geom_point(aes(fill = ppg - ppg_avg), size = 4, pch = 21, color = 'black') + 
        scale_fill_gradient2(breaks = c(-1.25, -0.75, -0.25, 0.25, 0.75, 1.25),low = 'blue', mid = 'lightgrey', high = 'red', midpoint = 0) + 
        theme(legend.text = element_text(angle = 90, size = 12),
              legend.position = 'bottom',
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 16)) + 
        labs(x = 'PPG', 
             y = '',
             fill = 'PPG vs. League Avg at Position',
             title = 'Positional PPG Relative to League Average') 
    }, 
    height = 750,
    width = 1334)
    
    
    
    outputOptions(output, 'stats_table', suspendWhenHidden = FALSE)
    outputOptions(output, 'bat_table', suspendWhenHidden = FALSE)
    outputOptions(output, 'pitch_table', suspendWhenHidden = FALSE)
    # outputOptions(output, "top_performers", suspendWhenHidden = FALSE, priority = 2)
    # outputOptions(output, "best_lineup", suspendWhenHidden = FALSE, priority = 2)
    # outputOptions(output, 'trade_chart', suspendWhenHidden = FALSE, priority = 2)
    # outputOptions(output, 'fa_chart', suspendWhenHidden = FALSE, priority = 2)
    # outputOptions(output, 'fs_chart', suspendWhenHidden = FALSE, priority = 2)
    outputOptions(output, 'wp_plot', suspendWhenHidden = FALSE, priority = 1)
    
    
    #   outputOptions(output, 'win_dist', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'points_dist', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'playoff_history', suspendWhenHidden = FALSE)
    #   
    #   outputOptions(output, 'bump', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'ppw_1', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'ppw_2', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'ppw_3', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'roll_k', suspendWhenHidden = FALSE)
    # 
    #   outputOptions(output, 'asg_lineup', suspendWhenHidden = FALSE)
    #   outputOptions(output, 'asg_counts', suspendWhenHidden = FALSE)
    
    
    #   
    #   outputOptions(output, "sp_pen", suspendWhenHidden = FALSE)
    #   outputOptions(output, "rp_pen", suspendWhenHidden = FALSE)
    
    
    
    
    
    
    
})
  
  
  
  
  
  