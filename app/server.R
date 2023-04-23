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
  # ### Update Parameterizations
  # updateSelectInput(session, inputId = "matchup_id", 
  #                   choices = (params$current_matchup:1), selected = params$current_matchup)
  # updateSelectInput(session, inputId = "matchup_id_wp", 
  #                   choices = (params$current_matchup:1), selected = params$current_matchup)
  
  ######################
  ### Advanced Stats ###
  ######################
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
    # df$logo[13] <- 'https://i.imgur.com/h3Vd6b8.png'
    df$logo[13] <- 'www/League.png'
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
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, max(df$win + df$loss, na.rm = T))),
                 autocolor_text = F) %>%
      data_color(columns = c(batting_points),
                 fn= scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$batting_points, na.rm = T)),
                 autocolor_text = F) %>%
      data_color(columns = c(pitching_points),
                 fn= scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$pitching_points, na.rm = T)),
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
      data_color(columns = c(adj_pitching_pts),
                 fn = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df$adj_pitching_pts, na.rm = T)),
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
      tab_spanner(label = 'Total Points', columns = c('batting_points', 'pitching_points', 'total_points')) %>%
      tab_spanner(label = 'Expected Record', columns = c('exp_win', 'exp_loss')) %>%
      tab_spanner(label = 'Record', columns = c('win', 'loss')) %>%
      tab_spanner(label = 'Points Per Player Appearance', columns = c('batting_ppg', 'rp_ppg', 'sp_ppg', 'qs_pct')) %>%
      tab_spanner(label = 'Points Per Matchup', columns = c('adj_batting_pts', 'adj_pitching_pts', 'adj_pts')) %>%
      
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
      
      ### Check
      # text_transform(
      #   locations = cells_body(
      #     columns = c(playoffs),
      #     rows = playoffs == 1
      #   ),
      #   fn = function(x) {
      #     emo::ji('check')
      #   }
      # ) %>%
      # 
      # text_transform(
      #   locations = cells_body(
      #     columns = c(playoffs),
      #     rows = playoffs == 0
      #   ),
      #   fn = function(x) {
      #     emo::ji('x')
      #   }
      # ) %>%
      # 
      # text_transform(
      #   locations = cells_body(
      #     columns = c(last_place),
      #     rows = last_place == 0
      #   ),
      #   fn = function(x) {
      #     emo::ji('x')
      #   }
      # ) %>%
      # 
      # text_transform(
      #   locations = cells_body(
      #     columns = c(last_place),
      #     rows = last_place == 1
      #   ),
      #   fn = function(x) {
      #     emo::ji('check')
      #   }
      # ) %>%
      # 
      # text_transform(
      #   locations = cells_body(
      #     columns = c(champ),
      #     rows = champ == 0
      #   ),
      #   fn = function(x) {
      #     emo::ji('x')
      #   }
      # ) %>%
      # 
      # text_transform(
      #   locations = cells_body(
      #     columns = c(champ),
      #     rows = champ == 1
      #   ),
      #   fn = function(x) {
      #     emo::ji('check')
      #   }
      # ) %>%
      
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
        locations = cells_column_spanners(spanners = "Expected Record")
      ) %>%
      tab_footnote(
        footnote = "Points Per Matchup adjusted as average per 7-day matchup",
        locations = cells_column_spanners(spanners = "Points Per Matchup")
      ) %>%
      tab_footnote(
        footnote = "10,000 simulations of remainder of season",
        locations = cells_column_spanners(spanners = "Season Simulations")
      )
    
    gt1
    
  })
  
  output$pitch_table <- render_gt({
    
    df_ps %>% 
      gt() %>% 
      
      ### Round Numbers
      fmt_number(columns = c(era, era_sp, era_rp, 
                             fip, fip_sp, fip_rp, 
                             k9, k9_sp, k9_rp, 
                             bb9, bb9_sp, bb9_rp, 
                             k_per_bb, k_per_bb_sp, k_per_bb_rp,
                             hr9, hr9_sp, hr9_rp), decimals = 2, sep_mark = '') %>% 
      fmt_number(columns = c(qs, blue_balls), decimals = 2, drop_trailing_zeros = T) %>% 
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
        blue_balls = 'Blue Balls'
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
        locations = cells_column_labels('blue_balls')
      ) 
    
  })
  
  
  output$bat_table <- render_gt({
    
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
    
    ggplot(pitch_matrix, aes(x = ip, y = earned_runs))  + 
      facet_wrap(~team) + 
      geom_rect(aes(xmin = '< 3', xmax = 'CG',
                    ymin = '4', ymax = '5+'),
                col = NA,
                fill = 'red',
                alpha = 0.01) +
      geom_rect(aes(xmin = '< 3', xmax = '6.0',
                    ymin = '3', ymax = '4'),
                col = NA,
                fill = 'red',
                alpha = 0.01) +
      geom_rect(aes(xmin = '< 3', xmax = '5.0',
                    ymin = '0', ymax = '3'),
                col = NA,
                fill = 'red',
                alpha = 0.01) +
      
      geom_rect(aes(xmin = '5.0', xmax = '6.0',
                    ymin = '0', ymax = '3'),
                col = 'orange',
                fill = 'orange',
                alpha = 0.01) +
      geom_rect(aes(xmin = '6.0', xmax = 'CG',
                    ymin = '3', ymax = '4'),
                col = 'orange',
                fill = 'orange',
                alpha = 0.01) +
      geom_rect(aes(xmin = '6.0', xmax = 'CG',
                    ymin = '0', ymax = '3'),
                col = 'seagreen',
                fill = 'mediumspringgreen',
                alpha = 0.01) +
  
      geom_label(aes(label = n, fill = start_type)) + 
      scale_x_discrete(drop = F) + 
      scale_fill_manual(values = c('salmon', 'lightskyblue', 'violet', 'seagreen3', 'orange')) + 
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
      theme(axis.text.x = element_text(size = 8),
            plot.caption = element_text(size = 12),
            plot.title = element_text(size = 24),
            axis.title = element_text(size = 20),
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 12)
            )
    
    
  })
  
  ######################
  ### Top Performers ###
  ######################
  
  week <- eventReactive(input$matchup_id, {
    input$matchup_id
  })
  
  df_top <- eventReactive(input$matchup_id, {
    
    
    
    ### Top Performers
    df_bat <-
      batter_points %>%
      filter(!is.na(n_games)) %>%
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254'),
             'ppg' = n_points/n_games) %>%
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
  
  df_best <- eventReactive(input$matchup_id, {
    df_log %>% 
      filter(matchup_id == input$matchup_id) %>% 
      mutate('player_url' = glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/{player_id}.png&w=350&h=254')) %>% 
      mutate('lineup_id' = ifelse(lineup_id == 5, 11, lineup_id)) %>% 
      mutate('n_times' = map_dbl(player_id, ~sum(.x == df_log$player_id, na.rm = T))) %>% 
      mutate('player' = paste0(player, ' (', n_times, ')')) %>% 
      left_join(select(teams, team, team_id, logo)) %>% 
      arrange(lineup_id)  %>% 
      select(position, player, player_url, team, logo, points, ppg)
    
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
  
  output$best_lineup <- render_gt({
    
    if(nrow(df_best()) > 0) {
    
    gt_best <-
      gt(df_best()) %>%
      
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
      gt_best <- 
        gt(df_best()) %>%
        
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
  
  ################
  ### Playoffs ###
  ################
  output$playoff_history <- renderPlot({
    ggplot(history, aes(x = matchup_id, y = playoffs, fill = team)) +
      facet_wrap(~team) +
      geom_line(lwd = 1.2, aes(color = team)) +
      geom_point(size = 2, color = 'black', pch = 21) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = 0:params$current_matchup) +
      labs(x = 'Week',
           y = 'Playoff Odds',
           title = 'Playoff Odds Over Time') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))
  })
  
  output$ferry_history <- renderPlot({
    ggplot(history, aes(x = matchup_id, y = last_place, fill = team)) +
      facet_wrap(~team) +
      geom_line(lwd = 1.2, aes(color = team)) +
      geom_point(size = 2, color = 'black', pch = 21) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = 0:params$current_matchup) +
      labs(x = 'Week',
           y = 'Ferry Odds',
           title = 'Ferry Odds Over Time') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))
  })
  
  output$win_dist <- renderPlot({
    ggplot(distributions, aes(x = wins, y = fct_reorder(team, mean_wins), fill = team)) +
      geom_density_ridges(stat = "binline", scale = 0.7, binwidth = 1) +
      scale_x_continuous(breaks = 0:20) +
      labs(x = "# of Wins",
           y = "Team",
           title = "Distribution of Wins",
           subtitle = 'Across 10,000 Season Simulation') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))

  })

  output$points_dist <- renderPlot({

    ggplot(distributions, aes(x = points, y = fct_reorder(team, mean_pts), fill = team)) +
      geom_density_ridges(scale = 0.9, quantiles = 2, quantile_lines = T) +
      labs(x = "# of Points",
           y = "Team",
           title = "Distribution of Points",
           subtitle = 'Across 100,000 Season Simulation') +
      theme(legend.position = "none",
            axis.text = element_text(size = 12))

  })
  
  
  ##############################
  ### Win Probability Charts ###
  ##############################
  df_wp <- eventReactive(input$matchup_id_wp, {
    
    plot_wp(season = params$season, 
            week = input$matchup_id_wp,
            plot = F,
            all =  hour(Sys.time()) > 17 | input$matchup_id_wp < params$current_matchup) %>% 
      mutate('start_factor' = factor(case_when(start_advantage >= 4 ~ '> +3',
                                               start_advantage <= -4 ~ '< -3',
                                               start_advantage > 0 ~ paste0('+', start_advantage),
                                               start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                               T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                    '+1', '+2', '+3', '> +3')))
    
  })
  
  output$wp_plot <- renderPlot({
    
    df_image <-
      df_wp() %>% 
      left_join(teams, by = c('team_home' = 'team')) %>% 
      left_join(teams, by = c('team_away' = 'team'), suffix = c('_home', '_away')) %>% 
      distinct(team_home, team_away, logo_home, logo_away)
    
    
    ggplot(df_wp(), aes(x = day_of_matchup, y = win_prob)) + 
      facet_wrap(~paste(team_home, 'vs.', team_away)) + 
      geom_line() +
      geom_point(aes(fill = start_factor), size = 8, color = 'black', pch = 21) +
      geom_image(data = df_image, aes(x = 0.4, y = 0.95, image = logo_home), size = 0.125) + 
      geom_image(data = df_image, aes(x = 0.4, y = 0.05, image = logo_away), size = 0.125) +
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
      geom_label(data = filter(df_wp(), day_of_matchup == max(day_of_matchup)),
                 aes(x = 2.5, y = 1, label = paste0(sprintf('%0.1f', 100 * win_prob), '%')),
                 size = 8) + 
      geom_label(data = filter(df_wp(), day_of_matchup == max(day_of_matchup)),
                 aes(x = 2.5, y = 0, label = paste0(sprintf('%0.1f', 100 * (1-win_prob)), '%')),
                 size = 8) 
    
  })
  
  ######################
  ### Points by Week ###
  ######################
  output$ppw_1 <- renderPlot({
    ggplot(team_points,  aes(x = matchup_id, y = adj_pts)) +
      facet_wrap(~team) +
      geom_point(data = select(team_points, -team),
                 aes(x = matchup_id, y = adj_pts), fill = "grey", alpha = 0.2) +
      geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
      geom_point(aes(color = team), size = 2) +
      labs(x = "Week",
           y = "Points",
           subtitle = "Total Points (Normalized to 7 Day Matchup)",
           title = "Fantasy Points by Week") +
      scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
  })
  
  output$ppw_2 <- renderPlot({
    ggplot(team_points,  aes(x = matchup_id, y = adj_batting_pts)) +
      facet_wrap(~team) +
      geom_point(data = select(team_points, -team),
                 aes(x = matchup_id, y = adj_batting_pts), fill = "grey", alpha = 0.2) +
      geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
      geom_point(aes(color = team), size = 2) +
      labs(x = "Week",
           y = "Points",
           subtitle = "Batting Points (Normalized to 7 Day Matchup)",
           title = "Fantasy Points by Week") +
      scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
  })
  
  output$ppw_3 <- renderPlot({
    ggplot(team_points,  aes(x = matchup_id, y = adj_pitching_pts)) +
      facet_wrap(~team) +
      geom_point(data = select(team_points, -team),
                 aes(x = matchup_id, y = adj_pitching_pts), fill = "grey", alpha = 0.2) +
      geom_line(data = mean_pts_by_week, alpha = 0.4, lty = 2) +
      geom_point(aes(color = team), size = 2) +
      labs(x = "Week",
           y = "Points",
           subtitle = "Pitching Points (Normalized to 7 Day Matchup)",
           title = "Fantasy Points by Week") +
      scale_x_continuous(limits = c(1, params$current_matchup), breaks = 0:params$current_matchup)
  })
  
  output$bump <- renderPlot({
    ggplot(df_points, aes(x = scoring_period_id, y = rank)) + 
      facet_wrap(~team) + 
      geom_vline(data = df_start %>% filter(matchup_id <= params$current_matchup), aes(xintercept = end_period), lty = 2) + 
      geom_bump(data = rename(df_points, 'team2' = team), aes(group = team2), col = 'grey', alpha = 0.5) +
      geom_bump(aes(col = team), lwd = 1.2) + 
      scale_y_reverse(limits = c(12, 1), breaks = 12:1) + 
      labs(x = 'Day of Season', 
           y = 'Rank by Points Scored', 
           title = 'Scoring Rank Over Time') 
  })
  
  ########################
  ### Rolling Averages ###
  ########################
  
  k_chart <- eventReactive(input$k, {
    plot_k_avg(input$k)
  })
  
  output$roll_k <- renderPlot(k_chart())
  
  ##########################
  ### Trades/Free Agents ###
  ##########################
  
  output$trade_chart <- render_gt(gt_trades)
  output$fa_chart <- render_gt(gt_fa)
  output$fs_chart <- render_gt(gt_fs)
  
  #################
  ### Penalties ###
  #################
  
  output$sp_pen <- render_gt(gt_start_cap)
  output$rp_pen <- render_gt(gt_rp_cap)
  
})





