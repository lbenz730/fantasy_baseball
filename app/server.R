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
  output$stats_table <- renderUI({
    HTML(paste(readLines('data/cache/stats_table.html', warn = FALSE), collapse = '\n'))
  })

  output$pitch_table <- renderUI({
    HTML(paste(readLines('data/cache/pitch_table.html', warn = FALSE), collapse = '\n'))
  })

  output$bat_table <- renderUI({
    HTML(paste(readLines('data/cache/bat_table.html', warn = FALSE), collapse = '\n'))
  })

  
  output$sp_matrix <- renderImage({
    list(src = file.path('www', 'cache', 'sp_matrix.png'),
         contentType = 'image/png', width = 1600, height = 900)
  }, deleteFile = FALSE)
  
  output$start_buckets <- renderImage({
    list(src = file.path('www', 'cache', 'start_buckets.png'),
         contentType = 'image/png', width = 1600, height = 900)
  }, deleteFile = FALSE)
  
  
  
  ######################
  ### Top Performers ###
  ######################

  week <- eventReactive(input$matchup_id, {
    input$matchup_id
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
  
  output$top_performers <- renderUI({
    w <- input$matchup_id
    f <- paste0('data/cache/top_performers_', params$season, '_week_', w, '.html')
    if(file.exists(f)) {
      HTML(paste(readLines(f, warn = FALSE), collapse = '\n'))
    } else {
      HTML('<p>Top performers data not yet available for this week.</p>')
    }
  })
  
  
  ################
  ### Playoffs ###
  ################
  output$playoff_history <- renderImage({
    list(src = file.path('www', 'cache', 'playoff_history.png'),
         contentType = 'image/png', width = 1067, height = 600)
  }, deleteFile = FALSE)
  
  output$win_dist <- renderImage({
    list(src = file.path('www', 'cache', 'win_dist.png'),
         contentType = 'image/png', width = 1067, height = 600)
  }, deleteFile = FALSE)

  output$points_dist <- renderImage({
    list(src = file.path('www', 'cache', 'points_dist.png'),
         contentType = 'image/png', width = 1067, height = 600)
  }, deleteFile = FALSE)
  
  
  ##############################
  ### Win Probability Charts ###
  ##############################
  output$wp_plot <- renderImage({
    w <- input$matchup_id_wp
    list(src          = file.path('www', 'cache', paste0('wp_', params$season, '_week_', w, '.png')),
         contentType  = 'image/png',
         width        = 1600,
         height       = 900)
  }, deleteFile = FALSE)
  
  output$leverage_plot <- 
    plotly::renderPlotly({
      
      if(params$current_matchup <= max(df_start$matchup_id[!df_start$playoffs])) {
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
      } else {
        p <- 
          ggplot() + 
          theme_void()
      }
      
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
  output$ppw_1 <- renderImage({
    list(src = file.path('www', 'cache', 'ppw_1.png'),
         contentType = 'image/png', width = 1334, height = 750)
  }, deleteFile = FALSE)

  output$ppw_2 <- renderImage({
    list(src = file.path('www', 'cache', 'ppw_2.png'),
         contentType = 'image/png', width = 1334, height = 750)
  }, deleteFile = FALSE)

  output$ppw_3 <- renderImage({
    list(src = file.path('www', 'cache', 'ppw_3.png'),
         contentType = 'image/png', width = 1334, height = 750)
  }, deleteFile = FALSE)

  output$bump <- renderImage({
    list(src = file.path('www', 'cache', 'bump.png'),
         contentType = 'image/png', width = 1334, height = 750)
  }, deleteFile = FALSE)
  
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
  
  k_chart_bat <- eventReactive(input$k_bat, {
    roll_avg_plot(total_stats = mlb_batting, 
                  date_range = input$roll_bat_date_range, 
                  player = names(input$roll_batter), 
                  player_id = as.numeric(input$roll_batter), 
                  stat_col = input$stat_col_bat, 
                  rolling_sum = as.logical(input$roll_bat_roll_sum), 
                  starter_only = as.logical(input$roll_bat_starter_filter),
                  roll_k = as.numeric(input$roll_bat_k),
                  batter = T)
  })
  
  output$roll_k_bat <- renderPlot({
    cat('Rendering K-Avg Chart Batter\n')
    k_chart_bat()
  },
  height = 675, 
  width = 1200)
  
  k_chart_pitch <- eventReactive(input$k_pitch, {
    roll_avg_plot(total_stats = mlb_pitching, 
                  date_range = input$roll_pitch_date_range, 
                  player = names(input$roll_pitcher), 
                  player_id = as.numeric(input$roll_pitcher), 
                  stat_col = input$stat_col_pitch, 
                  rolling_sum = as.logical(input$roll_pitch_roll_sum), 
                  starter_only = as.logical(input$roll_pitch_starter_filter),
                  roll_k = as.numeric(input$roll_pitch_k),
                  batter = F)
  })
  
  output$roll_k_pitch <- renderPlot({
    cat('Rendering K-Avg Chart Pitchern')
    k_chart_pitch()
  },
  height = 675, 
  width = 1200)
  
  
  ##########################
  ### Trades/Free Agents ###
  ##########################
  
  output$trade_chart <- renderUI({
    HTML(paste(readLines('data/cache/trade_chart.html', warn = FALSE), collapse = '\n'))
  })
  output$fa_chart <- renderUI({
    HTML(paste(readLines('data/cache/fa_chart.html', warn = FALSE), collapse = '\n'))
  })
  output$fs_chart <- renderUI({
    HTML(paste(readLines('data/cache/fs_chart.html', warn = FALSE), collapse = '\n'))
  })

  #################
  ### Penalties ###
  #################

  output$sp_pen <- renderUI({
    HTML(paste(readLines('data/cache/sp_pen.html', warn = FALSE), collapse = '\n'))
  })

  output$rp_pen <- renderUI({
    HTML(paste(readLines('data/cache/rp_pen.html', warn = FALSE), collapse = '\n'))
  })

  output$rp_start <- renderUI({
    HTML(paste(readLines('data/cache/rp_start.html', warn = FALSE), collapse = '\n'))
  })

  ##################
  ### All Starts ###
  ##################

  output$asg_lineup <- renderUI({
    if(file.exists('data/cache/asg_lineup.html')) {
      HTML(paste(readLines('data/cache/asg_lineup.html', warn = FALSE), collapse = '\n'))
    } else {
      HTML('<p>All-Star data not yet available (requires at least 7 matchups).</p>')
    }
  })

  output$asg_counts <- renderUI({
    if(file.exists('data/cache/asg_counts.html')) {
      HTML(paste(readLines('data/cache/asg_counts.html', warn = FALSE), collapse = '\n'))
    } else {
      HTML('<p>All-Star data not yet available (requires at least 7 matchups).</p>')
    }
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
          xanchor = 'center',
          x = 0.5,
          yanchor = 'bottom',
          orientation = 'h'),
          'height' = 900,
          'width' = 1600
        )
    })

  output$gt_draft <- renderUI({
    HTML(paste(readLines('data/cache/gt_draft.html', warn = FALSE), collapse = '\n'))
  })

  output$whatif_table <- renderUI({
    HTML(paste(readLines('data/cache/whatif_table.html', warn = FALSE), collapse = '\n'))
  })

  output$positional_ppg <- renderImage({
    list(src = file.path('www', 'cache', 'positional_ppg.png'),
         contentType = 'image/png', width = 1334, height = 750)
  }, deleteFile = FALSE)

  ### League History ###

  output$league_history_table <- renderUI({
    HTML(paste(readLines('data/cache/league_history_table.html', warn = FALSE), collapse = '\n'))
  })

  output$wl_mat <- renderUI({
    HTML(paste(readLines('data/cache/wl_mat.html', warn = FALSE), collapse = '\n'))
  })
  
  
  
  
  #######################
  ### Weekly Summary  ###
  #######################
  
  
  weekly_scoreboardOutput <- eventReactive(input$matchup_id_weekly, {
    if(params$current_matchup > 1) {
      df_week <- 
        team_points %>% 
        filter(matchup_id == input$matchup_id_weekly,
               !is.na(total_points))
      logo_lkup <- setNames(teams$logo, teams$team)
      make_scoreboard_table(df_week, params$season, input$matchup_id_weekly, logo_lkup)
    } else {
      gt(data = tibble('winner' = NULL, 
                       'loser' = NULL))  
    }
  })
  
  output$weekly_scoreboard <- render_gt(weekly_scoreboardOutput())
  
  weekly_standingsOutput <- eventReactive(input$matchup_id_weekly, {
    if(params$current_matchup > 1) {
      playoff_hist  <- read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv'),
                                show_col_types = FALSE)
      logo_lkup     <- setNames(teams$logo, teams$team)
      make_standings_table(exp_standings, playoff_hist, as.numeric(input$matchup_id_weekly), params$season, logo_lkup, team_points)
    } else {
      gt(data = tibble('winner' = NULL, 
                       'loser' = NULL)) 
    }
  })
  
  
  
  output$weekly_standings <- render_gt({
    weekly_standingsOutput()
  })
  
  
  
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









