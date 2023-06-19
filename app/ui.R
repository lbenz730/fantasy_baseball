library(shiny)
library(DT)
library(gt)

# Define UI 
shinyUI(navbarPage("Millburnish Fantasy Baseball",
                   id = 'navbar',
                   tabPanel("Stats",
                            value = 'stats',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              selectInput(inputId = 'stat_sort',
                                          label = 'Sort Stats Table',
                                          choices = list('Expected Wins' = 'exp_win',
                                                         'Expected Losses' = 'exp_loss',
                                                         'Wins' = 'win',
                                                         'Losses' = 'loss',
                                                         'Points' = 'total_points',
                                                         'Points (Batting)' = 'batting_points',
                                                         'Points (SP)' = 'sp_points',
                                                         'Points (RP)' = 'rp_points',
                                                         'PPG (Bat)' = 'batting_ppg',
                                                         'PPG (SP)' = 'sp_ppg',
                                                         'PPG (RP)' = 'rp_ppg',
                                                         'QS %' = 'qs_pct',
                                                         'Playoff %' = 'playoffs',
                                                         'Ferry %' = 'last_place',
                                                         'Simulated Wins' = 'mean_wins',
                                                         'Simulated Points' = 'mean_pts',
                                                         'Points/Matchup' = 'adj_pts',
                                                         'Points/Matchup (Batting)' = 'adj_batting_pts',
                                                         'Points/Matchup (SP)' = 'adj_sp_pts',
                                                         'Points/Matchup (RP)' = 'adj_rp_pts'
                                                         
                                          )),
                              
                              checkboxInput(inputId = 'stat_desc',
                                            label = 'Sort Descending',
                                            value = T,
                                            width = NULL),
                              gt_output('stats_table'),
                              br(),
                              dateRangeInput(inputId = 'pitch_dates', 
                                             label = 'Pitch Table Date Range',
                                             start = params$opening_day,
                                             min = params$opening_day,
                                             end = Sys.Date(),
                                             max = Sys.Date()),
                              
                              selectInput(inputId = 'pitch_sort',
                                          label = 'Sort Pitching Table',
                                          choices = list('ERA' = 'era',
                                                         'FIP' = 'fip',
                                                         'K/9' = 'k9',
                                                         'BB/9' = 'bb9',
                                                         'K/BB' = 'k_per_bb',
                                                         'HR/9' = 'hr9',
                                                         
                                                         'ERA (SP)' = 'era_sp',
                                                         'FIP (SP)' = 'fip_sp',
                                                         'K/9 (SP)' = 'k9_sp',
                                                         'BB/9 (SP)' = 'bb9_sp',
                                                         'K/BB (SP)' = 'k_per_bb_sp',
                                                         'HR/9 (SP)' = 'hr9_sp',
                                                         
                                                         
                                                         'ERA (RP)' = 'era_rp',
                                                         'FIP (RP)' = 'fip_rp',
                                                         'K/9 (RP)' = 'k9_rp',
                                                         'BB/9 (RP)' = 'bb9_rp',
                                                         'K/BB (RP)' = 'k_per_bb_rp',
                                                         'HR/9 (RP)' = 'hr9_rp',
                                                         
                                                         'QS' = 'qs',
                                                         'Blue Balls' = 'blue_balls'
                                          )),
                              
                              checkboxInput(inputId = 'pitch_desc',
                                            label = 'Sort Descending',
                                            value = F,
                                            width = NULL),
                              gt_output('pitch_table'),
                              br(),
                              dateRangeInput(inputId = 'bat_dates', 
                                             label = 'Batting Table Date Range',
                                             start = params$opening_day,
                                             min = params$opening_day,
                                             end = Sys.Date(),
                                             max = Sys.Date()),
                              
                              selectInput(inputId = 'bat_sort',
                                          label = 'Sort Batting Table',
                                          choices = list('OPS' = 'ops', 
                                                         'AVG' = 'avg', 
                                                         'OBP' = 'obp',  
                                                         'SLG' = 'slg',
                                                         'wOBA' = 'woba',
                                                         'BABIP' = 'babip',
                                                         'K %' = 'k_rate',
                                                         'BB %' = 'bb_rate')),
                              
                              checkboxInput(inputId = 'bat_desc',
                                            label = 'Sort Descending',
                                            value = T,
                                            width = NULL),
                              
                              gt_output('bat_table'),
                              br(),
                              plotOutput('sp_matrix', width = '90%', height = '850px')
                            )
                            
                   ),
                   
                   tabPanel("Top Performers",
                            value = 'top_performers',
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("matchup_id", 
                                            label = "Select Matchup",
                                            choices = (params$current_matchup:1), 
                                            selected = params$current_matchup
                                            
                                )
                              ),
                              
                              
                              ### Render Table
                              mainPanel(
                                width = 12,
                                gt_output('top_performers'),
                                br(),
                                br(),
                                gt_output('best_lineup')
                              )
                              
                            )
                   ),
                   
                   tabPanel("All-Stars",
                            value = 'all_stars',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              gt_output('asg_lineup'),
                              br(),
                              br(),
                              gt_output('asg_counts')
                            )
                            
                            
                   ),
                   
                   tabPanel("Win Probability Charts",
                            value = 'win_prob',
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("matchup_id_wp", 
                                            label = "Select Matchup",
                                            choices = (params$current_matchup:1), 
                                            selected = params$current_matchup
                                            
                                )
                              ),
                              
                              
                              ### Render Table
                              mainPanel(
                                width = 12,
                                plotOutput('wp_plot',  width = "100%", height = '1000px')
                              )
                              
                            )
                   ),
                   
                   
                   tabPanel("Playoffs",
                            value = 'playoffs',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              plotOutput('win_dist',  width = "60%", height = '600px'),
                              plotOutput('points_dist',  width = "60%", height = '600px'),
                              plotOutput('playoff_history',  width = "100%", height = '600px'),
                              plotOutput('ferry_history',  width = "100%", height = '600px'),
                            )      
                            
                   ),
                   
                   tabPanel("Points Over Time Trends",
                            value = 'trends',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              plotOutput('bump',  width = "100%", height = '600px'),
                              plotOutput('ppw_1',  width = "100%", height = '600px'),
                              plotOutput('ppw_2',  width = "100%", height = '600px'),
                              plotOutput('ppw_3',  width = "100%", height = '600px')
                            )      
                   ),
                   
                   tabPanel("Rolling Averages",
                            value = 'rolling',
                            
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("k",
                                            label = "Select # of Days Rolling Average",
                                            min = min(period, 5),
                                            max = min(period - 3, 50),
                                            step = 5,
                                            value = min(period - 3, 10)
                                            
                                )
                              ),
                              
                              mainPanel(
                                width = 12,
                                plotOutput('roll_k', width = '100%', height = '1000px')
                              )
                              
                            )
                   ),
                   
                   tabPanel('Trade/Free Agent Analysis',
                            mainPanel(
                              width = 12,
                              gt_output('trade_chart'),
                              gt_output('fa_chart'),
                              gt_output('fs_chart')
                            )
                   ),
                   
                   tabPanel('Penalties',
                            mainPanel(
                              width = 12,
                              gt_output('sp_pen'),
                              gt_output('rp_pen')
                            )
                   )
                   
                   
                   
))
