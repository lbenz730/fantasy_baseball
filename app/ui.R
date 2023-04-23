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
                              gt_output('stats_table'),
                              br(),
                              gt_output('pitch_table'),
                              br(),
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
