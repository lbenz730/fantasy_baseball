library(shiny)
library(DT)
library(gt)
# library(waiter)


# Define UI 
baseball <- 'https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExNzM0OWVkd2dtbGo0dnRyY3Fpemg4cXpkOXkxZWkyOXZvNDAyaTQxeCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9cw/1Aftgizz0ekprKsRHJ/giphy.gif'
shinyUI(navbarPage("Millburnish Fantasy Baseball",
                   id = 'navbar',
                   
                   tabPanel("Stats",
                            value = 'stats',
                            # useWaiter(),
                            ### Render Table
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(gt_output('stats_table'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              shinycssloaders::withSpinner(gt_output('pitch_table'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              shinycssloaders::withSpinner(gt_output('bat_table'),
                                                           image = baseball,
                                                           image.height = 100),
                              # br(),
                              shinycssloaders::withSpinner(plotOutput('start_buckets'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              shinycssloaders::withSpinner(plotOutput('sp_matrix'),
                                                           image = baseball,
                                                           image.height = 100)
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
                                shinycssloaders::withSpinner(plotOutput('wp_plot'),
                                                             image = baseball,
                                                             image.height = 100)
                              )
                              
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
                                shinycssloaders::withSpinner(gt_output('top_performers'),
                                                             image = baseball,
                                                             image.height = 100),
                                br(),
                                br(),
                                shinycssloaders::withSpinner(gt_output('best_lineup'),
                                                             image = baseball,
                                                             image.height = 100)
                              )
                              
                            )
                   ),
                   
                   tabPanel("Season All-Stars",
                            value = 'all_stars',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(gt_output('asg_lineup'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              shinycssloaders::withSpinner(gt_output('asg_counts'),
                                                           image = baseball,
                                                           image.height = 100)
                            )
                            
                            
                   ),
                   
                   tabPanel('Trade/Free Agent Analysis',
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(gt_output('trade_chart'),
                                                           image = baseball,
                                                           image.height = 100),
                              shinycssloaders::withSpinner(gt_output('fa_chart'),
                                                           image = baseball,
                                                           image.height = 100),
                              shinycssloaders::withSpinner(gt_output('fs_chart'),
                                                           image = baseball,
                                                           image.height = 100)
                            )
                   ),
                   
                   tabPanel('Draft Analysis', 
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(plotly::plotlyOutput('draft_plot'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              br(), 
                              br(),
                              
                              shinycssloaders::withSpinner(gt_output('gt_draft'),
                                                           image = baseball,
                                                           image.height = 100)
                            )
                            
                            
                            
                   ),
                   
                   tabPanel("Points Over Time Trends",
                            value = 'trends',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(plotOutput('bump'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              shinycssloaders::withSpinner(plotOutput('ppw_1'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              shinycssloaders::withSpinner(plotOutput('ppw_2'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              shinycssloaders::withSpinner(plotOutput('ppw_3'),
                                                           image = baseball,
                                                           image.height = 100)
                            )      
                   ),
                   
                   tabPanel("Rolling Averages",
                            value = 'rolling',
                            
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("k",
                                            label = "Select # of Days Rolling Average",
                                            min = min(period, 5),
                                            max = min(period - 3 - as.numeric(params$opening_day_chart - params$opening_day), 50),
                                            step = 5,
                                            value = min(period - 3 - as.numeric(params$opening_day_chart - params$opening_day), 10)
                                            
                                )
                              ),
                              
                              mainPanel(
                                width = 12,
                                shinycssloaders::withSpinner(plotOutput('roll_k'),
                                                             image = baseball,
                                                             image.height = 100)
                              )
                              
                            )
                   ),
                   
                   
                   tabPanel("Season Simulations",
                            value = 'season_sims',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              
                              shinycssloaders::withSpinner(plotOutput('playoff_history'),
                                                           image = baseball,
                                                           image.height = 100),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              shinycssloaders::withSpinner(plotOutput('win_dist'),
                                                           image = baseball,
                                                           image.height = 100),
                              
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              
                              shinycssloaders::withSpinner(plotOutput('points_dist'),
                                                           image = baseball,
                                                           image.height = 100)
                            )      
                            
                   ),
                   
                   
                   tabPanel('Penalties',
                            mainPanel(
                              width = 12,
                              shinycssloaders::withSpinner(gt_output('sp_pen'),
                                                           image = baseball,
                                                           image.height = 100),
                              shinycssloaders::withSpinner(gt_output('rp_pen'),
                                                           image = baseball,
                                                           image.height = 100)
                            )
                   ), 
                   
                   
                   
                   
))
