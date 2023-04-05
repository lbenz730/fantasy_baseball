library(shiny)
library(DT)

# Define UI 
shinyUI(navbarPage("Millburnish Fantasy Baseball",
                   id = 'navbar',
                   tabPanel("Stats",
                            value = 'stats',
                            
                            ### Render Table
                            mainPanel(
                              width = 12,
                              gt_output('stats_table')
                            )
                            
                   ),
                   
                   tabPanel("Top Performers",
                            value = 'top_performers',
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("matchup_id", 
                                            label = "Select Matchup",
                                            choices = c(""), 
                                            selected = NULL
                                            
                                )
                              ),
                              
                              
                              ### Render Table
                              mainPanel(
                                width = 12,
                                gt_output('top_performers')
                              )
                              
                            )
                   )
))
