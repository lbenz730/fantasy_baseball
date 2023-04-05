library(tidyverse)
library(glue)

### Ferry Logo
ferry <- '<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/06/Spirit_of_America_-_Staten_Island_Ferry.jpg/1280px-Spirit_of_America_-_Staten_Island_Ferry.jpg" style="height:30px;">'

### Parameters
params <- 
  list('season' = 2023,
       'opening_day' = as.Date('2023-03-30'))

period <- as.numeric(Sys.Date() - params$opening_day) + 1

df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season == params$season) 

params$current_matchup <- max(df_start$matchup_id[df_start$start_period <= period])


#### Read in Data Sets
teams <- read_csv(glue('data/stats/{params$season}/teams_{params$season}.csv'))
exp_standings <- read_csv(glue('data/stats/{params$season}/exp_standings.csv'))
sim_results <- 
  read_csv(glue('data/playoff_odds/historical_playoff_odds_{params$season}.csv')) %>% 
  filter(matchup_id == params$current_matchup)

batter_points <- read_csv(glue('data/stats/{params$season}/batting_weekly_{params$season}.csv'))
sp_points <- read_csv(glue('data/stats/{params$season}/sp_weekly_{params$season}.csv'))
rp_points <- read_csv(glue('data/stats/{params$season}/rp_weekly_{params$season}.csv'))

