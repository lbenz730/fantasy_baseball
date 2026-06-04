### run_cache.R
### Bootstraps the objects that cache_files.R expects from update_data.R,
### then sources cache_files.R to regenerate all cached outputs.

library(tidyverse)
library(glue)
library(lubridate)

source('helpers.R')
source('data/free_agents.R')

params <- list(
  'season'      = 2026,
  'opening_day' = as.Date('2026-03-25'),
  'nsims'       = 10000
)

df_start <-
  read_csv('data/df_start.csv', show_col_types = FALSE) %>%
  filter(season == params$season)

period <- min(max(df_start$end_period), max(1, as.numeric(Sys.Date() - params$opening_day) + 1))
params$matchup_id <- max(df_start$matchup_id[df_start$start_period <= period])

teams    <- read_csv(glue('data/stats/{params$season}/teams_{params$season}.csv'), show_col_types = FALSE)
df_daily <- read_csv(glue('data/stats/{params$season}/daily_stats_{params$season}.csv'), show_col_types = FALSE)

df_penalty <-
  read_csv('data/red_flags/penalties.csv', show_col_types = FALSE) %>%
  mutate('penalty' = as.numeric(penalty))

trans_log <- read_csv(
  glue('data/stats/{params$season}/transaction_log_{params$season}.csv'),
  show_col_types = FALSE
)

df_trades <- read_csv(
  glue('data/stats/{params$season}/trades_{params$season}.csv'),
  show_col_types = FALSE
)

exp_standings <- read_csv(
  glue('data/stats/{params$season}/exp_standings.csv'),
  show_col_types = FALSE
)

source('cache_files.R')
