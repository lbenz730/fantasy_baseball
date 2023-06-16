library(tidyverse)
library(ggridges)

df_daily <- read_csv('data/stats/2023/daily_stats_2023.csv')
teams <- read_csv('data/stats/2023/teams_2023.csv')

df_pcts <- 
  df_daily %>% 
  filter(in_lineup) %>% 
  filter(start) %>% 
  group_by(team_id) %>% 
  reframe('n_points' = 0:35, 
          'pct_starts' = map_dbl(0:35, ~mean(points >= .x))) %>% 
  inner_join(teams)

ggplot(df_pcts, aes(x = n_points, y = pct_starts)) + 
  facet_wrap(~team) +
  geom_line(data = select(df_pcts, -team), aes(group = team_id), alpha = 0.3) + 
  geom_line(aes(group = team_id, col = team), lwd = 1.2)

