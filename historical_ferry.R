teams2022 <- read_csv('data/stats/2022/teams_2022.csv')
teams2023 <- read_csv('data/stats/2023/teams_2023.csv')

odds2022 <- 
  read_csv('data/playoff_odds/historical_playoff_odds_2022.csv') %>% 
  mutate('season' = 2022) %>% 
  left_join(teams2022)

odds2023 <- read_csv('data/playoff_odds/historical_playoff_odds_2023.csv') %>% 
  mutate('season' = 2023) %>% 
  left_join(teams2023)

df <- 
  bind_rows(odds2022, odds2023) %>% 
  group_by(team_id) %>% 
  mutate('franchise' = team[season == 2023][1]) %>% 
  ungroup() %>% 
  filter(matchup_id <= 12)

ggplot(df, aes(x = matchup_id, y = last_place)) + 
  facet_wrap(~season) + 
  geom_point(aes(col = franchise)) + 
  geom_line(aes(col = franchise)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 0:12) +
  theme(legend.position = 'bottom') +
  labs(x = 'Week',
       y = 'Ferry Odds',
       col = '',
       title = 'Ferry Odds Over Time')

ggsave('~/Desktop/test.png', height = 9/1.4, width =16/1.4)
