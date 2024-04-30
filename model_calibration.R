library(tidyverse)
source('figures/wp_graphics.R')
source('helpers.R')

theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 24),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.title = element_text(size = 20),
                  strip.text = element_text(size = 12),
                  strip.text.y = element_text(size = 8),
                  plot.caption = element_text(size = 10),
                  legend.text = element_text(size = 12),
                  legend.position = "bottom"))

df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season > 2019) %>% 
  filter(season <= 2023 | matchup_id <= 5)

df_wp <- 
  map2_dfr(df_start$season, df_start$matchup_id, ~{
    print(c(.x, .y))
    plot_wp(season = .x, all = T, plot = F, week = .y) %>% 
      mutate('season_id' = .x, 'matchup_id' = .y)
  })

df_model <- 
  bind_rows(
    select(df_wp, 'team' = team_home, season_id, matchup_id, day_of_matchup, days_left, start_advantage, score_diff, win_prob),
    select(df_wp, 'team' = team_away, season_id, matchup_id, day_of_matchup, days_left, start_advantage, score_diff, win_prob) %>% 
      mutate('win_prob' = 1-win_prob,
             'start_advantage' = -start_advantage,
             'score_diff' = -score_diff)
  ) %>% 
  group_by(team, season_id, matchup_id) %>% 
  mutate('win' = as.numeric(max(win_prob) == 1)) %>% 
  ungroup()


df_calibration <- 
  df_model %>% 
  mutate('wp_bucket' = plyr::round_any(win_prob, 0.05)) %>% 
  group_by(season_id, wp_bucket) %>% 
  summarise('n' = n(),
            'mean_wp' = mean(win)) %>% 
  mutate('train_set' = ifelse(season_id <= 2023, 'Training Data', 'Out of Sample'))

ggplot(df_calibration, aes(x = wp_bucket, y = mean_wp)) + 
  facet_wrap(~season_id) +
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(aes(size = n, color = train_set)) + 
  # geom_smooth() +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Predicted Win Probability',
       y = 'Observed Win Probability',
       color = 'Model Training Set',
       title = 'Calibration of Win Probability Model',
       subtitle = 'Buckets of 5%',
       size = '# of Predictions')
ggsave('figures/model_calibraition.png', height = 9/1.2, width = 16/1.2)
