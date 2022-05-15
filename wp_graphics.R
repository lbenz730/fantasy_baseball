library(tidyverse)
library(xgboost)
library(tidymodels)
library(lubridate)
library(glue)
library(ggimage)
source('build_training_set.R')

plot_wp <- function(season, week, plot = T, all = F) {
  xgb_model <- xgb.load('xgb_winprob')
  log_reg <- read_rds('log_reg.rds')
  preprocessing_recipe <- read_rds('recipe.rds')
  df <- 
    build_train_set(season) %>% 
    mutate('start_factor' = factor(case_when(start_advantage >= 4 ~ '> +3',
                                             start_advantage <= -4 ~ '< -3',
                                             start_advantage > 0 ~ paste0('+', start_advantage),
                                             start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                             T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                  '+1', '+2', '+3', '> +3')))
  
  
  teams <- 
    read_csv(glue('data/stats/{season}/teams_{season}.csv')) %>% 
    select(team_id, team, logo)
  
  df <- 
    df %>% 
    left_join(teams, by = c('home_team_id' = 'team_id')) %>% 
    left_join(teams, by = c('away_team_id' = 'team_id'), suffix = c('_home', '_away')) %>% 
    filter(matchup_id == week)
  
  df_image <- distinct(df, team_home, team_away, logo_home, logo_away)
  
  df$win_prob <- predict(xgb_model, as.matrix(bake(preprocessing_recipe, df)))
  df$win_prob_lr <- predict(log_reg, newdata = df, type = 'response')
  
  df$win_prob <- 0.67 * df$win_prob + 0.33 * df$win_prob_lr
  
  df$win_prob[df$day_of_matchup == 0 & df$matchup_id == 1] <- 0.5
  
  if(hour(Sys.time()) < 12 & hour(Sys.time()) > 2 & !all) {
    df <- filter(df, days_left > min(days_left)) 
  } else if(!(wday(Sys.Date()) == 1 & hour(Sys.time()) < 20)) {
    df$win_prob[df$days_left == 0 & df$score_diff > 0] <- 1
    df$win_prob[df$days_left == 0 & df$score_diff < 0] <- 0
  }
  
  p <- 
    ggplot(df, aes(x = day_of_matchup, y = win_prob)) + 
    facet_wrap(~paste(team_home, 'vs.', team_away)) + 
    geom_line() +
    geom_point(aes(fill = start_factor), size = 6, color = 'black', pch = 21) +
    geom_image(data = df_image, aes(x = 0.4, y = 0.95, image = logo_home), size = 0.125) + 
    geom_image(data = df_image, aes(x = 0.4, y = 0.05, image = logo_away), size = 0.125) +
    theme_bw() + 
    theme(plot.title = element_text(size = 24, hjust = 0.5),
          axis.title = element_text(size = 16),
          strip.text =  element_text(size = 14),
          plot.subtitle = element_text(size = 18, hjust = 0.5),
          panel.grid.minor.x = element_blank(),
          legend.position = 'bottom')  + 
    labs(x = 'Day of Matchup',
         y = 'Win Probability',
         title = 'Win Probability Charts',
         subtitle = paste('Week:', week),
         fill = 'Start Advantage') +
    scale_y_continuous(limits = c(0,1), labels = function(x){ paste0(100 * pmax(x, 1-x), '%') }) + 
    scale_x_continuous(limits = c(0, max(df$days_left)), breaks = 0:max(df$days_left)) +
    scale_fill_brewer(palette = 'RdYlGn', drop = FALSE) + 
    guides(fill = guide_legend(nrow = 3))
  
  if(plot) {
    print(p)
  }
  
  
  ggsave(glue('figures/wp_charts/{season}/wp_chart_{season}_{week}.png'), height = 9, width = 16)
  
  return(select(df, 
                team_home, team_away, day_of_matchup, 
                days_left, score_diff, starts_left_home, 
                starts_left_away, start_advantage, win_prob, win_prob_lr))
}
