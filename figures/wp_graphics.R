library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(xgboost)
library(glue)
library(here)
source(here('models/build_training_set.R'))

plot_wp <- function(season, week, plot = T, all = F) {
  xgb_model <- xgb.load(here('models/xgb_winprob'))
  log_reg <- read_rds(here('models/log_reg.rds'))
  prior <- read_rds(here('models/prior.rds'))
  preprocessing_recipe <- read_rds(here('models/recipe.rds'))
  df <- 
    build_train_set(season) %>% 
    filter(matchup_id == week) %>% 
    mutate('start_factor' = factor(case_when(start_advantage >= 4 ~ '> +3',
                                             start_advantage <= -4 ~ '< -3',
                                             start_advantage > 0 ~ paste0('+', start_advantage),
                                             start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                             T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                  '+1', '+2', '+3', '> +3')))
  
  
  teams <- 
    read_csv(here(glue('data/stats/{season}/teams_{season}.csv'))) %>% 
    select(team_id, team, logo)
  
  df <- 
    df %>% 
    left_join(teams, by = c('home_team_id' = 'team_id')) %>% 
    left_join(teams, by = c('away_team_id' = 'team_id'), suffix = c('_home', '_away')) 
  
  df_image <- distinct(df, team_home, team_away, logo_home, logo_away)
  
  df$win_prob <- predict(xgb_model, as.matrix(recipes::bake(preprocessing_recipe, df)))
  df$win_prob_lr <- predict(log_reg, newdata = df, type = 'response')
  df$win_prob[df$day_of_matchup == 0] <- predict(prior, newdata = df[df$day_of_matchup == 0,], type = 'response')
  
  df <- 
    df %>% 
    mutate('weight_lr' = case_when(day_of_matchup == 0 ~ 0,
                                   days_left <= 4 & abs(win_prob_lr - 0.5) - abs(win_prob - 0.5) < -0.2 ~ 0.2,
                                   days_left <= 2 & abs(start_advantage) <= 2 ~ 0.25,
                                   T ~ 0.2)) %>% 
    mutate('win_prob' = (1-weight_lr) * win_prob + weight_lr * win_prob_lr)
  
  
  df$win_prob[df$day_of_matchup == 0 & df$matchup_id == 1] <- 0.5
  
  if(!all) {
    df <- filter(df, days_left > min(days_left)) 
  } else if(!(lubridate::wday(Sys.Date()) == 1 & lubridate::hour(Sys.time()) < 20) | all) {
    df$win_prob[df$days_left == 0 & df$score_diff > 0] <- 1
    df$win_prob[df$days_left == 0 & df$score_diff < 0] <- 0
  }
  
  if(plot) {
    p <- 
      ggplot(df, aes(x = day_of_matchup, y = win_prob)) + 
      facet_wrap(~paste(team_home, 'vs.', team_away)) + 
      geom_line() +
      geom_point(aes(fill = start_factor), size = 6, color = 'black', pch = 21) +
      ggimage::geom_image(data = df_image, aes(x = 0.4, y = 0.95, image = logo_home), size = 0.125) + 
      ggimage::geom_image(data = df_image, aes(x = 0.4, y = 0.05, image = logo_away), size = 0.125) +
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
    
    
    print(p)
    
    ggsave(plot = p, here(glue('figures/wp_charts/{season}/wp_chart_{season}_{week}.png')), height = 9, width = 16)
  }
  
  
  return(select(df, 
                team_home, team_away, day_of_matchup, 
                days_left, score_diff, starts_left_home, 
                starts_left_away, start_advantage, win_prob, win_prob_lr))
}
