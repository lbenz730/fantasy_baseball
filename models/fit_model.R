library(tidyverse)
library(xgboost)
# library(splitTools)
# library(tidymodels)
library(here)
source(here('models/build_training_set.R'))

set.seed(212)
df <- bind_rows(build_train_set(2020, augment = T), 
                build_train_set(2021, augment = T),
                build_train_set(2022, augment = T),
                build_train_set(2023, augment = T),
                build_train_set(2024, augment = T))
                

covariates <- c('score_diff', 
                'days_left',
                
                'score_days_ratio',
                'score_starts_ratio',
                
                'start_advantage',
                'start_advantage_ratio',
                
                'points_per_day_spread',
                # 'pitch_spread_per_start', 
                ''
)

constraints <- c(
  1, 0, 
  -1, -1,
  1, 1,
  1)




### Preprocessing Recipe
preprocessing_recipe <- 
  recipe(win ~ ., data = df) %>% 
  step_rm(ends_with('id')) %>% 
  step_rm(-any_of(covariates)) %>% 
  prep()
# preprocessing_recipe <- butcher::butcher(preprocessing_recipe)
write_rds(recipe, here('models/recipe.rds'))

### CV Folds
cv_folds <- 
  create_folds(y = paste0(df$season, '-', df$matchup_id),
               k = 10,
               type = "grouped",
               invert = TRUE)

### Hyper Param Grid
xgb_grid <-
  grid_latin_hypercube(
    mtry(range = c(1, ncol(df))),
    min_n(),
    tree_depth(range = c(5L, 15)),
    learn_rate(range = c(-4, -1), trans = log10_trans()),
    loss_reduction(),
    sample_size = sample_prop(),
    size = 1000) %>%
  mutate('mtry' = mtry/ncol(df)) %>%
  rename('eta'= learn_rate,
         'gamma' = loss_reduction,
         'subsample' = sample_size,
         'colsample_bytree' = mtry,
         'max_depth' = tree_depth,
         'min_child_weight' = min_n)


run_cv <- function(param_set) {
  df_train <- bake(preprocessing_recipe, df) 
  
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = param_set$eta,
      gamma = param_set$gamma,
      subsample = param_set$subsample,
      colsample_bytree = param_set$colsample_bytree,
      max_depth = param_set$max_depth,
      min_child_weight = param_set$min_child_weight,
      monotone_constraints = constraints
    )
  
  ### Cross validation
  cv_model <- 
    xgb.cv(
      data = as.matrix(df_train),
      label = df$win,
      params = params,
      nrounds = 5000,
      folds = cv_folds,
      metrics = list("logloss"),
      early_stopping_rounds = 50,
      verbose = T)
  
  # bundle up the results together for returning
  output <- params
  output$iter <- cv_model$best_iteration
  output$logloss <- cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  result <- bind_rows(output)
  
  return(result)
}

### Train The Model
# n <- nrow(xgb_grid)
# df_cv <- 
#   map_dfr(1:n, ~{
#     cat('Combo:', .x, 'of', n, '\n')
#     run_cv(dplyr::slice(xgb_grid, .x))
#   })

df_cv <- read_csv('models/xgb_cv_results.csv')

best_params <- 
  df_cv %>% 
  arrange(logloss) %>% 
  dplyr::slice(1)

params <-
  list('booster' = "gbtree",
       'objective' = "binary:logistic",
       'eval_metric' = c("logloss"),
       'eta' = best_params$eta,
       'gamma' = best_params$gamma,
       'subsample' = best_params$subsample,
       'colsample_bytree' = best_params$colsample_bytree,
       'max_depth' = best_params$max_depth,
       'min_child_weight' = best_params$min_child_weight,
       'monotone_constraints' = constraints)




model <- 
  xgboost(params = params,
          data = as.matrix(bake(preprocessing_recipe, df)),
          label = df$win,
          nrounds = best_params$iter,
          verbose = 2)

xgb.save(model, here('models/xgb_winprob'))

importance <- xgboost::xgb.importance(
  feature_names = colnames(bake(preprocessing_recipe, df)),
  model = model
)
xgboost::xgb.ggplot.importance(importance_matrix = importance)


log_reg <- glm(win ~ score_diff*factor(case_when(start_advantage >= 4 ~ '> +3',
                                                 start_advantage <= -4 ~ '< -3',
                                                 start_advantage > 0 ~ paste0('+', start_advantage),
                                                 start_advantage < 0 ~ paste0('-', abs(start_advantage)),
                                                 T ~ '0'), levels = c('< -3', '-3', '-2', '-1', '0', 
                                                                      '+1', '+2', '+3', '> +3')), 
               data = df, family = 'binomial')

write_rds(log_reg, here('models/log_reg.rds'))

prior <- glm(win ~ -1 + points_per_day_spread,  data = df %>% filter(day_of_matchup == 0), family = 'binomial')
write_rds(prior, here('models/prior.rds')) 

# model <- glm(win ~ -1 + score_diff * splines::ns(start_advantage, df = 3), data = df, family = 'binomial', offset = rep(0.5, nrow(df)))

