library(tidyverse)
library(xgboost)
library(splitTools)
source('build_training_set.R')

set.seed(212)
df <- build_train_set(2021) 

### Preprocessing Recipe
preprocessing_recipe <- 
  recipe(win ~ ., data = df) %>% 
  step_rm(ends_with('id')) %>% 
  prep()

### CV Folds
### One fold per week
cv_folds <- 
  create_folds(y = df$matchup_id,
               k = 5,
               type = "grouped",
               invert = TRUE)

### Hyper Param Grid
xgb_grid <-
  grid_latin_hypercube(
    mtry(range = c(1, ncol(df))),
    min_n(),
    tree_depth(range = c(5L, 12)),
    dials::learn_rate(range = c(-4, -1), trans = scales::log10_trans()),
    loss_reduction(),
    sample_size = sample_prop(),
    size = 10) %>%
  mutate('mtry' = mtry/ncol(df)) %>%
  rename('eta'= learn_rate,
         'gamma' = loss_reduction,
         'subsample' = sample_size,
         'colsample_bytree' = mtry,
         'max_depth' = tree_depth,
         'min_child_weight' = min_n)


run_cv <- function(param_set) {
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
      min_child_weight = param_set$min_child_weight
      # monotonic_constraints = '(0, 0, 
    )
  
  ### Cross validation
  cv_model <- 
    xgb.cv(
      data = as.matrix(df %>% select(-ends_with('_id'), -win)),
      label = df$win,
      params = params,
      nrounds = 2000,
      folds = cv_folds,
      metrics = list("logloss"),
      early_stopping_rounds = 50,
      verbose = F)
  
  # bundle up the results together for returning
  output <- params
  output$iter <- cv_model$best_iteration
  output$logloss <- cv_model$evaluation_log[output$iter]$test_logloss_mean
  
  result <- bind_rows(output)
  
  return(result)
}

### Train The Model
n <- nrow(xgb_grid)
df_cv <- 
  map_dfr(1:n, ~{
    cat('Combo:', .x, 'of', n, '\n')
    run_cv(dplyr::slice(xgb_grid, .x))
  })


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
       'min_child_weight' = best_params$min_child_weight)


model <- 
  xgboost(params = params,
          data = as.matrix(df %>% select(-ends_with('_id'), -win)),
          label = df$win,
          nrounds = best_params$iter,
          verbose = 2)

### Let's move to try one logistic regression per day of matchup
