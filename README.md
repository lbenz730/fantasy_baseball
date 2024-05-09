# ESPN Fantasy Baseball
A repository of code for scraping data for my ESPN fantasy baseball league in order to aggregate advanced stats in an R Shiny App, fit win probability models, and make graphics.



## Data

* __update_data.R__: Data build
* __deploy.R__: Deploy Shiny app
* __daily_stats.R__: Script w/ functions to scrape daily player statlines
* __scrape_daily_stats.R__: Wrapper to execute scraping of daily player statlines for entire season(s)
* __trades.R__: Script to find trades and parse trades for tracking of player stats before and after trades
* __define_starts.R__: Outlines matchup boundaries and start caps for each matchup (to csv file) 
* __df_start.csv__: Outlines matchup boundaries and start caps for each matchup
* __stats/__: Folder of statistics for each available season with each of the following files:
    * __teams_YYYY.csv__: Information on teams, including name, logo, owner, record
    * __schedule_YYYY.csv__: Schedule and results for season
    * __trades_YYYY.csv__: Date of trade and team accepting the trade
    * __traded_players_YYYY.csv__: Players involved in the trades and direction of their movement in the trade
    * __sp_weekly_YYYY.csv__: Starting pitcher stats aggregated over each matchup
    * __rp_weekly_YYYY.csv__: Relief pitcher stats aggregated over each matchup
    * __batting_weekly_YYYY.csv__: Batting stats aggregated over each matchup
    * __daily_stats_YYYY.csv__: Daily level statistics
* __playoff_odds/__: Folder containing files on historical playoff odds from simulations
    * __historical_playoff_odds.R:__ Old script used to back compute old playoff odds for 2021
    * __historical_playoff_odds_YYYY.csv__


## App
* __server.R__: Server file for shiny app
* __ui.R__: UI file for shiny app
* __global.R__: Load constants/data for shiny app
* __helpers.R__: Helper functions

## Models

* __model_calibration.R__: Model Calibration
* __build_training_set.R__: Function to build historical training set for training win probability model
* __fit_model.R__: Trains XGBoost model 
* __recipe.rds__: Pre-processing recipe object 
* __log_reg.rds__: Logistic regression model object 
* __xgb_winprob__: XGBoost model object
