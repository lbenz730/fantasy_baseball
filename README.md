# ESPN Fantasy Baseball
A repository of code for scraping data for my ESPN fantasy baseball league in order to aggregate advanced stats, fit win probability models, and make graphics.


## Data

* __daily_stats.R__: Script w/ functions to scrape daily player statlines
* __scrape_daily_stats.R__: Wrapper to execute scraping of daily player statlines for entire season(s)
* __trades.R__: Script to find trades and parse trades for tracking of player stats before and after trades
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


## Figures

* __playoff_odds.png__: Playoff odds by week 
* __stats.png__: Advanced stats table
* __points_bump.png__: Bump chart showing position by points scored over the course of the season 
* __trades.png__: Before/After trade analysis by points above league average at position group
* __top_performers.png__: Top performers by batter/SP/RP for current matchup 
* __wp_graphics.R__: R script for plotting win probability graphics
* __top_performers/__: Folder of archived top performer charts for each week
* __wp_charts/__: Folder of archived win probability charts
* __wp.txt__: Text file of current win prob
* __week_lines.txt__: Text file of pre-matchup win prob
* __stock_charts/__: Folder of stock charts showing 10 day rolling averages for bat/sp/rp PPG as well as QS% 
