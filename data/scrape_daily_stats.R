library(here)
source(here('data/daily_stats.R'))

### 2020
df_2020 <- map_dfr(1:9, ~get_matchup_stats(.x, season = 2020))
write_csv(df_2020, here('data/stats/2020/daily_stats_2020.csv'))

### 2021 Scrape
df_2021 <- map_dfr(1:22, ~get_matchup_stats(.x, season = 2021))
write_csv(df_2021, here('data/stats/2021/daily_stats_2021.csv'))

### 2022 Scrape
df_2022 <- map_dfr(1:22, ~get_matchup_stats(.x, season = 2022))
write_csv(df_2022, here('data/stats/2022/daily_stats_2022.csv'))

### 2023 Scrape
df_2023 <- map_dfr(1:23, ~get_matchup_stats(.x, season = 2023))
write_csv(df_2023, here('data/stats/2023/daily_stats_2023.csv'))

### 2024 Scrape
df_2024 <- map_dfr(1:6, ~get_matchup_stats(.x, season = 2024))
write_csv(df_2024, here('data/stats/2024/daily_stats_2024.csv'))
