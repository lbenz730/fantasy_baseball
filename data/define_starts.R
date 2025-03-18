library(readr)
library(dplyr)
library(purrr)
library(tidyr)

df_start_25 <-
  tibble('matchup_id' = 1:23) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 5,
                                 matchup_id == 16 ~ 11,
                                 matchup_id > 21 ~ 16,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 13,
                                matchup_id == 16 ~ 14,
                                matchup_id > 21 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 21)


df_start_24 <-
  tibble('matchup_id' = 1:23) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 5,
                                 matchup_id == 16 ~ 11,
                                 matchup_id > 21 ~ 16,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 12,
                                matchup_id == 16 ~ 14,
                                matchup_id > 21 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 21)

df_start_23 <-
  tibble('matchup_id' = 1:23) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 13,
                                 matchup_id == 14 ~ 11,
                                 matchup_id > 21 ~ 16,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 11,
                                matchup_id == 14 ~ 14,
                                matchup_id > 21 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 21)

df_start_21_22 <-
  tibble('matchup_id' = 1:22) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 13,
                                 matchup_id == 14 ~ 13,
                                 matchup_id > 20 ~ 16,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 11,
                                matchup_id == 14 ~ 14,
                                matchup_id > 20 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 20)

df_start_20 <-
  tibble('matchup_id' = 1:9) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 13,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 130,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 7)
df_start_20$duration[1] <- 11
df_start_20$start_period[1] <- 120

df_start_19 <-
  tibble('matchup_id' = 1:23) %>%
  mutate('start_cap' = case_when(matchup_id == 1 ~ 16,
                                 matchup_id == 14 ~ 16,
                                 matchup_id > 21 ~ 16,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 19,
                                matchup_id == 14 ~ 14,
                                matchup_id > 21 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1,
         'playoffs' = matchup_id > 21)

df_start <-
  bind_rows(df_start_25 %>% mutate('season' = 2025),
            df_start_24 %>% mutate('season' = 2024),
            df_start_23 %>% mutate('season' = 2023),
            df_start_21_22 %>% mutate('season' = 2022),
            df_start_21_22 %>% mutate('season' = 2021),
            df_start_20 %>% mutate('season' = 2020),
            df_start_19 %>% mutate('season' = 2019))

write_csv(df_start, 'data/df_start.csv')
