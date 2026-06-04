# Code Style

- One space after commas and assignment operators (`<-`)
- Long pipelines/chains start on a new line after the assignment:
  ```r
  result <-
    data %>%
    filter(...) %>%
    mutate(...)
  ```
