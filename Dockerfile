FROM rocker/shiny:latest
RUN install2.r rsconnect DT dials dplyr fs furrr ggbump ggimage ggrepel ggridges ggsci glue gt gtExtras here jsonlite knitr lubridate patchwork plyr purrr rmarkdown rsvg scales shiny splitTools stringr tidymodels tidyverse truncnorm xgboost zoo
WORKDIR /home/millburn_savant
COPY app app
COPY deploy.R deploy.R
CMD Rscript deploy.R
