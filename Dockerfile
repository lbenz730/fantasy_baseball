FROM rocker/shiny:4.2.3
RUN install2.r rsconnect DT dplyr fs furrr ggbump ggimage ggrepel ggridges ggsci glue gt gtExtras here jsonlite knitr lubridate patchwork plyr purrr rmarkdown rsvg scales shiny splitTools stringr recipes readr shinycssloaders truncnorm xgboost zoo devtools forcats plotly
WORKDIR /home/millburn_savant
COPY app app
COPY deploy.R deploy.R
CMD Rscript deploy.R
