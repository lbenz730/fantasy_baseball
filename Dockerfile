FROM rocker/shiny:4.1.0
RUN install2.r rsconnect renv
WORKDIR /home/millburn_savant
COPY app app
COPY deploy.R deploy.R
CMD Rscript deploy.R
