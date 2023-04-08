FROM rocker/shiny:4.0.4
RUN install2.r rsconnect
WORKDIR /home/millburn_savant
COPY app app
COPY deploy.R deploy.R
CMD Rscript deploy.R
