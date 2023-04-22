FROM rocker/shiny:4.1.0
RUN install2.r rsconnect renv
WORKDIR /home/millburn_savant
COPY app app
COPY deploy.R deploy.R
COPY renv.lock renv.lock
RUN sudo apt install libssl-dev
RUN R -e 'renv::restore()'
CMD Rscript deploy.R
