FROM r-base:latest
RUN R -e 'install.packages("renv", version="1.0.7")'
RUN apt-get update && apt-get install -y libssl-dev libxml2-dev libmagick++-dev libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev libv8-dev libgit2-dev
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
