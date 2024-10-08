FROM r-base:latest
ENV WORKSPACE=/__w/Spectran
RUN R -e 'install.packages("renv", version="1.0.7")'
RUN apt-get update && apt-get install -y libssl-dev libxml2-dev libmagick++-dev libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev libv8-dev libgit2-dev && mkdir -p "$WORKSPACE"
COPY renv.lock $WORKSPACE/renv.lock
RUN cd $WORKSPACE && R -e 'renv::restore()'
WORKDIR $WORKSPACE
