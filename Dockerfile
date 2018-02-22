FROM w201rdada/portfolio:fa17

USER root
COPY . /home/rstudio/
RUN chown -R rstudio:rstudio * .* \
  && sudo apt-get update \
  && sudo apt-get install libcgal-dev libglu1-mesa-dev libglu1-mesa-dev -y
USER rstudio

## run any install.R script we find
RUN if [ -f install.R ]; then R --quiet -f install.R; fi
