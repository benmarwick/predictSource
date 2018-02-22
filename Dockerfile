FROM w201rdada/portfolio:fa17

USER root
COPY . /home/rstudio/
RUN chown -R rstudio:rstudio * .*
USER rstudio

## run any install.R script we find
RUN if [ -f install.R ]; then R --quiet -f install.R; fi
