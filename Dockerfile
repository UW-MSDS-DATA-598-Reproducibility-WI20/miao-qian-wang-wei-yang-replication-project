FROM rocker/verse:3.6.2

MAINTAINER awesome team <@uw.edu>

COPY . /compendium

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev mesa-common-dev libcgal-dev libglu1-mesa-dev libglu1-mesa-dev libgfortran-8-dev libx11-dev -y \
  # build this compendium package
  && R -e "devtools::install('/compendium', dep=TRUE)" \
  && R -e "rmarkdown::render('/compendium/analysis/paper.Rmd')"
