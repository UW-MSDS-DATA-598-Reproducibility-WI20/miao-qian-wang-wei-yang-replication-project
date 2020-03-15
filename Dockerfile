<<<<<<< HEAD
FROM rocker/verse:3.6.2

MAINTAINER awesome team <@uw.edu>
=======
# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/geospatial:3.6.1

# required
MAINTAINER Your Name <your_email@somewhere.com>
>>>>>>> origin/master

COPY . /compendium

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
<<<<<<< HEAD
  && sudo apt-get install libudunits2-dev mesa-common-dev libcgal-dev libglu1-mesa-dev libglu1-mesa-dev libgfortran-8-dev libx11-dev -y \
  # build this compendium package
  && R -e "devtools::install('/compendium', dep=TRUE)" \
  && R -e "rmarkdown::render('/compendium/analysis/paper.Rmd')"
=======
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "devtools::install('/compendium', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "devtools::check('/compendium',error_on = 'error')" \
 && R -e "rmarkdown::render('/compendium/analysis/paper.Rmd')"
>>>>>>> origin/master
