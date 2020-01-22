## Final Project for DATA598A Winter 2020

# This repo is aimed to complete a professional, publication-worthy replication of a notable scientific paper for the course DATA 598A.

# Team members: Sihao Miao, Ruian Yang, Xiaolu Qian, Jiyu Wang, David Wei

# Packages
devtools <br />
devtools::install_github("bocinsky/FedData")

library(FedData)
pkgTest("maps") <br />
pkgTest("fields") <br />
pkgTest("minpack.lm") <br />
pkgTest("Hmisc") <br />
pkgTest("zoo") <br />
pkgTest("abind") <br />
pkgTest("plyr") <br />
pkgTest("geomapdata") <br />
pkgTest("spatstat") <br />
pkgTest("mgcv") <br />
pkgTest("plotrix") <br />
pkgTest("R.oo") <br />

devtools::install_github("bocinsky/PaleoCAR") <br />
library(FedData) <br />
library(PaleoCAR) <br />
pkgTest("raster") <br />
pkgTest("png") <br />
pkgTest("RColorBrewer") <br />
pkgTest("fields") <br />
pkgTest("gdata") <br />

# Data
https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00861

