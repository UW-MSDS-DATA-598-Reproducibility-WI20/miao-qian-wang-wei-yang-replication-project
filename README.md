## Final Project for DATA598A Winter 2020

# This repo is aimed to complete a professional, publication-worthy replication of a notable scientific paper for the course DATA 598A.

# Team members: Sihao Miao, Ruian Yang, Xiaolu Qian, Jiyu Wang, David Wei

devtools
devtools::install_github("bocinsky/FedData")

library(FedData)
pkgTest("maps")
pkgTest("fields")
pkgTest("minpack.lm")
pkgTest("Hmisc")
pkgTest("zoo")
pkgTest("abind")
pkgTest("plyr")
pkgTest("geomapdata")
pkgTest("spatstat")
pkgTest("mgcv")
pkgTest("plotrix")
pkgTest("R.oo")

devtools::install_github("bocinsky/FedData")
devtools::install_github("bocinsky/PaleoCAR")
library(FedData)
library(PaleoCAR)
pkgTest("raster")
pkgTest("png")
pkgTest("RColorBrewer")
pkgTest("fields")
pkgTest("gdata")

