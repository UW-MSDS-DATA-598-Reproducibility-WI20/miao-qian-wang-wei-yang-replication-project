<br />
<p align="center">
  <a href="https://github.com/github_username/repo">
  </a>

  <h3 align="center">A replication of...</h3>

  <p align="center">
    Our goal of this project is to complete a professional, publication-worthy replication of a notable scientific paper.
    <br />
</p>



<!-- TABLE OF CONTENTS -->
## Table of Contents

* [Contributors](#contributors)
* [Contents](#contents)
* [Data](#data)
* [Dependencies](#dependencies)

<!-- ABOUT THE PROJECT -->
## Contributors
Sihao Miao <br />
Ruian Yang [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0789-2465) <br />
Xiaolu Qian <br />
Jiyu Wang <br />
David Wei

<!-- Contents -->
## Contents

<!-- Data -->
## data
https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00861

<!-- Dependencies -->
## Dependencies
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



