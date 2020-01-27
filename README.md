<br />
<p align="center">
  </a>
  <h1 align="center">A replication of...</h1>
  <p align="center">
    The goal of this project is to complete a professional, publication-worthy replication of a notable scientific paper.
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
Xiaolu Qian [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-8747-1221) <br />
Jiyu Wang [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-1283-2934)<br />
David Wei [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-4347-5941)<br />

<!-- Contents -->
## Contents

<!-- Data -->
## Data
The original data for this paper can be found on the National Centers for Environmental Information website:
https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C00861. doi:10.7289/V5D21VHZ <br />

The paper provided an easy way to download the data using the [FedData](https://github.com/ropensci/FedData)

<!-- Dependencies -->
## Dependencies

```
devtools
FedData v1.1.0
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

devtools::install_github("bocinsky/PaleoCAR")
library(FedData)
library(PaleoCAR)

pkgTest("raster")
pkgTest("png")
pkgTest("RColorBrewer")
pkgTest("fields")
pkgTest("gdata")
```
