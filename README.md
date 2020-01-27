<br />
<p align="center">
  </a>
  <h1 align="center">A replication of a simulation study on measuring and correcting bias of under-5 mortality estimation in populations affected by HIV/AIDS. </h1>
  <p align="center">

</p>

<!-- TABLE OF CONTENTS -->
## Table of Contents

* [Contributors](#contributors)
* [Contents](#contents)
* [Data](#data)
* [Dependencies](#dependencies)

<!-- ABOUT THE PROJECT -->
## Contributors
Sihao Miao [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-2242-0906) <br />
Ruian Yang [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0789-2465) <br />
Xiaolu Qian [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-8747-1221) <br />
Jiyu Wang [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-1283-2934)<br />
David Wei [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-4347-5941)<br />

<!-- Contents -->
## Contents
The goal of this project is to complete a professional, publication-worthy replication of a [public health paper](https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-019-7780-3).<br />

The paper claims that indirect approaches can introduce bias to under-5 mortality (U5M) estimation in populations affected by HIV/AIDS and their predictive model enables correction of the bias. It is important to us to replicate this paper because its findings can create a substantial impact on public health, especially regarding policies and programs related to the HIV epidemics. <br />

Reference:.<br />
Quattrochi, John, ​et al.​ “Measuring and Correcting Bias in Indirect Estimates of under-5 Mortality in Populations Affected by HIV/AIDS: a Simulation Study.” ​BMC Public Health​, vol. 19, no. 1, Dec. 2019, doi:10.1186/s12889-019-7780-3.

<!-- Data -->
## Data
The original data for performing the individual-level simulation in this paper can be found in this [GitHub repository](https://github.com/jquattro/hiv-childmort-bias) under the data folder provided by the authors.  <br />

The README.md file in this repository provides well-documented introduction to the directory structure and scripts. Within the 'R' folder, there are six R script files for conducting simulations and regressions as well as creating tables and figures of the paper. The authors also provided options to turn parallel computing off to adapt to different computing environments.

We will be able to obtain the same simulation data by running 001_simulation.R with the provided seed. The generated simulation data will be imported with other input data files provided in the 'results' folder into 004_regression.R to generate the model for bias correction and create figure 4 and 5, which are our target figures to replicate.

<!-- Dependencies -->
## Dependencies

|     Team Member     | OS type              | R version |
| :-----------------: | ------------------------ | --------- |
|    Sihao Miao   |  MacOS Mojave 10.14.6   |  3.6.2 |
|    Ruian Yang   |  MasOS Catalina 10.15.2  |  |
|    Xiaolu Qian   | MacOS Mojave 10.14.6  | 3.4.1 |
|    Jiyu Wang     | Windows 10 Home x64  | 3.6.2 |
| David Wei         |   |   |

Packages necessary for 001_simulation.R. <br />
<table style="width:100%">
  <tr>
    <th>package</th>
    <th>version</th>
  </tr>
  <tr>
    <td>doRedis</td>
    <td>* 1.1.1</td>
  </tr>
  <tr>
    <td>dplyr</td>
    <td>* 0.8.0.1</td>
  </tr>
  <tr>
    <td>forcats</td>
    <td>* 0.3.0 </td>
  </tr>
  <tr>
    <td>foreach</td>
    <td>* 1.4.4</td>
  </tr>
  <tr>
    <td>ggplot2</td>
    <td>* 3.1.0</td>
  </tr>
  <tr>
    <td>iterators</td>
    <td>* 1.0.10</td>
  </tr>
  <tr>
    <td>parallel</td>
    <td>* 3.5.2</td>
  </tr>
  <tr>
    <td>purrr</td>
    <td>* 0.3.0</td>
  </tr>
  <tr>
    <td>readr</td>
    <td>* 1.1.1</td>
  </tr>
  <tr>
    <td>stringr</td>
    <td>* 1.4.0</td>
  </tr>
  <tr>
    <td>tibble</td>
    <td>* 2.0.1</td>
  </tr>
  <tr>
    <td>tidyr</td>
    <td>* 0.8.2</td>
  </tr>
  <tr>
    <td>tidyverse</td>
    <td>* 1.2.1</td>
  </tr>
</table>

Packages necessary for 004_regression.R. <br />
<table style="width:100%">
  <tr>
    <th>package</th>
    <th>version</th>
  </tr>
  <tr>
    <td>bcaboot</td>
    <td>* 0.2.1</td>
  </tr>
  <tr>
    <td>data.table</td>
    <td>* 1.11.0</td>
  </tr>
  <tr>
    <td>pls</td>
    <td>* 2.7.0</td>
  </tr>
  <tr>
    <td>glmnet</td>
    <td>* 2.0.16</td>
  </tr>
  <tr>
    <td>officer</td>
    <td>* 0.3.2</td>
  </tr>
  <tr>
    <td>flextable</td>
    <td>* 0.5.1</td>
  </tr>
  <tr>
    <td>magrittr</td>
    <td>* 1.5</td>
  </tr>
</table>
