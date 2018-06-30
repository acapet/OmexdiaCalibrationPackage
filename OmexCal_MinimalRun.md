OmexdiaCalibration Toolbox: Minimal Example
================
Arthur Capet
Nov, 2017

-   [Example of use](#example-of-use)
-   [User Data](#user-data)
-   [Modal-Data metrics](#modal-data-metrics)
-   [Display](#display)
-   [Calibration](#calibration)

This example script runs and display a first simulation of OMEXDIA (local version). It then provides an example of how to load data, compute model misfits for specific variables, and display the comparison with model outputs.

``` r
# Loading OmexCal functions and the Omexdia model dynamic library
source("Utils/OmexCal_Load.R")
```

Example of use
==============

In this example we consider steady-state model simulations. Since the model is deterministic, one set of value for parameters (which includes boundary conditions), provides a unique model solution. The global variable `parSta` has to be defined, since it is used inside auxiliary functions. It contains the full parameter list given in [Utils/OmexCal\_BasicSetup.R](Utils/OmexCal_BasicSetup.R). Later on, during calibration procedure, the values given in [Utils/OmexCal\_BasicSetup.R](Utils/OmexCal_BasicSetup.R) will be used as starting points for the calibration of individual parameters.

``` r
# local copy of the global parameter vector
parSta<-pars

# OCALL gets the model solution for parameters given in argument
DIA <-OCALL(parSta)

# Display is called directly with parameters value (the model solution is computed internally)
Simplot(pars)
```

![](Figs/Model%20Run-1.png)

User Data
=========

User data are be stored in a .xls file, respecting the [user data file structure](https://github.com/MAST-ULiege/OmexdiaCalibrationPackage/wiki/Data-Preparation). User-specific options (eg. filepaths, etc ..) are to be given in a [user file](https://github.com/MAST-ULiege/OmexdiaCalibrationPackage/wiki/User-Definitions-File), just like the example [UsersDefinitions\_HAMMOND.R](UsersDefinitions_HAMMOND.R). The script [OmexCal\_Load\_Data.R](Utils/OmexCal_Load_Data.R) interprets the data, following some informations providing in the user file. By default, when loading the data, plot and maps are generated in a dedicated directory (the map generation may take some time, tho disable this option switch off the `mapping` flag in the user file).

``` r
# The Hammond dataset is used as example and is provided in the package. 
# Hammond, D. E., et al. "Diagenesis of carbon and nutrients and benthic exchange in sediments of the Northern Adriatic Sea." Marine Chemistry 66.1-2 (1999): 53-79.
casedir<-'HAMMOND'

source(paste0('Cases/',casedir,'/','UsersDefinitions.R'))

## To test your own data, create a file "UsersDefinitions_OwnData.R" on the basis of UsersDefinitions_HAMMOND.R, and uncomment the following lines
#source('UsersDefinitions_OwnData.R')
#sta<-"Station_example"
#cam<-"Campaign_example"

# This loads data the based on info given in the UserDefinitions....R
# The default behavior is to generate plots of flux and profile data.
source('Utils/OmexCal_Load_Data.R')

source('Utils/OmexCal_PlotCase.R')

# We then create "local" dataframes, specific to one station in one campaign.
sta<-"H2" 
cam<-"Sep89"
localdata    <- subset(dfProfiles, Station==sta & Campaign == cam)
localdatafl  <- subset(dfFluxes,   Station==sta & Campaign == cam)
localdatasta <- subset(dfStations, Station==sta & Campaign == cam)
```

Some parameters are general, some have to be adapted for each station/campaign. This is the case, for instance, for the porosity grid and bottom water concentration for nutrients.

``` r
# In addition, some global parameters have to be given a local (station+campaign) value
parSta    <- OmexCal_AdaptForSta()

ggplot(localdata,
       aes(x=value,y=UpperDepth/2+LowerDepth/2,
             ymax=UpperDepth,ymin=LowerDepth,
             xmin=value-err, xmax=value+err))+
  geom_errorbar()+
  geom_errorbarh()+
  geom_point(size=2)+
  facet_wrap(~variable,scales = "free")+ylim(c(30,0))
```

![](Figs/unnamed-chunk-2-1.png)

Modal-Data metrics
==================

Once data are loaded, the generic cost function can be used while specifying which data should be used to asess the model skills.

``` r
#  Cost function can be called with a list of profile variables and a list of flux variables
 C1 <- OCOST_GEN(pars,Vlist = "NH3")
kable( as.data.frame(C1$var))
```

| name |      scale|    N|  SSR.unweighted|  SSR.unscaled|       SSR|
|:-----|----------:|----:|---------------:|-------------:|---------:|
| NH3  |  0.1428571|    7|        6820.193|      1119.105|  22.83888|

``` r
 C2 <- OCOST_GEN(parSta,Vlist = "NH3")
 kable( as.data.frame(C2$var))
```

| name |      scale|    N|  SSR.unweighted|  SSR.unscaled|       SSR|
|:-----|----------:|----:|---------------:|-------------:|---------:|
| NH3  |  0.1428571|    7|        4714.522|      710.8764|  14.50768|

``` r
 C3 <- OCOST_GEN(parSta,Vlist = c("NOx","PO4","NH3"))
 kable( as.data.frame(C3$var))
```

| name |      scale|    N|  SSR.unweighted|  SSR.unscaled|       SSR|
|:-----|----------:|----:|---------------:|-------------:|---------:|
| NH3  |  0.1428571|    7|      4714.52192|      710.8764|  14.50768|
| PO4  |  0.1428571|    7|        56.01756|     2425.1543|  49.49295|

``` r
 C4 <- OCOST_GEN(parSta,Vlist = c("NH3","DIC"), Flist = c("SIO","NH3","NOx"))
 kable( as.data.frame(C4$var))
```

| name    |      scale|    N|  SSR.unweighted|  SSR.unscaled|         SSR|
|:--------|----------:|----:|---------------:|-------------:|-----------:|
| SIOflux |  1.0000000|    1|    4.810232e+00|    13.3617544|  13.3617544|
| NH3flux |  1.0000000|    1|    9.310000e-05|     0.0258595|   0.0258595|
| NOxflux |  1.0000000|    1|    1.082130e-02|     3.0059105|   3.0059105|
| DIC     |  0.1666667|    6|    1.301674e+06|   103.3260453|   2.8701679|
| NH3     |  0.1428571|    7|    4.714522e+03|   710.8764097|  14.5076818|

Display
=======

The toolbox then contains a number of functions to display model outputs and useful summary tables.

``` r
# Some result display scripts, first one by one : 
Simplot(pars,plotdata=TRUE)+        # The flag TRUE is used to display the data along model outputs
  ggtitle(paste(sta,"0. No Fit"))
```

![](Figs/unnamed-chunk-4-1.png)

``` r
kable(partableplot(pars)$df )
```

|            | guess | Sta. | Cal.      | printunit  |
|------------|:------|:-----|:----------|:-----------|
| Temp       |       |      | 14        | °C         |
| Sal        |       |      | 36        | psu        |
| portop     |       |      | 0.65      | W. Cont.   |
| porbot     |       |      | 0.6       | W. Cont.   |
| pora       |       |      | 0.5       |            |
| biot       |       |      | 10        | cm2/yr     |
| mixL       |       |      | 15        | cm         |
| AlphIrr    |       |      | 0.24932   | cm2/d      |
| IrrEnh     |       |      | 1         | -          |
| w          |       |      | 0.16      | cm/yr      |
| MeanFlux   |       |      | 30        | mmol/m2/d  |
| rFast      |       |      | 12        | /yr        |
| rSlow      |       |      | 0.1       | /yr        |
| pFast      |       |      | 0.27      | -          |
| pRef       |       |      | 0.018     | -          |
| NCrFdet    |       |      | 0.15094   | molN/molC  |
| NCrSdet    |       |      | 0.04      | molN/molC  |
| NCrref     |       |      | 0.04      | molN/molC  |
| rSi        |       |      | 0.02      | /yr        |
| SiCdet     |       |      | 0.06667   | -          |
| EquilSiO   |       |      | 400       | mmol/m3    |
| PCrFdet    |       |      | 0.00943   | molP/molC  |
| PCrSdet    |       |      | 0.00125   | molP/molC  |
| rFePdesorp |       |      | 105       | /yr        |
| rFePadsorp |       |      | 77        | /yr        |
| rCaPprod   |       |      | 0.001     | /yr        |
| rCaPdiss   |       |      | 0         | /yr        |
| CPrCaP     |       |      | 104.73913 | /yr        |
| PO4ads     |       |      | 2         |            |
| Q          |       |      | 2         | -          |
| pdepo      |       |      | 0.3       | -          |
| NH3Ads     |       |      | 1.3       | -          |
| rnit       |       |      | 20        | /d         |
| ksO2nitri  |       |      | 10        | umolO2/m3  |
| rODUox     |       |      | 50        | /d         |
| ksO2oduox  |       |      | 1         | mmolO2/m3  |
| ksO2oxic   |       |      | 3         | mmolO2/m3  |
| ksNOxdenit |       |      | 30        | mmolNOx/m3 |
| kinO2denit |       |      | 10        | mmolO2/m3  |
| kinNOxanox |       |      | 10        | mmolNOx/m3 |
| kinO2anox  |       |      | 8         | mmolO2/m3  |
| bwO2       |       |      | 300       | mmol/m3    |
| bwNH3      |       |      | 2         | mmol/m3    |
| bwNOx      |       |      | 2         | mmol/m3    |
| bwODU      |       |      | 0         | mmol/m3    |
| bwDIC      |       |      | 2800      | mmol/m3    |
| bwSIO      |       |      | 15        | mmol/m3    |
| bwPO4      |       |      | 0.5       | mmol/m3    |
| DispO2     |       |      | 1.46948   | cm2/d      |
| DispNOx    |       |      | 1.20022   | cm2/d      |
| DispNH3    |       |      | 1.36727   | cm2/d      |
| DispODU    |       |      | 0.82689   | cm2/d      |
| DispDIC    |       |      | 0.81661   | cm2/d      |
| DispSIO    |       |      |           | cm2/d      |
| DispPO4    |       |      |           | cm2/d      |
| Flux       |       |      |           |            |

``` r
kable(fluxtable(pars)$d)
```

| variable    |        value|
|:------------|------------:|
| DICflux     |   29.4337830|
| O2flux      |  -18.6138891|
| NOxflux     |    0.2976869|
| NH3flux     |    1.1911479|
| ODUflux     |    4.3416395|
| SIOflux     |    1.1708900|
| PO4flux     |    0.1017672|
| P\_DenitEff |   27.7211602|
| P\_Oxic     |   12.5202524|
| P\_Denitr   |    2.4228480|
| P\_Anoxic   |   85.0568996|

Calibration
===========

The calibration approach is implemented in [OmexCal\_Calibration.R](OmexCal_Calibration.R).
