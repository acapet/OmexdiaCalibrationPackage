#' ---
#' title: " OmexdiaCalibration Toolbox: Minimal Example"
#' author: "Arthur Capet"
#' date: "Nov, 2017"
#' output: 
#' #  pdf_document:
#' #    toc: yes
#'   #slidy_presentation:
#'   #  toc: yes
#'   github_document:
#'     toc: yes
#' urlcolor: blue
#' ---
#' 
## ----global_options, include=FALSE---------------------------------------
rm(list=ls()) ### To clear namespace
library(knitr)
opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
               echo=TRUE, warning=FALSE, message=FALSE)

#' 
#' This example script runs and display a first simulation of OMEXDIA (local version).
#' It then provides an example of how to load data, compute model misfits for specific variables, 
#' and display the comparison with model outputs.
#' 
## ------------------------------------------------------------------------
# Loading OmexCal functions and the Omexdia model dynamic library
source("Utils/OmexCal_Load.R")

#' 
#' # Example of use
#' 
#' In this example we consider steady-state model simulations. 
#' Since the model is deterministic, one set of value for parameters (which includes boundary conditions), provides a unique model solution.
#' The global variable `parSta` has to be defined, since it is used inside auxiliary functions.
#' It contains the full parameter list given in [Utils/OmexCal_BasicSetup.R](Utils/OmexCal_BasicSetup.R). 
#' Later on, during calibration procedure, the values given in [Utils/OmexCal_BasicSetup.R](Utils/OmexCal_BasicSetup.R) will be used as starting points for the calibration of individual parameters. 
#' 
## ---- Model Run, warning=FALSE-------------------------------------------
# local copy of the global parameter vector
parSta<-pars

# OCALL gets the model solution for parameters given in argument
DIA <-OCALL(parSta)

# Display is called directly with parameters value (the model solution is computed internally)
Simplot(pars)

#' 
#' #  User Data
#' User data are be stored in a .xls file, respecting the [user data file structure](https://github.com/MAST-ULiege/OmexdiaCalibrationPackage/wiki/Data-Preparation).
#' User-specific options (eg. filepaths, etc ..) are to be given in a [user file](https://github.com/MAST-ULiege/OmexdiaCalibrationPackage/wiki/User-Definitions-File), just like the example [UsersDefinitions_HAMMOND.R](UsersDefinitions_HAMMOND.R).
#' The script [OmexCal_Load_Data.R](Utils/OmexCal_Load_Data.R) interprets the data, following some informations providing in the user file. 
#' By default, when loading the data, plot and maps are generated in a dedicated directory (the map generation may take some time, tho disable this option switch off the `mapping` flag in the user file).
#' 
## ---- Load Data----------------------------------------------------------
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

#' 
#' Some parameters are general, some have to be adapted for each station/campaign.
#' This is the case, for instance, for the porosity grid and bottom water concentration for nutrients.
#' 
## ---- warning=FALSE------------------------------------------------------
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


#' 
#' # Modal-Data metrics
#' 
#' Once data are loaded, the generic cost function can be used while specifying which data should be used to asess the model skills.
## ------------------------------------------------------------------------
#  Cost function can be called with a list of profile variables and a list of flux variables
 C1 <- OCOST_GEN(pars,Vlist = "NH3")
kable( as.data.frame(C1$var))

 C2 <- OCOST_GEN(parSta,Vlist = "NH3")
 kable( as.data.frame(C2$var))
 
 C3 <- OCOST_GEN(parSta,Vlist = c("NOx","PO4","NH3"))
 kable( as.data.frame(C3$var))
 
 C4 <- OCOST_GEN(parSta,Vlist = c("NH3","DIC"), Flist = c("SIO","NH3","NOx"))
 kable( as.data.frame(C4$var))


#' 
#' # Display 
#' 
#' The toolbox then contains a number of functions to display model outputs and useful summary tables.
## ------------------------------------------------------------------------
# Some result display scripts, first one by one : 
Simplot(pars,plotdata=TRUE)+        # The flag TRUE is used to display the data along model outputs
  ggtitle(paste(sta,"0. No Fit"))

kable(partableplot(pars)$df )
kable(fluxtable(pars)$d)


#' 
#' 
#' # Calibration
#' 
#' The calibration approach is implemented in [OmexCal_Calibration.R](OmexCal_Calibration.R).
#' 
