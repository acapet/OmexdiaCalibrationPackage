################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/acapet/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developped by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
# A. Eisele, annika.eisele@hzg.de
################
#
# Description :
# This script loads all the auxiliary functions, and runs + display a first simulation.
# It then provides an example of how to load data, compute model misfits for specific variables, 
# and display the comparison with model ouptuts
#
################

source("OmexCal_Load.R")

############################
#      EXAMPLE OF USE      #
############################

# The variable global parSta is used inside auxiliary functions
# It consists in the full parameter list, adapted for the station 
# Here we just copy the general parameters value as given in "OMEXDIA_OG3_BasicSetup.R"
parSta<-pars

# OCALL gets the model solution for parameters given in argument
DIA <-OCALL(parSta)

# Display can be done directly with parameters value
Simplot(pars)

# .. or with model outputs -> TO UPDATE
# Simplot(DIA)

################
##  USER DATA ##
################

if (TRUE){
  source('UsersDefinitions_HAMMOND.R')
  sta<-"H2"
  cam<-"Sep89"
} else{
  source('UsersDefinitions_NOAH.R')
  sta<-"C"
  cam<-"HE432"
}

# This loads data the based on info given in the UserDefinitions....R
source('OmexCal_Load_Data.R')

# We then create "local" dataframes, specific to one station.
localdata    <- subset(dfProfiles, Station==sta & Campaign == cam)
localdatafl  <- subset(dfFluxes,   Station==sta & Campaign == cam)
localdatasta <- subset(dfStations, Station==sta & Campaign == cam)
localdatamicro <-subset(dfO2micro, Station==sta & Campaign == cam)

# In addition, some global parameters have to be given a local (station+campagin) value
parSta    <- OmexCal_AdaptForSta()

ggplot(localdata,
       aes(x=value,y=UpperDepth/2+LowerDepth/2,
             ymax=UpperDepth,ymin=LowerDepth,
             xmin=value-err, xmax=value+err))+
  geom_errorbar()+
  geom_errorbarh()+
  geom_point(size=2)+
  facet_wrap(~variable,scales = "free")+ylim(c(30,0))

# parSta<-AdaptParsForStation_SS(sta)

#  Cost function can be called with a list of profile variables and a list of flux variables
 C1 <- OCOST_GEN(pars,Vlist = "NH3")
 C1$var

 C2 <- OCOST_GEN(parSta,Vlist = "NH3")
 C2$var
 
 C3 <- OCOST_GEN(parSta,Vlist = c("NOx","PO4","NH3"))
 C3$var
 
 C4 <- OCOST_GEN(parSta,Vlist = c("NH3","DIC"), Flist = c("SIO","NH3","NOx"))
 C4$var
 
# Some result display script, first one by one : 
Simplot(pars,plotdata=TRUE)+        # The flag TRUE is used to disaply the data along model ouputs
  ggtitle(paste(sta,"0. No Fit"))

partableplot(pars)

fluxtable(pars)$p

# Collect all on the same plot
pdf(paste(plotdir,sta,"_Fit0.pdf",sep=""),width=5*(3+1)+2,height=15)
grid.arrange(Simplot(parSta,TRUE)+ggtitle(paste(sta,"1. Pseudo")),
             arrangeGrob(partableplot(parSta)),
             arrangeGrob(fluxtable(parSta)$p,
                         fittableplot(C3),ncol=1,heights=c(6,4)),
             ncol = 3,nrow=1, widths=c(5*3,7,3), heights = c(12))
dev.off()


