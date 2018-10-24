################
#
# This script is part of the OmexdiaCalibration suite (https://github.com/MAST-ULiege/OmexdiaCalibrationPackage) 
# This toolbox exploits essentially codes and methods developed by K. Soetaert (NIOZ)
#
# Arthur Capet (acapet@ulg.ac.be), Oct 2017.
#
################
# Contributors : 
# A. Capet , acapet@ulg.ac.be 
################
#
# Description :
# This functions serves as a transfer function to generate a pdf report for a stepwise calibration procedure
################

ReportGen <- function (userfile,Parlist, Costlist, totdir, cam, sta) {
  
  rmarkdown::render("Utils/OmexCal_FitReport.Rmd", params = list(
    Parlist   = Parlist  ,
    Costlist  = Costlist , 
    userfile  = userfile , 
    cam       = cam      ,
    sta       = sta
  ))
  
  file.copy(from = paste0(getwd(),"/Utils/OmexCal_FitReport.pdf"),
            to = paste0(totdir,"Calib_Report_",cam,"_", sta,".pdf"), overwrite = T)
  
}