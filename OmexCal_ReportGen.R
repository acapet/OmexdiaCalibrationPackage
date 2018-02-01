ReportGen <- function (userfile,Parlist, Costlist, doctitle,camsta) {
  
  rmarkdown::render("OmexCal_FitReport.Rmd", params = list(
    Parlist   = Parlist  ,
    Costlist  = Costlist , 
    userfile  = userfile , 
    camsta      = camsta
  ))
  
  file.copy(from = paste0(getwd(),"/OmexCal_FitReport.pdf"),
            to = paste0(totdir,"Calib_Report_",camsta,".pdf"), overwrite = T)
}