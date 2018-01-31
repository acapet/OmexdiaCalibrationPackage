ReportGen <- function (userfile,Parlist, Costlist, doctitle,case) {
  
  rmarkdown::render("OmexCal_FitReport.Rmd", params = list(
    Parlist   = Parlist  ,
    Costlist  = Costlist , 
    userfile  = userfile , 
    case      = case
  ))
  
  
  file.copy(from = "OmexCal_FitReport.pdf",to = paste0(totdir,"Calib_Report_",case,".pdf"))
}