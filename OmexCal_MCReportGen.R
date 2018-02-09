MC_ReportGen <- function (userfile,MCMC, totdir,camsta) {
  
  rmarkdown::render("OmexCal_MCMCReport.Rmd", params = list(
    MCMC = MCMC , 
    userfile  = userfile , 
    camsta      = camsta
  ))
  
  file.copy(from = paste0(getwd(),"/OmexCal_MCMCReport.pdf"),
            to = paste0(totdir,"MCMC_Report_",camsta,".pdf"), overwrite = T)
}