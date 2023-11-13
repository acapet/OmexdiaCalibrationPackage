# Design Facies of Macrobenthic Activity
require(ReacTran)
require(marelac)

# exponential function
exp.profile <- function(x, x.0, y.0, y.inf, x.att)
  return(y.inf + (y.0-y.inf)*exp(-pmax(0.,(x-x.0))/x.att))

Grid  <- setup.grid.1D(N = 100, dx.1 = 0.01, L = 50)

dbprof <- function (pars){
DbGrid      <- setup.prop.1D(func = exp.profile, x.0 = pars["mixL"],
                             y.0 = pars["biot"]  , y.inf = 0 , x.att = 1, 
                             grid = Grid)
AlphIrrGrid <- setup.prop.1D(func = exp.profile, x.0 = pars["mixL"],
                             y.0 = pars["AlphIrr"], y.inf = 0, x.att = .1, 
                             grid = Grid)

return(data.frame(Depth=Grid$x.mid ,BioTurbation=DbGrid$mid,BioIrrigation=AlphIrrGrid$mid))
}

irrprof <- function (pars){

return(data.frame(Depth=Grid$x.mid ,BioIrrigation=AlphIrrGrid$mid))
}

require(FME)

case='BLACKSEA'
Campaign='BTX2016'
Nsens = 100

require(plyr)

sds<-ldply(c('6','7','12'),function(Station){
load(paste0(case,'/',Campaign,'/',Station,'/','Fit_MCMC.RData'))
MCMC<-mcsubset(MCMC)
bibi<- sensRange(dbprof,
                 parInput = MCMC$pars[,c('mixL','biot','AlphIrr')],
                 sensvar = c('BioTurbation','BioIrrigation'),
                 num = Nsens)

pdf(file = paste0(case,'/',Campaign,'/',Station,'/','MixProfile.pdf'),
    width=4, height = 4)
plot(summary(bibi),
     xyswap=TRUE, 
     xlab = c('BioDiffusion - [cm2/s]','Biorrigation - [/d]'), 
     ylab = 'Depth - [cm]',
     quant=TRUE, main="")
dev.off()
srd<-as.data.frame(summary(bibi))
srd$variable<-c(rep("BioTurbation - [cm2/d]",Nsens),rep("BioIrrigation - [/d]",Nsens))
srd$Station <- Station
return(srd)
})


srd<-as.data.frame(summary(bibi))
srd$variable<-c(rep("BioTurbation - [cm2/d]",100),rep("BioIrrigation - [/d]",100))
require(ggplot2)
ggplot(sds,aes(x = -x, y=Mean, ymin=q05, ymax=q95, fill=Station))+
  geom_ribbon(alpha=0.5)+
  geom_ribbon(aes(ymin=q25, ymax=q75))+
  coord_flip()+
  facet_grid(~variable)+
  geom_line()+xlab('Depth - [cm]')+ylab('')+scale_fill_brewer(palette = 'Set2')+theme_bw()

