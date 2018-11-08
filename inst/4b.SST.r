#' @title 4b.Sea Surface Temperatures
#' @description Calculates sea surface tempeartures from satellite data to populate shrimp indicator as part of the Ecosystem Characteristics in our assessment 
#' @param stratum defines the specific STRATUM for the survey data
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of survey data called 'shrimp.survey'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr select
#' @importFrom data.table %like%
#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### Start: November 07, 2018 
require(bio.shrimp)

#Read in 4 text files, and merge into 1 complete file:

SST.1<-read.table("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Temperature Files/2018 SST/pk1_sst_2018.stat", header=T)
SST.2<-read.table("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Temperature Files/2018 SST/pk2_sst_2018.stat", header=T)
SST.3<-read.table("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Temperature Files/2018 SST/pk3_sst_2018.stat", header=T)
SST.4<-read.table("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Temperature Files/2018 SST/pk4_sst_2018.stat", header=T)

all.sst<-rbind(SST.1,SST.2,SST.3,SST.4)
feb.sst<-all.sst[all.sst$date.id %like% "febb", ]
mar.sst<-all.sst[all.sst$date.id %like% "mara", ]
spring.sst<-rbind(feb.sst,mar.sst)
spring.sst$YEAR<-substr(spring.sst$date.id, 1, nchar(spring.sst$date.id)-4) 
spring.sst$YEAR<-as.numeric(as.character(spring.sst$YEAR))
#Select for valid SST measurements only:
spring.SST<-subset(spring.sst,mean_sst>-999)
ess.sst<-ddply(spring.SST,.(YEAR),summarize,MEAN.SST=mean(mean_sst))

######################################### GRAPHICS ################################################
#Plot satellite spring SST:
#Indicator Plot:
jpeg(filename="springSST.jpg")
ry<-quantile(ess.sst$MEAN.SST[ess.sst$YEAR>1999&ess.sst$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess.sst$MEAN.SST[ess.sst$YEAR>1999&ess.sst$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ess.sst, aes(YEAR,MEAN.SST)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "green", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =-0.5536709 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 0.02305538 , ymax = Inf), fill = "red", alpha = 0.015) +
  geom_hline(yintercept = ry,colour="dark green") + geom_hline(yintercept = yg,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ess.sst$YEAR), max(ess.sst$YEAR), 3)) +
  scale_y_continuous(name="Spring SST (°C)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 


