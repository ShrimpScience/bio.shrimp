#' @title 2b.Bellybag
#' @description Calculates a Survey Bellybag catch of yoy indicator as part of the Production Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey data
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of survey data called 'shrimp.survey'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr select

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: September 20, 2018 
require(bio.shrimp)

################### Survey O-Group Catch
################### Survey Belly Bag ############################## 
#Survey Belly Bag data query:
shrimp.db('Juveniles.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Juveniles', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.Juv) #95,639 RECORDS
#write.csv(shrimp.Juv,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpJuv.Data",Sys.Date(),".csv",sep=""), row.names=F)
#shrimp.Juv<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SHRJUV.24092018.csv",header=T)

#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
summary(shrimp.survey)
#shrimp.survey<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SHRSURVEY.24092018.csv",header=T)

#Filter out unsuccessful tows and data collected prior to 1995:
shrimp.surv<-subset(shrimp.survey, SETCODE<3 & YEAR>1994)
str(shrimp.surv) #1418 RECORDS

# To calculate number estimates based on carapace length, a merge between the survey and juvenile data must be done:
survey.dist<-select(shrimp.surv,CRUISE, XSET, SFA, STRATUM, DIST,'SDATE'=FDATE)
Juv.dat<-merge(survey.dist,shrimp.Juv, by=c('CRUISE', 'XSET', 'SFA',"STRATUM"))
Juv.dat$YEAR<-year(Juv.dat$SDATE)
Juv.dat$MONTH<-month(Juv.dat$SDATE)

table(Juv.dat$YEAR)
#2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#6095 6152 6516 5582 5923 5631 5593 6585 5883 5903 5491 4333 5163 4673 4688 4692 4919 

#Calculate length frequency per stratum of survey belly bag catch:
#Adjustment of the rounding function to round up when a CARLEN ends with .5
#round = function(x) trunc(x+0.5)
juv.dat<-ddply(Juv.dat,.(YEAR,STRATUM,CARLEN),summarize,FREQ=sum(XCOUNT*RATIO*(1.25/DIST), na.rm=T))
juv.dat2<-ddply(Juv.dat,.(YEAR,STRATUM),summarize,TOT.SET=(length(unique(XSET))))
ann.juv<-merge(juv.dat,juv.dat2, by=c('YEAR','STRATUM'))

#Add standard unit area values:
shrimp.area<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/UnitAreaStandards.csv",header=T)
#shrimp.area<-read.csv("C:/Users/cassi/Documents/GitHub/UnitAreaStandards.csv",header=T)
BB.area<-select(shrimp.area,'STRATUM','BB_unit_area')

#Add unit area to ann.juv file for calculations:
juv.area<-merge(ann.juv, BB.area, by='STRATUM')
area.freq<-ddply(juv.area,.(YEAR,STRATUM,CARLEN),summarize,AREA.FREQ=FREQ/TOT.SET*BB_unit_area)

#Calculate estimates of belly bag numbers at length by stratum area:
juv.sum<-ddply(area.freq,.(YEAR,CARLEN),summarize,ess.FREQ=sum(AREA.FREQ))

#Subset carapace length between 6 and 10 mm inclusive, indicative of o-group only:
o.group<-subset(juv.sum,CARLEN>5.4 & CARLEN<10.0)
ess.bb<- ddply(o.group,.(YEAR),summarize,ess.value=(sum(ess.FREQ)/1000000))
#> ess.bb
#   YEAR ess.value
#1  2002 959.55861
#2  2003 184.05267
#3  2004 320.40473
#4  2005 179.95833
#5  2006  56.98775
#6  2007 188.69760
#7  2008 466.64833
#8  2009 530.17892
#9  2010 194.28812
#10 2011  85.60169
#11 2012  86.26389
#12 2013  20.10908
#13 2014 786.85887
#14 2015 276.39244
#15 2016 107.85314
#16 2017  82.33739
#17 2018 264.31864

# These o-group values are in ess_2018 spreadsheet

# PLOT BELLY BAG number in Million:
#Indicator Plot:
jpeg(filename="bb_num.jpg")
ry<-quantile(ess.bb$ess.value[ess.bb$YEAR>1999&ess.bb$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess.bb$ess.value[ess.bb$YEAR>1999&ess.bb$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ess.bb, aes(YEAR,ess.value)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =187.0 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 365.7, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ess.bb$YEAR), max(ess.bb$YEAR), 3)) +
  scale_y_continuous(name="O Group 6 to 10 mm (Million)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

