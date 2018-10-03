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
#' @importFrom bio.survey Prepare.strata.data
#' @importFrom bio.survey Prepare.strata.file
#' @importFrom bio.survey Stratify
#' @importFrom bio.survey boot.strata
#' @importFrom bio.lobster convert.dd.dddd
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom car Anova
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: September 20, 2018 
require(bio.shrimp)

################### Survey Juvenile Catch
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

#tot.num<-ddply(Juv.dat,.(YEAR,STRATUM,XSET),summarise,TOT.NUM=sum(XCOUNT))
#xcount<-ddply(Juv.dat,.(YEAR,STRATUM,XSET,round(CARLEN)),summarize,XCOUNT=sum(XCOUNT))
#null.ratio<-subset(Juv.dat,is.na(TOTNUM))

#Calculate length frequency per stratum of survey belly bag catch:
#Adjustment of the rounding function to round up when a CARLEN ends with .5
round = function(x) trunc(x+0.5)

juv.dat<-ddply(Juv.dat,.(YEAR,STRATUM,'CL.mm'=round(CARLEN)),summarize,FREQ=sum(XCOUNT*RATIO*(1.25/DIST), na.rm=T))
juv.dat2<-ddply(Juv.dat,.(YEAR,STRATUM),summarize,TOT.SET=(length(unique(XSET))))
ann.juv<-merge(juv.dat,juv.dat2, by=c('YEAR','STRATUM'))

#Add standard unit area values:
shrimp.area<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/UnitAreaStandards.csv",header=T)
#shrimp.area<-read.csv("C:/Users/cassi/Documents/GitHub/UnitAreaStandards.csv",header=T)
BB.area<-select(shrimp.area,'STRATUM','BB_unit_area')

#Add unit area to ann.juv file for calculations:
juv.area<-merge(ann.juv, BB.area, by='STRATUM')

juv.freq<-ddply(juv.area,.(YEAR,STRATUM,CL.mm),summarize,TOT.FREQ=sum(area.FREQ))

#Calculate estimates of belly bag numbers at length by stratum area:
juv.sum<-ddply(juv.freq,.(YEAR,CL.mm),summarize,ess.FREQ=sum(TOT.FREQ))

#Subset carapace length between 6 and 10 mm inclusive:
bb.size<-subset(juv.sum,CL.mm>5 & CL.mm<11)
ess.bb<- ddply(bb.size,.(YEAR),summarize,ess.value=sum(ess.FREQ)/1000000)



juv.area$area.FREQ<-juv.area$FREQ/(juv.area$TOT.SET)*juv.area$BB_unit_area

Juv.check<-subset(Juv.dat,YEAR==2017 & STRATUM==13 & CARLEN>19.4 & CARLEN<20.6)

Juv.check$FREQ.check<-round(Juv.check$CARLEN)
