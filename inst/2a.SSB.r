#' @title 2a.SSB
#' @description Calculates a Survey Spawning Stock Biomass indicator as part of the Production Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey data
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of survey data called 'shrimp.survey'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
\#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: November 4, 2018 
require(bio.shrimp)

################### Survey Spawning Stock Biomass ############################## 
#Bring in the Survey data from 1a.Survey.CPUE.r after modifications have been applied for years where records were incomplete.  The final object will be: shrimp.surv:
str(shrimp.surv) #1952 RECORDS
summary(shrimp.surv) # Quick check on all range in values are within expected parameters
write.csv(shrimp.survey,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpSurvey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.surv)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.surv$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002  
# 61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60    
#2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  69   61   58   60   60   60   60   60   60   60   60   60   60   60   60   60 

#Select set code 1 and 2 for successful random and fixed survey tows:
shrimp.surv<-subset(shrimp.surv,SETCODE<3) #1,883 RECORDS

#Acquire Data from TOTALS View data query:
shrimp.db('TOTALS.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALS', oracle.username=oracle.username, oracle.password = oracle.password)
str(TOTALS.VIEW) #3,284 RECORDS
write.csv(TOTALS.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpTOTALS.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(TOTALS.VIEW)

# Select records where the total weight is greater than 0:
totals.sel<-subset(TOTALS.VIEW,TOTWT>0) #3,102 RECORDS

#Acquire Data from TOTALSFEMTRAN View data query:
shrimp.db('TOTALSFEMTRAN.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALSFEMTRAN', oracle.username=oracle.username, oracle.password = oracle.password)
str(TOTALSFEMTRAN.VIEW) #2,861 RECORDS
write.csv(TOTALSFEMTRAN.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpTOTALSFEMTRAN.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(TOTALSFEMTRAN.VIEW)

# Select records where the total weight is greater than 0:
totalsfemtran.sel<-subset(TOTALSFEMTRAN.VIEW,TOTWT>0) #2,861 RECORDS

#Create a new dataframe containing columns required for calculation:
survey.dat<-select(shrimp.surv,'BCODE'=CRUISE,YEAR,FDATE,STRATUM,XSET,DIST,WING,WEIGHT) 
totals.dat<-select(totals.sel,BCODE,YEAR,FDATE,STRATUM,XSET,TOTWT)
totalsfemtran.dat<-select(totalsfemtran.sel,BCODE,YEAR,FDATE,STRATUM,XSET,"TOTWTF"=TOTWT)

shrimp.dens<-merge(totals.dat,totalsfemtran.dat,by=c('BCODE',"YEAR",'FDATE','STRATUM','XSET'))
surv.dens<-merge(survey.dat,shrimp.dens,by=c('BCODE','YEAR','FDATE','STRATUM','XSET'))

#Calculate annual survey Spawning Stock Biomass:

surv.mdens<-ddply(surv.dens,.(YEAR,BCODE,STRATUM), summarize,AVG_DENS=mean(WEIGHT*1000/(DIST*1852*WING)*(TOTWTF/TOTWT)))
surv.mdens<-ddply(surv.dens,.(YEAR,BCODE,STRATUM), summarize,AVG_DENS=mean(WEIGHT*1000/(DIST*1852*17.705)*(TOTWTF/TOTWT)))

#Add strata surface area values:
shrimp.s.area<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/Strata.Surface.Area.csv",header=T)
shrimp.s.area<-rename.vars(shrimp.s.area, 'Stratum','STRATUM')
ssb.dat<-merge(surv.mdens,shrimp.s.area,by=c("STRATUM"))
#Calculate ESS SSB vales:
ssb.dat$SSB<-ssb.dat$AVG_DENS*ssb.dat$km2

#Annual ESS Values for SSB:
ann.surv.ssb<-ddply(ssb.dat,.(YEAR),summarise,ess.SSB=sum(SSB))

######################################### GRAPHICS ################################################
#Plot survey SSB:
#Indicator Plot:
jpeg(filename="Survey.SSB.jpg")
ry<-quantile(ann.surv.ssb$ess.SSB[ann.surv.ssb$YEAR>1999&ann.surv.ssb$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ann.surv.ssb$ess.SSB[ann.surv.ssb$YEAR>1999&ann.surv.ssb$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.surv.ssb, aes(YEAR,ess.SSB)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =16147.06 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 18087.92, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = ry,colour="red") + geom_hline(yintercept = yg,colour="dark green") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.surv.ssb$YEAR), max(ann.surv.ssb$YEAR), 3)) +
  scale_y_continuous(name="SSB (mt)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 
