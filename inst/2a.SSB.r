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
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
summary(shrimp.survey) # Quick check on all range in values are within expected parameters
write.csv(shrimp.survey,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpSurvey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002  
# 61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60    
#2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  69   61   58   60   60   60   60   60   60   60   60   60   60   60   60   60 

#Acquire Data from TOTALS View data query:
shrimp.db('TOTALS.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALS', oracle.username=oracle.username, oracle.password = oracle.password)
str(TOTALS.VIEW) #3,284 RECORDS
write.csv(TOTALS.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpTOTALS.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(TOTALS.VIEW)

#Acquire Data from TOTALSFEMTRAN View data query:
shrimp.db('TOTALSFEMTRAN.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALSFEMTRAN', oracle.username=oracle.username, oracle.password = oracle.password)
str(TOTALSFEMTRAN.VIEW) #2,861 RECORDS
write.csv(TOTALSFEMTRAN.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpTOTALSFEMTRAN.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(TOTALSFEMTRAN.VIEW)

ssb.qry<-paste("select shrsurvey.cruise,totals.sfa,avg(weight*1000/(dist*1852*17.705)*(totalsfemtran.totwt/totals.totwt))
               from shrsurvey, totals, totalsfemtran 
               where shrsurvey.cruise=totals.bcode
               and shrsurvey.cruise=totalsfemtran.bcode
               and shrsurvey.fdate=totals.fdate
               and shrsurvey.fdate=totalsfemtran.fdate
               and shrsurvey.sfa=totals.sfa
               and shrsurvey.sfa=totalsfemtran.sfa
               and shrsurvey.xset=totals.xset
               and shrsurvey.xset=totalsfemtran.xset
               and totalsfemtran.totwt>0 and totals.totwt>0
               and shrsurvey.setcode in(1,2)
               group by shrsurvey.cruise, totals.sfa
               order by shrsurvey.cruise, totals.sfa")


ssb.dat<-sqlQuery(ch,ssb.qry)
head(ssb.dat)


##### Manual read in - query does not work for me............ Michele Queried using SQL+

ssb.dat <- read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/SSB.csv")
ssb.dat <- read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SSB.2018.csv")

colnames(ssb.dat)<-c("CRUISE", "SFA", "DENSITY")

kms2<-c(1620,1517,948,1415)
dd<-ssb.dat$DENSITY[ssb.dat$CRUISE=='CK1801']

ssb<-sum(kms2*dd)

