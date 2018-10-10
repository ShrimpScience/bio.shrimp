#' @title 2d.Survey main trawl length-based biomass estimates  
#' @description Calculates survey proportions of catch of primiparous/transitional, multiparous, and male shrimp in preperation for the Mixture Analysis based indicators as part of the Production Characteristics in our assessment 
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
#' @importFrom car Anova
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr select

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### Start: October 09, 2018 
require(bio.shrimp)

################### Shrimp Survey Main Trawl Samples ######################
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
#write.csv(shrimp.survey,paste("I:/Offline Data Files/Shrimp/Survey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#Filter out unsuccessful tows:
shrimp.surv<-subset(shrimp.survey, SETCODE<3)
str(shrimp.surv) #1883 RECORDS

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 
# 61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58 
#2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  60   60   60   60   60   60   60   60   60   60   60   60   60 

#Main Trawl Survey data query:
shrimp.db('Details.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Details', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.DETAILS) #1952 RECORDS
write.csv(shrimp.DETAILS,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpDetails.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.DETAILS)

#Disable the scientific notation in R:
options(scipen = 999)

#Acquire Data from MILLIM View data query:
shrimp.db('MILLIM.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('MILLIM', oracle.username=oracle.username, oracle.password = oracle.password)
str(MILLIM.VIEW) #1,235,065 RECORDS
write.csv(MILLIM.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpMILLIM.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(MILLIM.VIEW)

#Select for survey data only:
millim.survey<-subset(MILLIM.VIEW,YEAR>1994 & GEAR==4) #543,081 RECORDS

#Calculate the percent primiparous/transitional portion of the catch:
round = function(x) trunc(x+0.5)
perc.primtran<-ddply(millim.survey,.(YEAR,'CL_MM'=round(CARLEN/10)),summarize,FREQ.PT=sum(PRIMNET+TRAN)/length(round(CARLEN/10)))

#Calculate the percent multiparous portion of the catch:
perc.multi<-ddply(millim.survey,.(YEAR,'CL_MM'=round(CARLEN/10)),summarize,FREQ.MP=sum(MULT)/length(round(CARLEN/10)))

#Import survey total population length frequency estimates from running 2c.MainTrawl.r
Surv.TotPop<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SurvTotPopCalc.Data2018-10-10.csv",header=T)

#Calculate biomass of Primiparous/Transitional, and Multiparous shrimp using total survey population estimates of the main trawl:

#Multiparous:
head(perc.multi)
head(Surv.TotPop)




