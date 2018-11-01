#' @title 2c.MainTrawl
#' @description Calculates a Survey Proportion of main trawl catch at size to populate shrimp indicator as part of the Production Characteristics in our assessment 
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

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### Start: October 05, 2018 
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
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 
#  61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69
#2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  61   58   60   60   60   60   60   60   60   60   60   60   60   60  60

#Main Trawl Survey data query:
shrimp.db('Details.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Details', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.DETAILS) #1,235,063 RECORDS
write.csv(shrimp.DETAILS,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpDetails.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.DETAILS)

#Disable the scientific notation in R:
options(scipen = 999)

# Reproduction of ORACLE view to calculate totals to be applied for caclculations 
main.data<-select(shrimp.DETAILS,BCODE,FDATE,GEAR,SFA,STRATUM,XSET,WTIND,CARLEN,TOTNUM)
totals.dat<-ddply(main.data,.(BCODE,FDATE,GEAR,SFA,STRATUM,XSET),summarize,
                  TOTWT=sum(WTIND/100),AVEWT=mean(WTIND),TOTCT=length(TOTNUM),CTLB=TOTCT                     /((sum((WTIND/100000)*2.204))+0.0000001))

# To calculate number estimates based on carapace length, a merge between the survey and main trawl data must be done:
survey.data<-select(shrimp.surv,'BCODE'=CRUISE, XSET, SFA, STRATUM, WEIGHT,DIST,'SDATE'=FDATE)
Main.dat<-merge(survey.data,shrimp.DETAILS, by=c('BCODE', 'XSET', 'SFA',"STRATUM"))

table(Main.dat$YEAR)
# 1993  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005  2006  2007  2008 
#14801 23110 23432 28338 27269 25197 24581 22355 21935 26458 29663 28144 18303 18141 18271 
# 2009  2010  2011  2012  2013  2014  2015  2016  2017  2018 
#18684 18562 18247 18010 18216 18009 18140 18037 17882 18300 

#Merge together the Totals calculations with the details table:
totmain<-merge(totals.dat,Main.dat, by=c('BCODE', 'FDATE','GEAR','XSET', 'SFA',"STRATUM"))

#Calculate catch for all records based on carapace length:
round = function(x) trunc(x+0.5)
main.dat<-ddply(totmain,.(YEAR,STRATUM,'CL_MM'=round(CARLEN/10)),summarize,FREQ=sum((1/TOTCT)*WEIGHT*(1.25/DIST)*(100000/AVEWT)))

#Tally the number of sets per stratum for biomass calculations:
main.dat2<-ddply(shrimp.surv,.(YEAR,STRATUM),summarize,TOT.SET=(length(unique(XSET))))
ann.mt<-merge(main.dat,main.dat2, by=c('YEAR','STRATUM'))

#Add standard unit area values:
shrimp.area<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/UnitAreaStandards.csv",header=T)
#shrimp.area<-read.csv("C:/Users/cassi/Documents/GitHub/UnitAreaStandards.csv",header=T)
MT.area<-select(shrimp.area,'STRATUM','T_unit_area')

#Add unit area to ann.dat file for calculations:
MainTrawl.area<-merge(ann.mt, MT.area, by='STRATUM')
area.freq<-ddply(MainTrawl.area,.(YEAR,STRATUM,CL_MM),summarize,AREA.FREQ=FREQ/TOT.SET*T_unit_area)
write.csv(area.freq,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SurvStratPopCalc.Data",Sys.Date(),".csv",sep=""), row.names=F)

#Calculate annual estimates of main trawl biomass at length by stratum area:
#This value is used as total population estimates in 2d.MainTrawl.Classes
mt.sum<-ddply(area.freq,.(YEAR,CL_MM),summarize,ess.FREQ=sum(AREA.FREQ,na.rm=T))
write.csv(mt.sum,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SurvMTPopCalc95-18.Data",Sys.Date(),".csv",sep=""), row.names=F)

#Calculate annual biomass values in Millions:
ess.mt<- ddply(mt.sum,.(YEAR),summarize,ess.value=(sum(ess.FREQ, na.rm=T)/1000000))
#> ess.mt
#YEAR ess.value
#1  1995 4959.71390
#2  1996   61.26219
#3  1997 4083.94153
#4  1998 4653.11914
#5  1999 4480.67709
#6  2000 4444.90931
#7  2001 3801.10369
#8  2002 2763.36270
#9  2003 4044.22779
#10 2004 7648.69347
#11 2005 7293.90071
#12 2006 6107.71769
#13 2007 6050.43323
#14 2008 4756.20495
#15 2009 6860.84203
#16 2010 5724.99723
#17 2011 4370.00012
#18 2012 4096.47631
#19 2013 5760.55488
#20 2014 5431.00093
#21 2015 4641.80633
#22 2016 4590.26307
#23 2017 3949.11632
#24 2018 3451.06646

# These main trawl values are used as a total population estimates to identify several indicators based on carapace length noted in the ess_2018 spreadsheet

# PLOT MAIN TRAWL number in Million:
#Indicator Plot:
jpeg(filename="bb_num.jpg")
ry<-quantile(ess.mt$ess.value[ess.mt$YEAR>1999&ess.mt$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess.mt$ess.value[ess.mt$YEAR>1999&ess.mt$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ess.mt, aes(YEAR,ess.value)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =4538.3 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 6084.8, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ess.mt$YEAR), max(ess.mt$YEAR), 3)) +
  scale_y_continuous(name="Main Trawl Catch 7 to 33 mm (Million)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

