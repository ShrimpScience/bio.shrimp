#' @title 1b.Gulf.CPUE
#' @description Calculates Gulf Commercial CPUE indicator as part of the Abundance Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @return data.frame of commercial log data called 'shrimp.COMLOG'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: December 20, 2017 
require(bio.shrimp)

##################################### Commercial Catches ########################################## 
#Data Query:
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('ComLogs', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.COMLOG) #49,043 RECORDS
head(shrimp.COMLOG)

#TABLE DATA:
#Number of records per year the commercial log data contains
table(shrimp.COMLOG$YEAR)
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
#1082 1220 1184 1605 2151 1716 1720 1807 2049 1851 2144 1799 1949 2193 2265 2086 1628 2552 2400 
#2012 2013 2014 2015 2016 2017
#2579 2143 2794 2866 1829 1431 

#Gulf Commercial Landings only:
Gulf.comlog<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE==3, na.rm=T)
#ESS Commercial Landings only:
ESS.comlog<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE==1, na.rm=T)

#Calculate annual CPUE (Kg/Hr):
#Converts effort from FHOURS to HOURS and DECIMAL HOURS
Gulf.cpue<-ddply(Gulf.comlog,.(YEAR),summarize,GULF_CPUE=mean(WEIGHT)/mean(trunc(FHOURS/100)+((FHOURS/100)-trunc(FHOURS/100))/0.6))
>Gulf.cpue
#   YEAR GULF_CPUE
#1  1993     187.9
#2  1994     213.5
#3  1995     187.0
#4  1996     244.6
#5  1997     236.3
#6  1998     343.7
#7  1999     395.7
#8  2000     383.7
#9  2001     428.2
#10 2002     572.4
#11 2003     675.4
#12 2004     793.1
#13 2005     683.3
#14 2006     716.4
#15 2007     696.6
#16 2008     664.1
#17 2009     648.8
#18 2010     536.2
#19 2011     671.2
#20 2012     520.9
#21 2013     626.7
#22 2014     418.7
#23 2015     571.0
#24 2016     547.8
#25 2017     442.6

# These Gulf CPUE values are in ess_2017 spreadsheet

######################################### GRAPHICS ################################################
# PLOT Gulf CPUE:
#Indicator Plot:
jpeg(filename="gulf_cpue.jpg")
ry<-quantile(Gulf.cpue$GULF_CPUE[Gulf.cpue$YEAR>1999&Gulf.cpue$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Gulf.cpue$GULF_CPUE[Gulf.cpue$YEAR>1999&Gulf.cpue$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(Gulf.cpue, aes(YEAR,GULF_CPUE)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =595.4 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 680.1, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(Gulf.cpue$YEAR), max(Gulf.cpue$YEAR), 3)) +
  scale_y_continuous(name="Gulf cpue (kg/hr)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

#Plot Gulf CPUE by month:
str(Gulf.comlog)
Gulf.mth.cpue<-ddply(Gulf.comlog,.(YEAR,MONTH),summarize,GULF_M_CPUE=mean(WEIGHT)/mean(trunc(FHOURS/100)+((FHOURS/100)-trunc(FHOURS/100))/0.6))
ggplot(Gulf.mth.cpue,aes(MONTH,GULF_M_CPUE)) + geom_line() + geom_point() +
  scale_x_continuous(name="Month",breaks=seq(1, 12, 1)) +
  scale_y_continuous(name="CPUE (kg/hr)", breaks=seq(0,1000,250)) + theme_bw() + ggtitle("Monthly Gulf CPUE") +
  facet_wrap(~YEAR)

# Plot Gulf CPUE by vessel:
Gulf.vessel.cpue<-ddply(Gulf.comlog,.(YEAR,BCODE),summarize,GULF_V_CPUE=mean(WEIGHT)/mean(trunc(FHOURS/100)+((FHOURS/100)-trunc(FHOURS/100))/0.6))
ggplot(Gulf.vessel.cpue,aes(x=YEAR,y=GULF_V_CPUE)) + geom_line(aes(color=factor(BCODE))) + 
  geom_point(aes(color=factor(BCODE))) + scale_x_continuous(name="YEAR",breaks=seq(1993, 2017, 3)) +
  scale_y_continuous(name="CPUE (kg/hr)", breaks=seq(0,1000,200)) +
  theme_bw() + ggtitle("Vessel Gulf CPUE") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Table of data:
Ann.Mth.CPUE<-merge(Gulf.cpue,Gulf.mth.cpue, id.vars="YEAR")
Ann.Mth.Ves.CPUE<-merge(Ann.Mth.CPUE,Gulf.vessel.cpue,id.vars="YEAR")

#Plotting against TAC allocation:
#Gulf can fish up to 25% (1993 to 1998), unknown for a few years, and 22.5% (2005 to now)
#of annual TAC
TAC.9317<-c(2650,3100,3170,3170,3600,3800,4800,5300,4700,2700,2700,3300,4608,4608,4820,4912,3475,4900,4432,3954,3496,4140,4140,2990,2392)
Gulf.TAC<-c(662.50,775.00,792.50,792.50,900.00,950.00,1080.00,1192.50,1057.50,607.50,607.50,742.50,1036.80,1036.80,1084.50,1105.20,781.875,1102.50,997.20,889.65,786.60,931.50,931.50,672.75,538.20)
str(Gulf.comlog)
#catch conversion to mt:
gulf.com.catch<-ddply(Gulf.comlog,.(YEAR),summarize,Ann_Catch_MT=round((sum(WEIGHT,na.rm=T)/1000),0))

ann.TAC<-as.data.frame(cbind(YEAR=unique(Gulf.comlog$YEAR),TAC.MT=TAC.9317))
ann.TAC.g<-as.data.frame(cbind(YEAR=unique(ann.TAC$YEAR),TAC.MT=ann.TAC$TAC.MT,Gulf.Pro=Gulf.TAC))
ann.gulf.catch<-merge(annAC.g,gulf.com.catch,by=('YEAR'))

#Tracking gulf TAC allocation across years against their annual landings:
catch.gulf<-melt(ann.gulf.catch,id.vars='YEAR')
ggplot(catch.gulf,aes(x=YEAR,y=value)) + geom_bar(data=subset(catch.gulf, variable=="TAC.MT"),
  stat=(position="identity"), fill="light blue") + geom_line(data=subset(catch.gulf, 
  variable=="Ann_Catch_MT")) + geom_line(data=subset(catch.gulf,variable=="Gulf.Pro"),col="red") +
  scale_x_continuous(name="Year",breaks=seq(1993, 2017, 3)) +
  scale_y_continuous(name="Catch in tonnes (t)", breaks=seq(0,5000,500))

#Annual catch broken down by month:

catch.percent<-ddply(ann.gulf.catch,.(YEAR), summarize,Perc_Ann_Catch=round((Ann_Catch_MT/TAC.MT)*100,0))
glf.ann.catch<-merge(ann.gulf.catch,catch.percent,by=('YEAR'))
mean(catch.percent$Perc_Ann_Catch)

glf.month.catch<-ddply(Gulf.comlog,.(YEAR,MONTH),summarize,Mth_Catch_MT=round(sum(WEIGHT,na.rm=T)/1000,0))
ann.TAC<-as.data.frame(cbind(YEAR=unique(Gulf.comlog$YEAR),TAC.MT=TAC.9317))
mth.gulf.catch<-merge(ann.TAC,glf.month.catch,by=('YEAR'))
mth.catch.percent<-ddply(mth.gulf.catch,.(YEAR,MONTH), summarize,Perc_Mth_Catch=round((Mth_Catch_MT/TAC.MT)*100,2))
glf.mth.catch<-merge(mth.gulf.catch,mth.catch.percent, by=c('YEAR','MONTH'))
glf.catch<-merge(glf.ann.catch,glf.mth.catch, by= c('YEAR','TAC.MT'))
march.catch<-subset(glf.catch,MONTH==3)

#March % catches only to identify ovigerous fishing in the early spring:
ggplot(glf.ann.catch, aes(x=YEAR,y=Perc_Ann_Catch)) + 
  coord_cartesian(xlim=c(1993,2017),ylim=c(0,100)) + geom_bar(aes(x=YEAR,y=Perc_Ann_Catch),stat=(position="identity")) +
  geom_bar(data=march.catch,aes(x=YEAR,y=Perc_Mth_Catch),stat=(position="identity"), fill="blue") + 
  geom_bar(data=ESS.march.catch,aes(x=YEAR,y=Perc_Mth_Catch),stat=(position="identity"), fill="orange") +
  scale_x_continuous(name="Year",breaks=seq(1993, 2017, 3)) +
  scale_y_continuous(name="Percent Catch (%)", breaks=seq(0,100,10)) + theme_bw() + ggtitle("March Gulf Catches") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Monthly overall:
monthly.catch<-melt(glf.catch,id.vars =c('YEAR','MONTH'))

ggplot(monthly.catch,aes(MONTH,value,group=variable)) + 
  geom_bar(data=subset(monthly.catch, variable=="Mth_Catch_MT"),
  stat=(position="identity"), fill="light blue", color="black") + facet_wrap(~YEAR) +
  scale_x_continuous(name="Month",breaks=seq(1, 12, 1)) +
  scale_y_continuous(name="Catch (t)", breaks=seq(0,1000,250)) + theme_bw() + ggtitle("Monthly Gulf Catches") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Gulf and ESS commercial catch comparison:
#ESS
ESS.com.catch<-ddply(ESS.comlog,.(YEAR),summarize,Ann_Catch_MT=round((sum(WEIGHT,na.rm=T)/1000),0))
ESS.ann.TAC<-as.data.frame(cbind(YEAR=unique(ESS.comlog$YEAR),TAC.MT=TAC.9317))
ann.ESS.catch<-merge(ESS.ann.TAC,ESS.com.catch,by=('YEAR'))
ESS.catch.percent<-ddply(ann.ESS.catch,.(YEAR), summarize,Perc_Ann_Catch=round((Ann_Catch_MT/TAC.MT)*100,0))
ESS.ann.catch<-merge(ann.ESS.catch,ESS.catch.percent,by=('YEAR'))
mean(ESS.catch.percent$Perc_Ann_Catch)

ESS.month.catch<-ddply(ESS.comlog,.(YEAR,MONTH),summarize,Mth_Catch_MT=round(sum(WEIGHT,na.rm=T)/1000,0))
ESS.ann.TAC<-as.data.frame(cbind(YEAR=unique(ESS.comlog$YEAR),TAC.MT=TAC.9317))
mth.ESS.catch<-merge(ESS.ann.TAC,ESS.month.catch,by=('YEAR'))
ESS.mth.catch.percent<-ddply(mth.ESS.catch,.(YEAR,MONTH), summarize,Perc_Mth_Catch=round((Mth_Catch_MT/TAC.MT)*100,2))
ESS.mth.catch<-merge(mth.ESS.catch,ESS.mth.catch.percent, by=c('YEAR','MONTH'))
ESS.catch<-merge(ESS.ann.catch,ESS.mth.catch, by= c('YEAR','TAC.MT'))
ESS.march.catch<-subset(ESS.catch,MONTH==3)
