#' @title 1c.Trap.CPUE
#' @description Calculates Trap commercial CPUE indicator as part of the Abundance Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of commercial log data called 'shrimp.COMLOG'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom bio.lobster convert.dd.dddd
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom car Anova
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @seealso \code{\link{template.function}}, \url{http://www.github.com/Beothuk/bio.template}
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

#Trap Commercial Landings only:
Trap.comlog<-subset(shrimp.COMLOG, WEIGHT>0 & NTRAPS>0 & BTYPE==4 & SFA<16, na.rm=T)
#ESS Commercial Landings only:
ESS.comlog<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE==1, na.rm=T)

#Calculate annual CPTH (Kg/Trap):
Trap.cpth<-ddply(Trap.comlog,.(YEAR),summarize,Trap_CPTH=mean(WEIGHT/NTRAPS))
> Trap.cpth
#   YEAR Trap_CPTH
#1  1996  2.211866
#2  1997  2.263959
#3  1998  1.691587
#4  1999  2.018077
#5  2000  2.580254
#6  2001  2.944682
#7  2002  2.898451
#8  2003  2.837150
#9  2004  3.421321
#10 2005  2.985844
#11 2006  4.338065
#12 2007  3.603371
#13 2008  4.487500
#14 2009  5.153229
#15 2010  3.236933
#16 2011  3.745630
#17 2012  2.960040
#18 2013  3.847661
#19 2014  3.392664
#20 2015  3.511838
#21 2016  2.714460
#22 2017  2.508065

# These Trap CPTH values are in ess_2017 spreadsheet

######################################### GRAPHICS ################################################
#Plot Trap CPTH:
#Indicator Plot:
jpeg(filename="trap_cpue.jpg")
ry<-quantile(Trap.cpth$Trap_CPTH[Trap.cpth$YEAR>1999&Trap.cpth$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Trap.cpth$Trap_CPTH[Trap.cpth$YEAR>1999&Trap.cpth$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(Trap.cpth, aes(YEAR,Trap_CPTH)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =2.96 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 3.53, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(Trap.cpth$YEAR), max(Trap.cpth$YEAR), 3)) +
  scale_y_continuous(name="Trap cpth (kg/trap)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

#Plot Trap CPTH by month:
str(Trap.comlog)
Trap.mth.cpth<-ddply(Trap.comlog,.(YEAR,MONTH),summarize,Trap_M_CPTH=mean(WEIGHT/NTRAPS))
ggplot(Trap.mth.cpth,aes(MONTH,Trap_M_CPTH)) + geom_line() + geom_point() +
  scale_x_continuous(name="Month",breaks=seq(1, 12, 1)) +
  scale_y_continuous(name="CPTH (kg/trap)", breaks=seq(0,12,2)) + theme_bw() + ggtitle("Monthly Trap CPTH") +
  facet_wrap(~YEAR)

# Plot Trap CPTH by vessel:
Trap.vessel.cpth<-ddply(Trap.comlog,.(YEAR,BCODE),summarize,Trap_V_CPTH=mean(WEIGHT/NTRAPS))
ggplot(Trap.vessel.cpth,aes(x=YEAR,y=Trap_V_CPTH)) + geom_line(aes(color=factor(BCODE))) + 
  geom_point(aes(color=factor(BCODE))) + scale_x_continuous(name="YEAR",breaks=seq(1993, 2017, 3)) +
  scale_y_continuous(name="CPTH (kg/trap)", breaks=seq(0,11,2)) +
  theme_bw() + ggtitle("Vessel Trap CPTH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Table of data:
Ann.Mth.CPTH<-merge(Trap.cpth,Trap.mth.cpth, id.vars="YEAR")
Ann.Mth.Ves.CPTH<-merge(Ann.Mth.CPTH,Trap.vessel.cpth,id.vars="YEAR")

#Plotting against TAC allocation:
#From 1996 to 1999, trap catch was not included in TAC. In 1999, trappers can fish up to 10% of TAC
#until 2005.  From 2005 up until now, trappers catch up to 8% of TAC.
TAC.9618<-c(3170,3600,3800,4800,5300,4700,2700,2700,3300,4608,4608,4820,4912,3475,4900,4432,3954,3496,4140,4140,2990,2392,2392)
Trap.TAC<-c(NA,NA,NA,480.00,530.00,470.00,270.00,270.00,330.00,368.64,368.64,385.60,392.96,278.00,392.00,354.56,316.32,279.68,331.20,331.20,239.20,191.36)
str(Trap.comlog)
#catch conversion to mt:
Trap.com.catch<-ddply(Trap.comlog,.(YEAR),summarize,Ann_Catch_MT=round((sum(WEIGHT,na.rm=T)/1000),0))

ann.TAC<-as.data.frame(cbind(YEAR=unique(Trap.comlog$YEAR),TAC.MT=TAC.9617))
ann.TAC.t<-as.data.frame(cbind(YEAR=unique(ann.TAC$YEAR),TAC.MT=ann.TAC$TAC.MT,Trap.Pro=Trap.TAC))
ann.trap.catch<-merge(ann.TAC.t,Trap.com.catch,by=('YEAR'))

#Tracking Trap TAC allocation across years against their annual landings:
catch.trap<-melt(ann.gulf.catch,id.vars='YEAR')
ggplot(catch.gulf,aes(x=YEAR,y=value)) + geom_bar(data=subset(catch.gulf, variable=="TAC.MT"),
                                                  stat=(position="identity"), fill="light blue") + geom_line(data=subset(catch.gulf, 
                                                                                                                         variable=="Ann_Catch_MT")) + geom_line(data=subset(catch.gulf,variable=="Gulf.Pro"),col="red") +
  scale_x_continuous(name="Year",breaks=seq(1993, 2017, 3)) +
  scale_y_continuous(name="Catch in tonnes (t)", breaks=seq(0,5000,500))



















