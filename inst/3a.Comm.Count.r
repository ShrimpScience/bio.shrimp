#' @title 3a.Comm.Count
#' @description Calculates an annual mean commercial count per pound indicator as part of the Fishing Effects Characteristics in our assessment 
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
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars
#' @importFrom Rmisc summarySE
#' 
#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: November 6, 2018 
require(bio.shrimp)

################### Commercial Count per Pound ############################## 
#Data Query:
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('ComLogs', oracle.username=oracle.username, oracle.password = oracle.password)

write.csv(shrimp.COMLOG,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpComlog.Data",Sys.Date(),".csv",sep=""), row.names=F)
str(shrimp.COMLOG) #50,690 RECORDS
head(shrimp.COMLOG)

#TABLE DATA:
#Number of records per year the commercial log data contains
table(shrimp.COMLOG$YEAR)
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 
#1082 1220 1184 1605 2151 1716 1720 1807 2049 1851 2144 1799 1949 2193 2265 2086 1628 2552 2400 2579 2143 
#2014 2015 2016 2017 2018 
#2794 2866 1829 1551 1527

#Select Mobile fleet records only, and that have a count/lb recorded:
comlog.dat<-subset(shrimp.COMLOG, BTYPE<4 & SFA<16)
#Select only columns required for calculations:
count.dat<-select(comlog.dat,BCODE,YEAR,SFA,XSET,XCOUNT)
#Filtered data to remove NULL counts in records across years:
filt.dat<-count.dat[!is.na(count.dat[,5]),]

#Calculte summary statistics associated with commercial shrimp count:
ann.com.cnt<- summarySE(filt.dat, measurevar="XCOUNT", groupvars=c("YEAR"))

######################################### GRAPHICS ################################################
#Plot survey Commercial Count:
#Indicator Plot:
jpeg(filename="ComCount.jpg")
ry<-quantile(ann.com.cnt$XCOUNT[ann.com.cnt$YEAR>1999&ann.com.cnt$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ann.com.cnt$XCOUNT[ann.com.cnt$YEAR>1999&ann.com.cnt$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.com.cnt, aes(YEAR,XCOUNT)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "green", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =55.14302 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 58.46715, ymax = Inf), fill = "red", alpha = 0.015) +
  geom_hline(yintercept = ry,colour="dark green") + geom_hline(yintercept = yg,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.com.cnt$YEAR), max(ann.com.cnt$YEAR), 3)) +
  scale_y_continuous(name="Commercial Count per Pound") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

#Plot data with standard error and number of observations:

ggplot(ann.com.cnt, aes(x=YEAR, y=XCOUNT, label=N)) + 
  geom_errorbar(aes(ymin=XCOUNT-se, ymax=XCOUNT+se), width=.1) +
  geom_line() + geom_point()+geom_text(aes(label=N),hjust=0.5, vjust=-3.5, size=2,fontface="bold")
