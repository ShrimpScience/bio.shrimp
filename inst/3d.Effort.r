#' @title 3d.Commercial Based Effort
#' @description Calculates ESS portion of Commercial ,fishing effort indicator as part of the Fishing Effects Characteristics in our assessment 
#' @param sfa defines the specific ESS SFAs 
#' @param yrs is the stock years to estimate
#' @param mnts months, defaults to the full year
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
#' @importFrom xlsx write.xlsx

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: July 20, 2018 
require(bio.shrimp)

##################################### Commercial Logbook Data ########################################## 
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

#Calculate EFFORT by record before filtering:
shrimp.COMLOG$EFFORT<-shrimp.COMLOG$FHOURS/1000

# Filter for commercial data only:
com.effort<-subset(shrimp.COMLOG,BTYPE<4)
ess.com.effort<-ddply(com.effort,.(YEAR),summarise,Tot_Effort=sum(EFFORT, na.rm=T))

######################################### GRAPHICS ################################################
#Plot commercial fishing effort:
#Indicator Plot:
jpeg(filename="Com.Effort.jpg")
ry<-quantile(ann.com.effort$Tot_Effort[ann.com.effort$YEAR>1999&ann.com.effort$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ann.com.effort$Tot_Effort[ann.com.effort$YEAR>1999&ann.com.effort$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.com.effort, aes(YEAR,Tot_Effort)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "green", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =705.2553 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 865.056, ymax = Inf), fill = "red", alpha = 0.015) +
  geom_hline(yintercept = ry,colour="dark green") + geom_hline(yintercept = yg,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.com.effort$YEAR), max(ann.com.effort$YEAR), 3)) +
  scale_y_continuous(name="Commercial Effort (Hours)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

