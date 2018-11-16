#' @title 1f.Comm.Area
#' @description Calculates a count of areas with Commercial catch standard indicator as part of the Abundance Characteristics in our assessment 
#' @param sfa defines the specific SFA for the commercial logs
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of commercial log data called 'shrimp.COMLOG'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom PBSMapping makeGrid
#' @importFrom gdata rename.vars
#' @importFrom devtools install_github
#' @importFrom SpatialHub gridData
#' @importFrom SpatialHub bioMap
#' @importFrom RColorBrewer brewer.pal
#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: October 26, 2018 
require(bio.shrimp)
install_github("BradHubley/SpatialHub")
library(SpatialHub)

##################################### Commercial Logbook Data ########################################## 
#Data Query:
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('ComLogs', oracle.username=oracle.username, oracle.password = oracle.password)

write.csv(shrimp.COMLOG,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpComlog.Data",Sys.Date(),".csv",sep=""), row.names=F)
#shrimp.COMLOG<-read.csv("C:/Users/cassi/Documents/Workfiles/ShrimpComlog.Data2018-10-26.csv",sep=",")

str(shrimp.COMLOG) #50,705 RECORDS
head(shrimp.COMLOG)

#Calculate CPUE by record before filtering:
shrimp.COMLOG$CPUE<-(shrimp.COMLOG$WEIGHT/((trunc(shrimp.COMLOG$FHOURS/100)+((shrimp.COMLOG$FHOURS/100)-trunc(shrimp.COMLOG$FHOURS/100))/0.6)))

#TABLE DATA:
#Number of records per year the commercial log data contains
table(shrimp.COMLOG$YEAR)
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
#1082 1220 1184 1605 2151 1716 1720 1807 2049 1851 2144 1799 1949 2193 2265 2086 1628 2552 2400  
#2012 2013 2014 2015 2016 2017 2018 
#2579 2143 2794 2866 1829 1551 1542 

#Commercial areas are filtered using records where weight and hours are not 0, CV_LAT/CV_LONG are not 0 or NA, where survey records, and SFA 16 records are excluded:
com.select<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE !=4 & CV_LAT!=0.0 & CV_LONG!="NA" & SFA<16) 
str(com.select)#38,072 RECORDS
  
####################################### SpatialHub:
#CPUE Area Map:
str(com.select)#38,072 RECORDS

cpue.events<-select(com.select,YEAR,Y,X,CPUE) #Kilograms/hr
cpue.events<-com.select

cpue.events$EID = 1:nrow(cpue.events)
yrs = sort(unique(cpue.events$YEAR))

gt150=c()
gt250=c()
gt350=c()
gt450=c()
gr150=c()
gr250=c()
gr350=c()
gr450=c()
gr550=c()
### I set it up as a loop but run it one year at a time by changing i=1, i=2, etc. to see how it goes

#Prepare data for aggregation:
xl = range(cpue.events$X,na.rm=T)
yl = range(cpue.events$Y,na.rm=T)

i=1
#yrs = 1993:2018
for(i in 1:length(yrs)){
  
cpueGrids = gridData(na.omit(subset(cpue.events,YEAR==yrs[i],c("EID","X","Y","CPUE"))), lvls= c(0,150,250,350,450,550), FUN=mean, grid.size=1.852,aspr=1,border=NA )
 pdf(paste0("map",yrs[i],".pdf")) 
  bioMap(xlim=xl,ylim=yl,poly.lst=cpueGrids)
  contLegend('bottomleft',lvls = cpueGrids$lvls,Cont.data = cpueGrids, cex=0.70,title="CPUE (Kg/hr)",bg='white')
 dev.off() 
  gt150[i]= sum(cpueGrids$polyData$Z>150)
  gt250[i]= sum(cpueGrids$polyData$Z>250)
  gt350[i]= sum(cpueGrids$polyData$Z>350)
  gt450[i]= sum(cpueGrids$polyData$Z>450)
  
  gr150[i]= sum(cpueGrids$polyData$Z<150)
  gr250[i]= sum(cpueGrids$polyData$Z>150 & cpueGrids$polyData$Z<250)
  gr350[i]= sum(cpueGrids$polyData$Z>250 & cpueGrids$polyData$Z<350)
  gr450[i]= sum(cpueGrids$polyData$Z>350 & cpueGrids$polyData$Z<450)
  gr550[i]= sum(cpueGrids$polyData$Z>450)
}

cpue.event.sum<-data.frame(YEAR=yrs,GT150=gt150,GT250=gt250,GT350=gt350,GT450=gt450,GR150=gr150,GR250=gr250,GR350=gr350,GR450=gr450,GR550=gr550)
######################################### GRAPHICS ################################################
# PLOT Gulf CPUE:
#Indicator Plot:
indicator.data<-cpue.event.sum[,c(1,3)] #ESS indicator value
jpeg(filename="area_catch.jpg")
ry<-quantile(indicator.data$GT250[indicator.data$YEAR>1999&indicator.data$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(indicator.data$GT250[indicator.data$YEAR>1999&indicator.data$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(indicator.data, aes(YEAR,GT250)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =776.0 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 1051.2, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(indicator.data$YEAR), max(indicator.data$YEAR), 3)) +
  scale_y_continuous(name="Catch Area (count)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

#Figure 9 in ResDoc:
spatial.cpue<-melt(cpue.event.sum,id.vars='YEAR')
gtcpue.spat<-subset(spatial.cpue,variable%in%c("GT450", "GT350", "GT250","GT150"))

p1<-ggplot(gtcpue.spat,aes(YEAR,value)) + geom_line(aes(linetype=variable, group=variable),size=1.25) +
  geom_point(aes(shape=variable,group=variable),size=3) + 
  scale_x_continuous(name="Year", breaks= seq(1993,2018,2))  +
  scale_y_continuous(name="Area (no. 1X1 min. units)", breaks=seq(0,1600,200),limits=c(0,1600))  +
  #scale_color_manual(name = "Area with CPUE above value (mt)",values=c("black","black","black","black"))+
  scale_linetype_manual(values=c("dotted","solid","dashed","longdash"),breaks=c("GT150", "GT250", "GT350","GT450"),labels=c(">150",">250",">350",">450")) +
  scale_shape_manual(values=c(0,16,2,1),,breaks=c("GT150", "GT250", "GT350","GT450"),labels=c(">150",">250",">350",">450"))+
  theme(legend.title=element_text(size=10),legend.justification=c(1,1), legend.position=c(1.02,1.02),legend.background = element_rect(fill="transparent"),legend.key.width = unit(2, "cm"))+
  labs(shape="Area with CPUE above value (mt)",linetype="Area with CPUE above value (mt)")

grcpue.spat<-subset(spatial.cpue,variable%in%c("GR550","GR450", "GR350", "GR250","GR150"))

p2<-ggplot(grcpue.spat,aes(YEAR,value)) + geom_line(aes(linetype=variable, group=variable),size=1.25) +
  geom_point(aes(shape=variable,group=variable),size=3) + 
  scale_x_continuous(name="Year", breaks= seq(1993,2018,2))  +
  scale_y_continuous(name="Area (no. 1X1 min. units)", breaks=seq(0,800,100))  +
  #scale_color_manual(name = "Area with CPUE range (mt)",values=c("black","black","black","black"))+
  scale_linetype_manual(values=c("dotted","solid","dashed","longdash","dotdash"),breaks=c("GR150", "GR250", "GR350","GR450","GR550"),labels=c("<150","151-250","251-350","351-450",">450")) +
  scale_shape_manual(values=c(0,16,2,1,4),,breaks=c("GR150", "GR250", "GR350","GR450","GR550"),labels=c("<150","151-250","251-350","351-450",">450"))+
  theme(legend.title=element_text(size=10),legend.justification=c(1,1), legend.position=c(1,1.07),legend.background = element_rect(fill="transparent"),legend.key.width = unit(2, "cm"))+
  labs(shape="Area with CPUE above value (mt)",linetype="Area with CPUE above value (mt)")

require(cowplot)
pdf('ComCatchArea.pdf',paper="letter",width=11,height=11)
plot_grid(p1,p2,
          align = "v", nrow = 2, rel_heights = c(1/2,1/2),rel_widths=c(1,1))
dev.off()