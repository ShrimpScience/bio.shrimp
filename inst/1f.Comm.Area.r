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

str(shrimp.COMLOG) #50,690 RECORDS
head(shrimp.COMLOG)

#Calculate CPUE by record before filtering:
shrimp.COMLOG$CPUE<-(shrimp.COMLOG$WEIGHT/((trunc(shrimp.COMLOG$FHOURS/100)+((shrimp.COMLOG$FHOURS/100)-trunc(shrimp.COMLOG$FHOURS/100))/0.6)))

#TABLE DATA:
#Number of records per year the commercial log data contains
table(shrimp.COMLOG$YEAR)
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
#1082 1220 1184 1605 2151 1716 1720 1807 2049 1851 2144 1799 1949 2193 2265 2086 1628 2552 2400  
#2012 2013 2014 2015 2016 2017 2018 
#2579 2143 2794 2866 1829 1551 1527 

#Commercial areas are filtered using records where weight and hours are not 0, CV_LAT/CV_LONG are not 0 or NA, where survey records, and SFA 16 records are excluded:
com.select<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE !=4 & CV_LAT!=0.0 & CV_LONG!="NA" & SFA<16) 
str(com.select)#38,057 RECORDS

#######################################Brad's Code:
com.select <- rename(com.select,c('CV_LAT'='Y','CV_LONG'='X'))
xl = range(com.select$X,na.rm=T)
yl = range(com.select$Y,na.rm=T)

data = com.select[,c("YEAR","X","Y","CPUE")]
data$EID = 1:nrow(data)

yrs = sort(unique(data$YEAR))

gt150=c()
gt250=c()
gt350=c()
gt450=c()

### I set it up as a loop but run it one year at a time by changing i=1, i=2, etc. to see how it goes

i=23
for(i in 1:length(yrs)){
  
cpueGrids = gridData(na.omit(subset(data,YEAR==yrs[i],c("EID","X","Y","CPUE"))), lvls= c(0,150,250,350,450), FUN=mean, grid.size=1.852,aspr=1,border=NA )
  
  bioMap(xlim=xl,ylim=yl,poly.lst=cpueGrids)
  contLegend('topright',lvls = cpueGrids$lvls,Cont.data = cpueGrids, title="CPUE")
  
  gt150[i]= sum(cpueGrids$polyData$Z>150)
  gt250[i]= sum(cpueGrids$polyData$Z>250)
  gt350[i]= sum(cpueGrids$polyData$Z>350)
  gt450[i]= sum(cpueGrids$polyData$Z>450)
  
}
##################################################
#Add standard unit area values:
shrimp.area<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/UnitAreaStandards.csv",header=T)

#One minute square aggregation of commercial fishing positions:
ji <- function(xy, origin=c(0,0), cellsize=c(.001,.001)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
JI <- ji(cbind(com.select$CV_LONG, com.select$CV_LAT))
com.select$X <- JI[, 1]
com.select$Y <- JI[, 2]
com.select$Cell <- paste(com.select$X, com.select$Y)

cpue.position<-ddply(com.select,.(YEAR,Cell),summarize,AVG.CPUE=mean(CPUE))
a<-subset(cpue.position,YEAR==2014)


com.select$ID<-paste(round(com.select$CV_LAT,2),round(com.select$CV_LONG,2),sep = ".")
samp.size.cpue<-with(rec.with.pos,tapply(ID,ID,length))			 
ss.cpue<-data.frame(ID=names(samp.size.cpue),N=samp.size.cpue)
POS.CNT<-merge(rec.with.pos,ss.cpue,all=T)
POS.CNT$ID<-paste(round(POS.CNT$CV_LAT,2),round(POS.CNT$CV_LONG,2),sep = ".")
cpue.out<-subset(POS.CNT,YEAR>2013 & YEAR<2018)
cpue.position<-ddply(cpue.out,.(YEAR,ID),summarize,AVG.CPUE=mean(CPUE))

CPUE.T1=which(cpue.position$AVG.CPUE > 150) 
cpue.position[CPUE.T1,"CPUE.TARGET"]=">150"
CPUE.T2=which(cpue.position$AVG.CPUE > 250) 
cpue.position[CPUE.T2,"CPUE.TARGET"]=">250"
CPUE.T3=which(cpue.position$AVG.CPUE > 350) 
cpue.position[CPUE.T3,"CPUE.TARGET"]=">350"
CPUE.T4=which(cpue.position$AVG.CPUE > 450) 
cpue.position[CPUE.T4,"CPUE.TARGET"]=">450"
CPUE.T5=which(cpue.position$AVG.CPUE < 150) 
cpue.position[CPUE.T5,"CPUE.RANGE"]="<150"
CPUE.T6=which(cpue.position$AVG.CPUE > 150 & cpue.position$AVG.CPUE < 251) 
cpue.position[CPUE.T6,"CPUE.RANGE"]="150-250"
CPUE.T7=which(cpue.position$AVG.CPUE > 250 & cpue.position$AVG.CPUE < 251) 
cpue.position[CPUE.T5,"CPUE.RANGE"]="<150"
CPUE.T5=which(cpue.position$AVG.CPUE < 150) 
cpue.position[CPUE.T5,"CPUE.RANGE"]="<150"
CPUE.T5=which(cpue.position$AVG.CPUE < 150) 
cpue.position[CPUE.T5,"CPUE.RANGE"]="<150"



cpue.target<-ddply(cpue.position,.(YEAR,CPUE.TARGET), summarise,COM.AREA=length(CPUE.TARGET))

cpue.250<-aggregate(CPUE~YEAR,data=cpue.out,)
cpue.target<-ddply(cpue.out,.(YEAR), summarise,CPUE250=length(CPUE>250))




com.select$ID<-paste(round(com.select$CV_LAT,4),round(com.select$CV_LONG,4),sep = ".")
rec.with.pos<-subset(com.select,ID!="0.0") #Removes 95 records with no position (1994-91 and 1995-4)
samp.size.cpue<-with(rec.with.pos,tapply(ID,ID,length))			 
ss.cpue<-data.frame(ID=names(samp.size.cpue),N=samp.size.cpue)
POS.CNT<-merge(rec.with.pos,ss.cpue,all=T)			 



cpue.out<-subset(POS.CNT,YEAR>2013 & YEAR<2018)

cpue450<-subset(POS.CNT,CPUE>450 & YEAR==1994)
cpue.target<-ddply(cpue450,.(YEAR), summarise,CPUE450=length(unique(ID)))


