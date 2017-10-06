#' @title PolySFA_shrimp
#' @description Mapping of shrimp fishing areas
#' @param sfa defines the specific SFA for the survey
#' @return data.frame of georeferenced data called 'shrimp.map'
#' 
#' @importFrom PBSMapping 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom car Anova
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars
#' @importFrom rgdal readOGR
 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @seealso \code{\link{template.function}}, \url{http://www.github.com/Beothuk/bio.template}
#' @export

### MCassistaDaRos
### Start: September 20, 2017 
require(bio.shrimp)
#Commercial data load:
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Comlogs', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.COMLOG)
##DEFINING POLYGONS AND PLOTTING DATA POINTS:
##CREATE A UNIQUE IDENTIFIER FOR EACH RECORD:
ESS.comlog<-shrimp.COMLOG
ESS.comlog$EID<-1:length(ESS.comlog[,1])
str(ESS.comlog)
##CREATE A FILE WITH EVENT(1 to length of file), LAT, LONG, and REC.NUMBER(opt):
comlog.EID<-as.data.frame(cbind(EID=1:length(ESS.comlog[,1]),X=ESS.comlog[,20],Y=ESS.comlog[,19],
                                XSET=ESS.comlog[,4],SFA=ESS.comlog[,11],YEAR=ESS.comlog[,21]))
comlog.EID<-subset(comlog.EID,!is.na(X) & !is.na(Y))
str(comlog.EID)
##READ_IN POLYGON DELIMITER FILE (PID,POS,X,Y,SubID):
Poly13to15.box<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Mapping Files/SFA/Shrimp.Poly.csv")
AllPolySFA.pb <- importShapefile("C:/Users/cassistadarosm/Documents/SHRIMP/Mapping Files/SFA/Shrimp_Poly.shp")
MyPoly<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Mapping Files/SFA/MySFAPoly.csv")
##MATCH DATAFILE POSITIONS WITH SUBAREA:
SFA.grid<-findPolys(comlog.EID,AllPolySFA.pb,maxRow=3e+6)
PolyEID<-merge(ESS.comlog,SFA.grid,by=c("EID"),all.x=T,all.y=T, sort=F)
str(PolyEID)
#write.table(PolyEID, file="Poly id of commercial catch.Sept.2017.txt", row.names=FALSE, col.names=TRUE, sep=" ")

#REDUCE THE PolyEID FILE TO 2 COLUMNS (EID AND PID) THEN MERGE WITH EID IDENTIFIER FROM ORIGINAL DATA FILE
PolyEID.red <- subset(PolyEID, select = -c(2:11,13:19))
raw.SFA.grid<-merge(comlog.EID,PolyEID.red,by="EID")
str(raw.SFA.grid)
#REMOVE LOCATIONS OUTSIDE POLYGONS:
na.sub<-which(is.na(PolyEID$PID))
na<-PolyEID[na.sub,]
length(na.sub) #1,081/47,590 RECORDS - 2.3% of records is located outside polygons....
raw.SFA.grid.nona<-raw.SFA.grid[-na.sub,]
#Plotting
land<-read.table("C:/Users/cassistadarosm/Documents/SHRIMP/Mapping Files/martimesHIGH.ll",header=T)
attr(land,"projection")<-"LL"
plotMap(land,xlim=c(-68.5,-55.0),ylim=c(40.0,48.0),col='bisque3',main="ESS SFA 13-15 Commercial Data")
attr(Poly13to15.pb,"projection")<-"LL"
addPolys(Poly13to15.pb)
attr(Poly13to15,"projection")<-"LL"
addPolys(Poly13to15)
attr(MyPoly,"projection")<-"LL"
addPolys(MyPoly)

label.frame<-PolyEID.red[complete.cases(PolyEID.red),]
label.frame<-rename.vars(label.frame, from="SFA",to="PID")
label.frame<-label.frame[,-8]
label.frame<-rename.vars(label.frame, from="CV_LAT",to="Y")
label.frame<-rename.vars(label.frame, from="CV_LONG",to="X")
label.frame$POS<-1

label.frame$COMPOS<-paste(label.frame$X,label.frame$Y,sep='.')
dim(label.frame)
Poly.id<-subset(label.frame,unique(label.frame$COMPOS))

samp.size<-with(LFA41.size.dat,tapply(SAMPNO,SAMPNO,length))			 
ss.dat<-data.frame(SAMPNO=names(samp.size),N=samp.size)			 




labelData<-data.frame(SFA=c(13,14,15,16,17),label=c("Fosse Louisbourg Hole",
                    "Fosse de Misaine Hole","Fosse de Canso Hole","Southwest Nova",
                    "Inshore"))
labXY<-calcCentroid(label.frame,1)
labelData<-merge(labelData,labXY,all.x=TRUE)
attr(labelData,"projection")<-"LL"
addLabels(labelData,cex=0.6, font=2)

addPoints(raw.SFA.grid,col='red',pch=19)
addPoints(subset(raw.SFA.grid,YEAR==2016),col='green',pch=20)
addPoints(subset(raw.SFA.grid,YEAR==2015),col='purple',pch=16)
addPoints(na.omit(subset(LFA27as.EID,Year==2012)),col='blue',pch=0) # 9 samples were omitted because location is undisclosed

          