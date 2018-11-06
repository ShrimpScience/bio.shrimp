#' @title 2e.Max.Length
#' @description Calculates an annual mean max length indicator as part of the Production Characteristics in our assessment 
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

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: November 1, 2018 
require(bio.shrimp)

############################### Survey Maximum Shrimp Size ################################ 
#script max_length.sql
#csv file max_length.csv
#setwd()

#Main Trawl Survey data query:
shrimp.db('Details.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Details', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.DETAILS) #1,235,063 RECORDS
write.csv(shrimp.DETAILS,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpDetails.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.DETAILS)

# Select survey data only:
CL.survey<-subset(shrimp.DETAILS, GEAR==4)# 726,420 RECORDS

#Identify maximum carapace length for each set:
max.surv.CL<-ddply(CL.survey,.(YEAR,BCODE,SFA,XSET),summarize, MAX_CL=max(CARLEN)/10)

#Calculate an annual average of maximum carapace length:
#This value is entered in ESS.20XX Excel Spreadsheet
ann.max.surv.CL<-ddply(max.surv.CL,.(YEAR),summarize, AVG_MAX_CL=mean(MAX_CL, na.rm=T))

######################################### GRAPHICS ################################################
#Plot survey Maximum Carapace Length:
#Indicator Plot:
jpeg(filename="Survey.MAXCL.jpg")
ry<-quantile(ann.max.surv.CL$AVG_MAX_CL[ann.max.surv.CL$YEAR>1999&ann.max.surv.CL$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ann.max.surv.CL$AVG_MAX_CL[ann.max.surv.CL$YEAR>1999&ann.max.surv.CL$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.max.surv.CL, aes(YEAR,AVG_MAX_CL)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =29.02582 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =29.28792, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = ry,colour="red") + geom_hline(yintercept = yg,colour="dark green") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.max.surv.CL$YEAR), max(ann.max.surv.CL$YEAR), 3)) +
  scale_y_continuous(name="Carapace Length (mm)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 


