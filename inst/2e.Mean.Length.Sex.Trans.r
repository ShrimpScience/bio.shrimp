#' @title 2e.Mean Length at Sex Transition
#' @description Calculates an annual mean length at sex transition indicator as part of the Production Characteristics in our assessment 
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
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom gdata rename.vars

#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: October 29, 2017 
require(bio.shrimp)

######################### All Samples Mean CL at Sex Transition ########################### 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
summary(shrimp.survey) # Quick check on all range in values are within expected parameters
write.csv(shrimp.survey,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpSurvey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#Acquire Data from MILLIM View data query:
shrimp.db('MILLIM.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('MILLIM', oracle.username=oracle.username, oracle.password = oracle.password)
str(MILLIM.VIEW) #1,235,065 RECORDS
write.csv(MILLIM.VIEW,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpMILLIM.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(MILLIM.VIEW)

#Select for all data of transitionals or primnet (there are no incidences of both being 1) :
millim.all<-subset(MILLIM.VIEW, (TRAN==1 | PRIMNET==1)) #203,438 RECORDS

table(millim.all$GEAR)
    1      2      3      4      6      7      8 
62689  22591    250 116893    658    152    205 

#Calculate the average carapace length for primiparous/transitional portion of the catch:
mean.CL.Trans<-ddply(millim.all,.(BCODE,FDATE,YEAR,SFA,XSET),summarize,AVG.CL=mean(CARLEN)/10)

#This is the values to be added to ess.20XX table.  Eventhough the original sql script had a specification to use only commercial data to calculate these this was not the case.  The filtering conditions had a bug in it, which nullified the conditions.  The values below are inclusive of all gear types.
ess.mean.cl<- ddply(mean.CL.Trans,.(YEAR),summarize,AVG.CL=mean(AVG.CL,na.rm=T))


####################### Commercial Samples Mean CL at Sex Transition ########################## 

#Commercial year ranges from 1994 to now
millim.com<-subset(millim.all, GEAR<4)

#Calculate the average carapace length for primiparous/transitional portion of the catch:
com.mean.CL.Trans<-ddply(millim.com,.(BCODE,FDATE,YEAR,SFA,XSET),summarize,AVG.CL=mean(CARLEN)/10)

com.mean.cl<- ddply(com.mean.CL.Trans,.(YEAR),summarize,AVG.CL=mean(AVG.CL,na.rm=T))



######################### Survey Samples Mean CL at Sex Transition ############################ 

#Survey year ranges from 1982 to now, with a gap in series between 1989 to 1992 inclusive, and no data for 1994
millim.surv<-subset(millim.all, GEAR>3)

#Calculate the average carapace length for primiparous/transitional portion of the catch:
surv.mean.CL.Trans<-ddply(millim.surv,.(BCODE,FDATE,YEAR,SFA,XSET),summarize,AVG.CL=mean(CARLEN)/10)

surv.mean.cl<- ddply(surv.mean.CL.Trans,.(YEAR),summarize,AVG.CL=mean(AVG.CL,na.rm=T))

######################################### GRAPHICS ################################################
#Plot mean carapace length of transitionals:
#Indicator Plot:
ess.values<-subset(surv.mean.cl,YEAR>1946)
jpeg(filename="mean_CLTran.jpg")
ry<-quantile(ess.values$AVG.CL[ess.values$YEAR>1999&ess.values$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess.values$AVG.CL[ess.values$YEAR>1999&ess.values$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ess.values, aes(YEAR,AVG.CL)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =23.93561 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 24.23894, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ess.values$YEAR), max(ess.values$YEAR), 3)) +
  scale_y_continuous(name="Mean CL Trasitionals (mm)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 

#Plot commercial and survey differences in mean carapace lengths of transitionals:
ess.mean.cl$DATA<-"All"
com.mean.cl$DATA<-"Commercial"
surv.mean.cl$DATA<-"Survey"
plot.dat<-rbind(ess.mean.cl,com.mean.cl,surv.mean.cl)
plot.dat<-subset(plot.dat,YEAR>1946)

CBPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(plot.dat,aes(YEAR,AVG.CL),col=DATA) + geom_line(data=subset(plot.dat,DATA=="All")) + geom_line(data=subset(plot.dat,DATA=="Survey")) + geom_line(data=subset(plot.dat,DATA=="Commercial"))

ggplot(plot.dat,aes(YEAR,AVG.CL,col=DATA)) + geom_line(group=DATA) + geom_point() +
  theme_bw() + labs(x = "Year", y = "Average Carapace Length (mm)") +scale_x_continuous(name="Year", breaks= seq(1982,2018,3))



