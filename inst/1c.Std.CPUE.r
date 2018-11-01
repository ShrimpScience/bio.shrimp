#' @title 1c.Std.CPUE
#' @description Calculates ESS portion of Commercial CPUE indicator as part of the Abundance Characteristics in our assessment 
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

#Calculate CPUE by record before filtering:
shrimp.COMLOG$CPUE<-(shrimp.COMLOG$WEIGHT/((trunc(shrimp.COMLOG$FHOURS/100)+((shrimp.COMLOG$FHOURS/100)-trunc(shrimp.COMLOG$FHOURS/100))/0.6)))

#CATCH TABLE:
# The catch table includes all vessels and their catch by SFA - includes factory trawlers
#############################################################################################
# The calculations for catch include an effort filter of FHOURS>0, which could be removed
# when only looking at quantifying catch.  Something to adjust for framework.
#############################################################################################
# Map in ArcMap to plot data points to evaluate fishing coordinates
# Inshore box was created in Google Earth, exported xml to convert into a shapefile.  
# Coordinates are:  -59.333333,46,0 -61.333333,46,0 -61.333333,45,0 -59.333333,45,0 -59.333333,46,0 

#OFFSHORE:
off.comlog<-subset(shrimp.COMLOG, !(BLAT>451000 && BLONG>592000) & SFA!=16 & WEIGHT>0 & FHOURS>0 & BTYPE<4, na.rm=F)
off.comlog$STRATUM<-off.comlog$SFA
off.comlog$TYPE<-'OFFSHORE'

#INSHORE:
ins.comlog<-subset(shrimp.COMLOG, BLAT>451000 & BLONG>592000 & WEIGHT>0 & FHOURS>0 & BTYPE!=4, na.rm=F)
ins.comlog$STRATUM<-ins.comlog$SFA
ins.comlog$TYPE<-'INSHORE'

#TRAP:
trap.inshore<-subset(shrimp.COMLOG, BLAT>451000 & BLONG>592000 & WEIGHT>0 & FHOURS>0 & BTYPE==4, na.rm=F)
trap.inshore$STRATUM<-17
trap.inshore$TYPE<-'TRAP'

#Combined overall catch table(populates the landing values for Table 1 and the catch portion of Table 6 in the Tables_Figures_2018_for_review.docx):
catch.all<-rbind(off.comlog,ins.comlog,trap.inshore)
write.csv(catch.all, file= "C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/All.Catch.for.GIS.Oct.2018.csv")
catch.table<-ddply(catch.all,.(YEAR,STRATUM,TYPE),summarize,CATCH_KG=sum(WEIGHT),CATCH_MT=sum(WEIGHT/1000))
catchSFA.table<-ddply(catch.all,.(YEAR,STRATUM),summarize,CATCH_KG=sum(WEIGHT),CATCH_MT=sum(WEIGHT/1000))
catch.strata17<-subset(catch.all,TYPE %in% c("INSHORE","TRAP"))
catch17.table<-ddply(catch.strata17,.(YEAR),summarize,CATCH_KG=sum(WEIGHT),CATCH_MT=sum(WEIGHT/1000))
catch17.table$STRATUM<-17
catch17.table$TYPE<-'COMBO'
catch<-rbind(catch.table,catch17.table)
write.csv(catch,file="C:/Users/cassistadarosm/Documents/SHRIMP/Data/2018 Assessment/Catch.Report.Oct.2018.csv")


#UNSTANDARDIZED TRAWL CPUE:
#CPUE CALCULATIONS:

#Calculate unstandardized annual CPUE (Kg/Hr):
#Converts effort from FHOURS to HOURS and DECIMAL HOURS
table(shrimp.COMLOG$BTYPE,shrimp.COMLOG$SFA)

# OFFSHORE SELECTION:
off.comlog<-subset(shrimp.COMLOG, !(BLAT>451000 && BLONG>592000) & SFA!=16 & WEIGHT>0 & FHOURS>0 & BTYPE<4 & !(BCODE %in% c(5631,102680)), na.rm=F)
off.comlog$STRATUM<-off.comlog$SFA
offshore.catch<-ddply(off.comlog,.(YEAR,STRATUM),summarize,CATCH_KG=sum(WEIGHT),CATCH_MT=round(sum(WEIGHT/1000)),EFFORT_HR=sum(FHOURS))
offshore.cpue<-ddply(off.comlog,.(YEAR,STRATUM),summarize,CATCH_KG=sum(WEIGHT),CATCH_MT=sum(WEIGHT/1000),AVG_CPUE=mean(CPUE))
offshore.cpue
# INSHORE SELECTION:
ins.comlog<-subset(shrimp.COMLOG, BLAT>451000 & BLONG>592000 & WEIGHT>0 & FHOURS>0 & BTYPE<4, na.rm=T)
ins.comlog$STRATUM<-17
inshore.catch<-ddply(ins.comlog,.(YEAR,SFA),summarize,CATCH_MT=round(sum(WEIGHT/1000)),EFFORT_HR=sum(FHOURS))
inshore.cpue<-ddply(ins.comlog,.(YEAR,STRATUM),summarize,CATCH_MT=sum(WEIGHT/1000),AVG_CPUE=mean(CPUE))
inshore.cpue



################################ Commercial Catch ##########################################
str(shrimp.COMLOG) #48,751 RECORDS
head(shrimp.COMLOG)
#INSHORE:
#Select data that is inshore only (blat<451000	and	blong<592000) with records
#that have WEIGHT and FHOURS that are not 0 or NA
ins.catch<-subset(shrimp.COMLOG, BLAT>451000 & BLONG>592000 & WEIGHT>0 & FHOURS>0 & BTYPE<5, na.rm=T)
ins.catch$STRATUM<-17
#Calculate inshore catch:
inshore.catch<-ddply(ins.catch,.(YEAR,STRATUM),summarize,CATCH_KG=sum(WEIGHT),.drop=F)

#OFFSHORE:
#Select data that is offshore only (blat>451000	and	blong>592000) with records
#that have WEIGHT and FHOURS that are not 0 or NA
off.catch<-subset(shrimp.COMLOG, !(BLAT>451000 & BLONG>592000) & WEIGHT>0 & FHOURS>0 & BTYPE<4 & SFA!=16, na.rm=T)
off.catch$STRATUM<-off.catch$SFA
#Calculate offshore catch:
offshore.catch<-ddply(off.catch,.(YEAR,STRATUM),summarize,CATCH_KG=sum(WEIGHT),.drop=F)

#Bring the ofshore and inshore files together, and sum by YEAR and SFA:
combined.catch<-rbind(inshore.catch,offshore.catch)
com.catch<-ddply(combined.catch,.(YEAR,STRATUM),summarize,TOT_CATCH_KG=sum(CATCH_KG),TOT_CATCH_MT=round(TOT_CATCH_KG/1000),.drop=F)
catch.table<-reshape(com.catch,v.names="TOT_CATCH_MT",idvar="YEAR",timevar="STRATUM", direction='wide')
#Catch value to be added to calculate exploitation rate against shrimp survey biomass (95-16SURVEY.MCD)
#Exploitation rate goes in ESS_20XX
com.catch.table<-catch.table[order(catch.table$YEAR),]

#Plot annual CPUE boxplot of inshore and offshore selection:
box.unst.cpue<-rbind(off.comlog,ins.comlog)
ggplot(subset(box.unst.cpue, CPUE>10),aes(YEAR,CPUE, group=YEAR)) + geom_boxplot() + theme_bw() +
  scale_x_continuous(name="Year",breaks=seq(min(box.unst.cpue$YEAR), max(box.unst.cpue$YEAR), 2)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,3500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Plot monthly CPUE boxplot of inshore and offshore selection:
ggplot(subset(box.unst.cpue, CPUE>10),aes(factor(YEAR),CPUE, group=YEAR)) + geom_boxplot() + facet_wrap(~MONTH)

#MERGE INSHORE WITH OFFSHORE CPUE
comlog_4strat.cpue<-rbind(offshore.cpue, inshore.cpue)
cpue.table<-reshape(comlog_4strat.cpue,v.names="AVG_CPUE",idvar="YEAR",timevar="STRATUM", direction='wide')
comlog_4strat.cpue<-cpue.table[order(cpue.table$YEAR),]
#print(comlog_4strat.cpue, digits=6)

#MEAN ANNUAL CPUE
str(comlog_4strat.cpue)
rowMeans(x = comlog_4strat.cpue[,2:ncol(comlog_4strat.cpue)], na.rm = TRUE)
mean<-rowMeans(x = comlog_4strat.cpue[,2:ncol(comlog_4strat.cpue)], na.rm = TRUE)
unstd.mean.cpue<-cbind(comlog_4strat.cpue, mean)
unstd.mean.cpue

#MEAN ANNUAL COMMERCIAL CATCH ACROSS SFAs
str(comlog_4strat.cpue)
comlog_4strat.cpue$MEAN<-rowMeans(x = comlog_4strat.cpue[,3:ncol(comlog_4strat.cpue)], na.rm = TRUE)
plot.mean<-melt(comlog_4strat.cpue,id.vars='YEAR')
plot.mean$GRP=NA
val1 <- which(is.na(plot.mean$value))
plot.mean[val1,"GRP"]=1
val2<- which(!is.na(plot.mean$value))
plot.mean[val2,"GRP"]=2

jpeg(filename="All_SFA_cpue.jpg")
cb.color<-c("#999999", "#D55E00", "#0072B2","#009E73","black")
ggplot(plot.mean, aes(YEAR,value, group=variable)) + 
  expand_limits(y=c(0,900)) + 
  geom_line(data=subset(plot.mean,variable=="AVG_CPUE.13"),aes(group='GRP',lty=variable,col=variable),lwd=1) + 
  geom_line(data=subset(plot.mean,variable=="AVG_CPUE.14"),aes(group='GRP',lty=variable,col=variable),lwd=1) + 
  geom_line(data=subset(plot.mean,variable=="AVG_CPUE.15"),aes(group='GRP',lty=variable,col=variable),lwd=1) + 
  geom_line(data=subset(plot.mean,variable=="AVG_CPUE.17"),aes(group='GRP',lty=variable,col=variable),lwd=1) + 
  geom_line(data=subset(plot.mean,variable=="MEAN"),aes(group='GRP',lty=variable,col=variable), lwd=1) + 
  geom_point(data=subset(plot.mean,variable=="AVG_CPUE.13")) +
  scale_linetype_manual("",labels=c("Stratum 13", "Stratum 14", "Stratum 15", "Stratum 17", "Mean"),values=c(2,3,4,5,1)) +
  scale_color_manual("",labels=c("Stratum 13", "Stratum 14", "Stratum 15", "Stratum 17", "Mean"),values=cb.color) +
  scale_x_continuous(name="Year",breaks=seq(min(plot.mean$YEAR), max(plot.mean$YEAR), 2)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,900,150)) + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0, 1), 
        legend.justification = c(-0.5, 1.05)) 
dev.off()    

##############################################
#STANDARDIZED TRAWL CPUE - GLM fishing April to July inclusively and only boats that have fished 7 years or more,
#do not process onboard, and NS fleet only.  
#Standarized to highliner, highest catch month and SFA
head(shrimp.COMLOG) #50,690 RECORDS
#Select for weights and hours not equal to zero and boat types from NS fleet only (1 & 2)
comlog.clean<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE<3, na.rm=T)
str(comlog.clean) #28,850 RECORDS

comlog.dat<-comlog.clean[,c("BCODE", "FDATE", "YEAR", "MONTH", "SFA", "FHOURS", "WEIGHT", "CPUE")]
head(comlog.dat)

#First, select April-July inclusive (months 4 to 7)
month.filter<-comlog.dat[comlog.dat$MONTH>3 & comlog.dat$MONTH<8,]
dim(month.filter)#20,435 RECORDS

#Plot annual boxplot of filtered data for selected months
ggplot(month.filter,aes(YEAR,CPUE, group=YEAR)) + geom_boxplot() + theme_bw() +
  scale_x_continuous(name="Year",breaks=seq(min(month.filter$YEAR), max(month.filter$YEAR), 2)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,3500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Plot monthly boxplot of filtered data for selected months
ggplot(month.filter,aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() + facet_wrap(~YEAR) +
  scale_x_continuous(name="MONTH", breaks=seq(4,7,1)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Plot SFA boxplot of filtered data for selected months
ggplot(subset(month.filter, !is.na(SFA)),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() + facet_wrap(~SFA) +
  scale_x_continuous(name="MONTH", breaks=seq(4,7,1)) + ggtitle("All SFA") +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Plot Annual SFA boxplot of filtered data for selected months
ggplot(subset(month.filter, SFA==13),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() + facet_wrap(~YEAR) +
  scale_x_continuous(name="Month", breaks=seq(4,7,1), labels=c('Apr', 'Jun', 'Jul', 'Aug')) + ggtitle("SFA 13") +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggplot(subset(month.filter, SFA==14),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() + facet_wrap(~YEAR) +
  scale_x_continuous(name="Month", breaks=seq(4,7,1), labels=c('Apr', 'Jun', 'Jul', 'Aug')) + ggtitle("SFA 14") +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggplot(subset(month.filter, SFA==15),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() + facet_wrap(~YEAR) +
  scale_x_continuous(name="Month", breaks=seq(4,7,1), labels=c('Apr', 'Jun', 'Jul', 'Aug')) + ggtitle("SFA 15")
scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2500,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Plot monthly boxplot of filtered data for latest year
ggplot(subset(month.filter, YEAR==2018),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() +
  scale_x_continuous(name="MONTH", breaks=seq(4,7,1)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2000,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Second, include only boats that have fished for 7 years or more
fishing.years = ddply(month.filter,.(BCODE), summarize, F.YR=length(unique(YEAR)))
year.7plus.filter<-subset(fishing.years, F.YR>6)
fish.filter<-subset(month.filter,BCODE %in% year.7plus.filter$BCODE)
dim(fish.filter)#14,731 RECORDS

#Finally, remove Final Venture and Island Provider, which are bcodes -'5631', '102680', the factory trawlers
boat.filter<-subset(fish.filter, BCODE!=5631 & BCODE!=102680)

#Create a filtered file containing unstandardized CPUE
write.table(boat.filter,file="filtered_cpue.2017.txt") 
#write.csv(boat.filter,paste("I:/Offline Data Files/Shrimp/Filtered.CPUE.Comlog.Data",Sys.Date(),".csv",sep=""), row.names=F)
filtered.cpue<-boat.filter

#Calculate annual unstandardized CPUE for the filtered file - Res Doc figure
unstand.cpue<-ddply(filtered.cpue,.(YEAR),summarize,MEAN.CPUE=mean(CPUE,na.rm=T))
write.table(unstand.cpue,file="unstand.CPUE_filtered.file.txt") 

#Run the GLM
ggplot(filtered.cpue,aes(YEAR,UNSTD_CPUE, fill=YEAR,color=YEAR)) + geom_violin() +
  theme_bw(base_size = 12)

#Add columns that transforms variables from numeric to factors:
tt<-transform(filtered.cpue, FBCODE=as.factor(BCODE), FSFA=as.factor(SFA), FMONTH=as.factor(MONTH), FYEAR=as.factor(YEAR))

#with Gaussian error distribution it is basically an ANOVA, but could try other error distributions
M1 <- glm(formula = CPUE ~ factor(BCODE) + factor(YEAR) + factor(MONTH) + factor(SFA), family = gaussian, 
          data = tt, na.action = na.exclude, 
          control = list(epsilon = 0.0001, maxit = 50, trace = F))

#Look at the summary of M1 and note the AIC number at the bottom, which is a measure of the complexity,
#and goodness of fit of the GLM as it is defined
summary(M1)

#Run ANOVA to confirm significance of factors

Anova(M1) #significance of factors

#Plots dependent factors against CPUE:
j2 = allEffects(M1)
plot(j2) # plots fixing everything at their means but the var of interest

#then look at the YEAR effect, which is essentially the GLM standardized CPUE estimate
j$year
str(j)

#The "predict.val" dataframe holds the values at which I want predicted estimates.  
##The example below is arbitrarily defined at the first row of all values except for all years.  
##Because there are no interaction terms, changing this will only move the estimates up or down,
##but will not actually change the temporal trend.

predict.val = data.frame(BCODE=filtered.cpue$BCODE[1], YEAR=sort(unique(filtered.cpue$YEAR)), MONTH=filtered.cpue$MONTH[1],
                         SFA=filtered.cpue$SFA[1])

#To MANUALLY identify the highligher for the latest fishing year
highliner.id<-ddply(subset(filtered.cpue,YEAR==2017),.(BCODE), summarize, MEAN_CPUE=mean(CPUE))

#HARD CODED  - 2017 - FIXING BCODE AT MEAN OF HIGHLINER - 104885, MONTH AS MEAN IN JULY, SFA AS MEAN IN SFA14
pred.val = data.frame(BCODE=as.factor(104885), YEAR=sort(unique(filtered.cpue$YEAR)), MONTH=as.factor(7), SFA=as.factor(14))

#Run the GLM (M1) on newdata (predict.val)
cpue.predict = predict( M1, newdata=pred.val, type = c("response"), se.fit=T)
pred.val$cpue = cpue.predict $fit
pred.val$cpue_se = cpue.predict $se.fit

#Define the upper and lower CPUE bounds
pred.val$u.bound = (pred.val$cpue + 2*pred.val$cpue_se)
pred.val$l.bound = (pred.val$cpue - 2*pred.val$cpue_se)
write.table(pred.val,file="Stand.Com.CPUE.txt") 

