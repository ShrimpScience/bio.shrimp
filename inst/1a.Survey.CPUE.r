#' @title 1a.Survey.CPUE
#' @description Calculates Survey CPUE indicator as part of the Abundance Characteristics 
#' @description in our assessment 
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @return data.frame of survey data called 'shrimp.survey'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom bio.survey Prepare.strata.data
#' @importFrom bio.survey Prepare.strata.file
#' @importFrom bio.survey Stratify
#' @importFrom bio.survey boot.strata
#' @importFrom bio.lobster convert.dd.dddd
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
### Start: December 20, 2017 
require(bio.shrimp)
MMM = "Mike is awesome."
#SURVEY DETAILS:
#1 nautical mile = 1852 metres
#1.25 nautical miles travelled during 30 minute tow at 2.5 knots
#standard unit = 1.25 nm x 17.4 m = 40,281 m2
strata.area.data<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/Strata.Surface.Area.csv",header=T)
strata.area.data$Num_Units<-((strata.area.data$km2/40281)*1000000)

################### Survey CPUE and Biomass ############################## 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
summary(shrimp.survey) # Quick check on all range in values are within expected parameters
write.csv(shrimp.survey,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpSurvey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 
#  61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58   60   60   60   60 
#2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  60   60   60   60   60   60   60   60   60 

#Standardize catch to trawlable unit all data:
# 1- Select successful tows only and year range for reporting (data range available: 1982 to 2017):
shrimp.surv<-subset(shrimp.survey, SETCODE<3 & YEAR>1994)

# 2- For YEARS where WING was not extracted from net mensuration, the expected measurement was used.
# In the case of YEARS where partial net mensuration data is available, the mean wing measurement
# was calculated and used in the standardization.  Values were hardcoded in the database for all 
# years except for 2006-07. 
shrimp.surv[shrimp.surv$YEAR==2008,]$WING<-17.4

# 4- Replace WING==0/NA and H_HEADLINE==0/NA with mean of WING/H_HEADLINE measurement for that 
# year's survey measurements
y=unique(shrimp.surv$YEAR)
for(i in 1:length(y)){
  mW = mean(shrimp.surv$WING[shrimp.surv$YEAR==y[i] & shrimp.surv$WING!=0], na.rm = T)
  mH = mean(shrimp.surv$H_HEIGHT[shrimp.surv$YEAR==y[i] & shrimp.surv$H_HEIGHT!=0], na.rm = T)
  shrimp.surv$WING[shrimp.surv$YEAR==y[i]&is.na(shrimp.surv$WING)]=mW
  shrimp.surv$H_HEIGHT[shrimp.surv$YEAR==y[i]&is.na(shrimp.surv$H_HEIGHT)]=mH
  shrimp.surv$WING[shrimp.surv$YEAR==y[i]&shrimp.surv$WING==0]=mW
  shrimp.surv$H_HEIGHT[shrimp.surv$YEAR==y[i]&shrimp.surv$H_HEIGHT==0]=mH
}

#In 1995 and 1996 different trawl nets were used.  From 1997 onward the gear type has been constant.
#Standardize catches and calculate density:
y=unique(shrimp.surv$YEAR)
shrimp.surv$STD_CATCH<-NA
shrimp.surv$ADJ_STD_CATCH<-NA
shrimp.surv$DENSITY<-NA
## ADJ_STD_CATCH parameter created to calculate catch during comparative years only ## 
for(j in 1:length(y)){
  #1995 and 1996 catches are standardized and adjusted to gear being used from 1997 onward
   if (y[j]==1995 ){
     shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]<-
     shrimp.surv[shrimp.surv$YEAR==y[j],"WEIGHT"] *
    ((15.1/shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])*
    (1.19/shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]))
     #Adjustment to final catch is     
     shrimp.surv[shrimp.surv$YEAR==y[j],"ADJ_STD_CATCH"]<-
       (shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"] -
          41.61)/1.1826
     #Use adjusted catch in density calculations
     shrimp.surv[shrimp.surv$YEAR==y[j],"DENSITY"]<-
       (shrimp.surv[shrimp.surv$YEAR==y[j],"ADJ_STD_CATCH"]*
       1000)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852*shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])
     
  #Comparative work done in 1996, catch was standardized to net mensuration and adjusted 
  #according to linear regression equation : y=0.6564(x)-38.77, x = Cody & Kathryn catch
  }else if (y[j]==1996){
    shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]<-
    shrimp.surv[shrimp.surv$YEAR==y[j],"WEIGHT"] *
    (18.1/shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])*
    ((1.24*1852)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852))
  #Adjustment to final catch is     
    shrimp.surv[shrimp.surv$YEAR==y[j],"ADJ_STD_CATCH"]<-
      (1.5235*shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"] +
      (38.77-41.61))/1.1826
  #Use adjusted catch in density calculations
    shrimp.surv[shrimp.surv$YEAR==y[j],"DENSITY"]<-
      (shrimp.surv[shrimp.surv$YEAR==y[j],"ADJ_STD_CATCH"]*
         1000)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852*shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])
    
  #Comparative work done in 1997, catch was standardized to net mensuration and adjusted 
  #according to linear regression equation : y=0.8457(x)-41.61, x = Cody & Kathryn catch
  } else if (y[j]==1997){
    shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]<-
      shrimp.surv[shrimp.surv$YEAR==y[j],"WEIGHT"] *
    (17.3/shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])*
    ((1.21*1852)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852))
  #Density correction applied to the calculations
    shrimp.surv[shrimp.surv$YEAR==y[j],"DENSITY"]<-
      (1.1825*(shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]*
      1000)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852*shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])) +
      1.027
    
  }else if (y[j]>1997){
  shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]<-
    shrimp.surv[shrimp.surv$YEAR==y[j],"WEIGHT"]*
    (17.4/shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])*
    ((1.25*1852)/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852))
  
  shrimp.surv[shrimp.surv$YEAR==y[j],"DENSITY"]<-
    shrimp.surv[shrimp.surv$YEAR==y[j],"STD_CATCH"]*
    1000/(shrimp.surv[shrimp.surv$YEAR==y[j],"DIST"]*1852*shrimp.surv[shrimp.surv$YEAR==y[j],"WING"])
}
}

#STD_CATCH and DENSITY should all be populated with a value.
#Check for NAs and negative values.  Replace as needed.
#Negative values in DENSITY are replaced with zero values:

    shrimp.surv[!is.na(shrimp.surv$ADJ_STD_CATCH) & 
                   shrimp.surv$ADJ_STD_CATCH<0,"ADJ_STD_CATCH"]<-0 
    shrimp.surv[!is.na(shrimp.surv$DENSITY) & 
                   shrimp.surv$DENSITY<0,"DENSITY"]<-0 

#Create a column that brings together the final catch values for each year:
shrimp.surv$FINAL_CATCH<-NA

shrimp.surv$FINAL_CATCH <- ifelse(is.na(shrimp.surv[,"ADJ_STD_CATCH"]),
       shrimp.surv[,"STD_CATCH"],
       shrimp.surv[,"ADJ_STD_CATCH"]
       )
head(shrimp.surv) 
################################################################################      
#Create table of standardized values to use with the Stratify function:
#Calculate a mean, and standard deviation on catch by SFA:

survey.dat<-select(shrimp.surv, YEAR,STRATUM,XSET,FINAL_CATCH)

#survey.dat<-ddply(shrimp.surv,.(YEAR,STRATUM),summarize,AVG_STD_CATCH=mean(FINAL_CATCH),
#           STDEV=sd(FINAL_CATCH,na.rm=T))

#Plot Standardized survey catch:
ggplot(survey.dat,aes(YEAR,AVG_STD_CATCH)) + geom_bar(stat='identity', position="stack")

#Run data through the stratify function one year at a time:
strata.Shrimp<-rename(strata.area.data,c("Stratum"="Strata","km2"="Area","Num_Units"="NH"))


outputs2 <- as.data.frame(do.call(rbind,out))
out <- list()
m=0
y=unique(survey.dat$YEAR)

for(i in 1:length(y)){
  SurveyCatchStd = survey.dat[survey.dat$YEAR==y[i]]
  SurveyCatchStd<-rename.vars(SurveyCatchStd, from='FINAL_CATCH', to='FINAL.CATCH')
  SurveyCatch = SurveyCatchStd[,c("STRATUM","FINAL.CATCH")]
  SurveyCatch$STRATA.ID=SurveyCatch$Strata=SurveyCatch$STRATUM
  SurveyCatchStd = Prepare.strata.data(SurveyCatch)
  strata.Shrimp = Prepare.strata.file(strata.Shrimp)
  browser()
  survey.CPUE<-Stratify(SurveyCatchStd,strata.group=strata.Shrimp, species=FINAL.CATCH)
  Shrimp.boot<-boot.strata(survey.CPUE,nresamp=1000,method="BWR")
  Boot.Sum<-summary.boot(Shrimp.boot,CI.method = "Percentile",prints=T)
  d<-survey.CPUE$Strata
  print(y[i])
  print(survey.CPUE$Strata[i])
  print(survey.CPUE$Mean[i])
  print(SurveyCatchStd$AVG.STD.CATCH)
  m=m+1
  out[[m]]<-c(y[i],d)
  out[[m]]<-c(Strata=survey.CPUE$Strata,Set=survey.CPUE$Sets,Mean=survey.CPUE$Mean,
                       survey.CPUE$Std.Err,Strat_Mean=mean(Shrimp.boot$boot.means))
}
outputs2 <- as.data.frame(do.call(rbind,out))




#Isolate last survey year:
survey.2017<-subset(shrimp.surv2,YEAR==2017)

#survey.lyear<-cbind.data.frame(SFA=survey.2016$SFA,STD.CATCH=survey.2016$STD_CATCH)
#head(survey.lyear)

survey.lyear = survey.2017[,c("SFA","STD_CATCH")]


#survey.2017<-subset(shrimp.surv2,YEAR==2017)
survey.last.year<-ddply(survey.2017,.(YEAR,SFA),summarize,TOT_WEIGHT=sum(WEIGHT), TOT_CATCH=sum(STD_CATCH),
                        MEAN_CATCH=mean(STD_CATCH))
head(survey.last.year)
#Bootstrap stratified standardized catch rate
#Use Stratify function from S.Smith 
#Create dataframe that contains the strata, the area in km2, 
#Data Formatting Requirements:
#Strata = SFA, Area = the area of the SFA in km2, NH = total number of possible trawlable units in area
##strata.Shrimp<-data.frame(Strata=c(13,14,15,17),Area=c(1620,1517,948,1415),NH=c(40207.55862,37653.07586,23535.30115,35128.22422))

#SurveyCatchStd<-survey.lyear[,c("SFA","STD.CATCH")]
SurveyCatchStd<-survey.lyear
SurveyCatchStd$STRATA.ID=SurveyCatchStd$Strata=SurveyCatchStd$SFA
str(SurveyCatchStd)
#'data.frame':	60 obs. of  4 variables:
#$ SFA      : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STD_CATCH: num  60 75.4 69.3 98.4 105.3 ...
#$ Strata   : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STRATA.ID: int  15 15 15 15 15 15 15 15 15 15 ...

SurveyCatchStd = Prepare.strata.data(SurveyCatchStd)
str(SurveyCatchStd)
#Classes ‘strata.data’ and 'data.frame':	60 obs. of  4 variables:
#$ SFA      : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STD.CATCH: num  60 75.4 69.3 98.4 105.3 ...
#$ Strata   : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STRATA.ID: int  15 15 15 15 15 15 15 15 15 15 ...

strata.Shrimp = Prepare.strata.file(strata.Shrimp)
str(strata.Shrimp)
#List of 2
#$ Strata: num [1:4] 13 14 15 17
#$ NH    : num [1:4] 40208 37653 23535 35128

survey.CPUE<-Stratify(SurveyCatchStd,strata.group=strata.Shrimp, species=STD.CATCH)

survey.CPUE
#    Strata  Sets         Wh                 Mean            Std. Err.        RE(%) Sets.w.Spec
#[1,] "13"   "15" "0.294508742366013" "153.837463874531" "21.9932588482337" "Not.Est" "14"       
#[2,] "14"   "15" "0.275797894683034" "217.455210751274" "25.0407313656335" "Not.Est" "15"       
#[3,] "15"   "15" "0.172389276563638" "117.519140363592" "17.0506820551315" "Not.Est" "15"       
#[4,] "17"   "15" "0.257304086387315" "240.354123751201" "64.3790642220654" "Not.Est" "15"   

Shrimp.boot<-boot.strata(survey.CPUE,nresamp=1000,method="BWR")
summary.boot(Shrimp.boot,CI.method = "Percentile",prints=T)
#Bootstrap mean is survey CPUE used as indicator
#FOR 2017:
#Original Mean = 171.3 
#Original Variance = 529.5 
#Number of bootstraps =  1000 
#Bootstrap Mean= 170.7 
#Variance of Bootstrap Mean= 496.1 
#Percentile CI's for alpha= 0.05 are  129.0 217.4 
#Length = 88.43 
#Shape= 0.1775 
#Method =  BWR 
#[[1]]
#2.5% 97.5%   50% 
#129.0 217.4 169.3 
#
#[[2]]
#2.5%  97.5%    50% 
#0.1110 0.3013 0.1994 

#PLOT
direct <- setwd("C:/Users/cassistadarosm/Documents/SHRIMP/Data/2017 Update/Data Input for Plots/")	
getwd()
str.surv.catch<-read.csv("Survey.Stratified.Catch.per.Std.Tow.2017.csv",header=T)
str.surv.catch
catch.sfa<-str.surv.catch[,1:5]
surv.catch.data<-melt(catch.sfa,id.vars = "YEAR")
mean.catch<-str.surv.catch[,c(1,6:8)]

ggplot(str.surv.catch,aes(x=YEAR, y=STR_MEAN_CT)) +  
  geom_errorbar(aes(ymin=LOWER_BOUND, ymax=UPPER_BOUND)) + geom_line()+ geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2017,3)) +
  scale_y_continuous(limits=c(0,400), name="Mean Stratified CPUE") 


ggplot(mean.catch,aes(YEAR,STR_MEAN_CT) + geom_errorbar(aes(ymin=LOWER_BOUND, ymax=UPPER_BOUND)) + geom_line() + geom_point() + 
         scale_x_continuous(name="Year", breaks= seq(1995,2017,3)) + facet_wrap(~SFA) +
         scale_y_continuous(limits=c(20,75), name="Mean Commercial Count") + theme_bw() + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
