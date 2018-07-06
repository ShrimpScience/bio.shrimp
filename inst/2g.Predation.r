#' @title 2g.Predation
#' @description Calculates an annual mean overall predation indicator as part of the Production Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey data
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
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
#' @seealso \code{\link{template.function}}, \url{http://www.github.com/Beothuk/bio.template}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: December 20, 2017 
require(bio.shrimp)

################### Commercial Catches
################### Survey CPUE and Biomass ############################## 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1892 RECORDS
#write.csv(shrimp.survey,paste("I:/Offline Data Files/Shrimp/Survey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
#61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58   60 
#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 
#60   60   60   60   60   60   60   60   60   60   60 

#Standardize catch to trawlable unit all data:
#1 nautical mile = 1852 metres

#
# 1- Select successful tows only and year range for reporting (data range available: 1982 to 2017):
shrimp.surv<-subset(shrimp.survey, SETCODE<3 & YEAR>1994)

# 2- In YEAR==1996, there was some comparative work.There are 11 tows done with the Cody & Kathryn, 
# and those are excluded from analyses:
shrimp.surv2<-subset(shrimp.surv, CRUISE!="CK9601")

# 3- For YEARS where WING was not extracted from net mensuration, the expected measurement was used.
# In the case of YEARS where partial net mensuration data is available, the mean wing measurement
# was calculated and used in the standardization.  Values were hardcoded in the database for all 
# years except for 2006-07 
shrimp.surv2[shrimp.surv2$YEAR==2008,]$WING<-17.4

# 4- Replace WING==0/NA H_HEADLINE==0/NA with mean of WING/H_HEADLINE measurement for that 
# year's survey measurements
y=unique(shrimp.surv2$YEAR)
for(i in 1:length(y)){
  mW = mean(shrimp.surv2$WING[shrimp.surv2$YEAR==y[i] & shrimp.surv2$WING!=0], na.rm = T)
  mH = mean(shrimp.surv2$H_HEIGHT[shrimp.surv2$YEAR==y[i] & shrimp.surv2$H_HEIGHT!=0], na.rm = T)
  shrimp.surv2$WING[shrimp.surv2$YEAR==y[i]&is.na(shrimp.surv2$WING)]=mW
  shrimp.surv2$H_HEIGHT[shrimp.surv2$YEAR==y[i]&is.na(shrimp.surv2$H_HEIGHT)]=mH
  shrimp.surv2$WING[shrimp.surv2$YEAR==y[i]&shrimp.surv2$WING==0]=mW
  shrimp.surv2$H_HEIGHT[shrimp.surv2$YEAR==y[i]&shrimp.surv2$H_HEIGHT==0]=mH
}

#Standardize catches and calculate density:
shrimp.surv2$STD_CATCH<-shrimp.surv2$WEIGHT*(17.4/shrimp.surv2$WING)*((1.25*1852)/(shrimp.surv2$DIST*1852))
shrimp.surv2$DENSITY<-shrimp.surv2$WEIGHT*1000/(shrimp.surv2$DIST*1852*shrimp.surv2$WING)

#Calculate a mean, standard deviation and coefficient of variation on catch by SFA:
survey.dat<-ddply(shrimp.surv2,.(YEAR,SFA),summarize,MSTD_CATCH=mean(STD_CATCH,na.rm=T),
                  STDEV=sd(STD_CATCH,na.rm=T))
head(survey.dat)

#Plot Standardized survey catch:
ggplot(survey.dat,aes(YEAR,MSTD_CATCH)) + geom_bar(stat='identity', position="stack")

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
#Strata = SFA, Area = the area of the SFA in km2, NH = total number of possible sets in area
strata.Shrimp<-data.frame(Strata=c(13,14,15,17),Area=c(1620,1517,948,1415),NH=c(40207.55862,37653.07586,23535.30115,35128.22422))
strata.Shrimp

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
