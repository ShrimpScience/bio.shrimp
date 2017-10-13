#' @title ess_shrimp
#' @description Perform Shrimp Stock Assessment for the fishing season
#' @param size.range defines the minimum and maximum value and is a filter (default is 0, 220mm CW)
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @param bin.size aggregates the abundance into size bins (default is 5mm bins)
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
### Start: April 3, 2017 
require(bio.shrimp)

######################### Commercial Landings ############################
#Commercial data query
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Comlogs', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.COMLOG)
#write.csv(shrimp.COMLOG,paste("I:/Offline Data Files/Shrimp/Comlog.Data",Sys.Date(),".csv",sep=""), row.names=F)

# Select ESS fleet - mobile,Gulf, and Trap, and records with effort>0 and catch>0:
AllSFA.fleet<-subset(shrimp.COMLOG,FHOURS>0 & WEIGHT>0 & SFA!= 16)
#AllSFA.fleet<-subset(shrimp.COMLOG,BTYPE<4)
a<-subset(AllSFA.fleet,YEAR==1995)
land.com<-ddply(shrimp.COMLOG,.(YEAR,SFA),summarize,LAND_KG=sum(WEIGHT,na.rm=T),
                LAND_MT=sum(WEIGHT/1000,na.rm=T))
catch<-land.com[,-3]
a<-t(catch)
catch.table<-reshape(catch,v.names="LAND_MT",idvar="YEAR",timevar="SFA", direction='wide', na.rm=T)
comlog_4strat.cpue<-cpue.table[order(cpue.table$YEAR),]
com.catch <- gather(catch, key = "SFA", value = "sum", LAND_MT)

com.landeff<-ddply(AllSFA.fleet,.(YEAR,MONTH,BTYPE),summarize,LAND_KG=sum(WEIGHT,na.rm=T),
                   LAND_MT=sum(WEIGHT/1000,na.rm=T),EFF_HRS=sum(FHOURS/1000,na.rm=T),
                   EFFHR=sum(trunc(FHOURS/100)+((FHOURS/100)-trunc(FHOURS/100))/0.6))

#FOR ME:
ggplot(com.landeff,aes(factor(MONTH),LAND_KG/1000, group=YEAR, col=factor(YEAR))) + 
  xlab("Month") + ylab("Landings (mt)") + geom_line()

ggplot(com.landeff,aes(factor(MONTH),EFF_HRS, group=YEAR, col=factor(YEAR))) + 
  xlab("Month") + ylab("Effort (hr)") + geom_line()

ggplot(com.landeff,aes(factor(MONTH),EFF_HRS,group=YEAR)) + 
  xlab("Month") + ylab("Effort (hr)") + geom_line() + geom_line(data=subset(com.landeff,
  YEAR==2016),col="blue", size=1)

#FOR OTHERS:
ggplot(com.landeff,aes(factor(MONTH),LAND_KG/1000,group=YEAR)) + 
  xlab("Month") + ylab("Landings (mt)") + geom_line() + geom_line(data=subset(AllSFA.fleet,
  YEAR==2016),col="blue", size=1)



#Annual Commercial Landings:
str(mob.fleet)
ann.com<-ddply(com.landeff,.(YEAR),summarize,LAND_MT=round(sum(LAND_MT)),EFF_HR=round(sum(EFF_HRS)),EFF=round(sum(EFFHR))
a<-subset(mob.fleet,YEAR==1993 & SFA==16)

################### Survey CPUE and Biomass ############################## 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey)
#write.csv(shrimp.survey,paste("I:/Offline Data Files/Shrimp/Survey.Data",Sys.Date(),".csv",sep=""), row.names=F)

#'data.frame':	1892 obs. of  33 variables:
#  $ CRUISE  : chr  "CK9501" "CK9501" "CK9501" "CK9501" ...
#$ XSET    : int  1 2 3 4 5 6 7 8 9 10 ...
#$ SFA     : int  17 17 17 17 17 17 13 13 13 13 ...
#$ STRATUM : logi  NA NA NA NA NA NA ...
#$ SETCODE : int  1 1 1 1 1 1 1 1 1 1 ...
#$ GEAR    : int  4 4 4 4 4 4 4 4 4 4 ...
#$ FDATE   : POSIXct, format: "1995-05-31" "1995-05-31" "1995-05-31" ...
#$ SPEED   : num  2.3 2.5 2.3 2.3 2.1 2.3 2.4 2.4 2.5 2.3 ...
#$ HOW_SP  : int  NA NA NA NA NA NA NA NA NA NA ...
#$ BLAT    : int  452176 452700 452402 453306 453423 453636 454161 454199 454782 454528 ...
#$ BLONG   : int  605822 604146 603250 602310 600581 593840 590383 585130 584847 583979 ...
#$ ELAT    : int  452171 452668 452416 453323 453454 453597 454233 454269 454662 454577 ...
#$ ELONG   : int  605657 604322 603081 602151 600435 593999 590214 584992 584824 584122 ...
#$ BTIME   : chr  "05:38" "08:37" "10:37" "13:18" ...
#$ ETIME   : chr  "06:08" "09:08" "11:07" "13:48" ...
#$ BEARING : int  110 110 115 85 90 275 80 76 190 310 ...
#$ DURATION: int  30 30 30 30 30 30 30 30 30 30 ...
#$ DIST    : num  1.16 1.25 1.2 1.13 1.1 1.22 1.3 1.22 1.23 1.14 ...
#$ HOW_DIST: int  1 1 1 1 1 1 1 1 1 1 ...
#$ H_HEIGHT: num  NA NA NA NA NA NA NA NA NA NA ...
#$ BDEPTH  : int  61 75 100 95 93 110 104 133 145 141 ...
#$ EDEPTH  : int  69 NA NA 103 98 99 103 141 145 152 ...
#$ ADEPTH  : int  65 75 100 99 96 105 104 137 145 147 ...
#$ TEMP    : num  0.2 0.7 0.9 0.8 0.7 1.4 1.9 2.4 2.6 2.7 ...
#$ WIND_DIR: int  315 293 293 NA 45 23 NA NA NA NA ...
#$ WIND_SP : int  10 5 5 0 10 10 0 0 0 0 ...
#$ WAVE_HT : int  0 1 2 0 1 1 1 1 1 1 ...
#$ WEIGHT  : int  90 31 40 681 211 16 2 127 172 403 ...
#$ XCOUNT  : int  71 75 78 84 91 88 45 63 75 78 ...
#$ WING    : num  16.4 20.7 15.9 15.1 14.8 ...
#$ CV_LAT  : num  45.4 45.5 45.4 45.6 45.6 ...
#$ CV_LONG : num  -61 -60.7 -60.5 -60.4 -60.1 ...
#$ YEAR    : num  1995 1995 1995 1995 1995 ...
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

# 1- Select successful tows only and year range for reporting:
shrimp.surv<-subset(shrimp.survey, SETCODE<3 & YEAR>1994)
#shrimp.surv<-subset(shrimp.survey, SETCODE<3)

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
ess.sfa<-subset(survey.dat,SFA<17)
ggplot(survey.dat,aes(YEAR,STD_CATCH)) + geom_bar(stat='identity', position="stack")

#Isolate last survey year:
survey.1985<-subset(shrimp.surv2,YEAR==1985)
#survey.lyear<-cbind.data.frame(SFA=survey.2016$SFA,STD.CATCH=survey.2016$STD_CATCH)
#head(survey.lyear)

survey.lyear = survey.1984[,c("SFA","STD_CATCH")]


#survey.2017<-subset(shrimp.surv2,YEAR==2017)
survey.last.year<-ddply(survey.1984,.(YEAR,SFA),summarize,TOT_WEIGHT=sum(WEIGHT), TOT_CATCH=sum(STD_CATCH),
                        MEAN_CATCH=mean(STD_CATCH))
#head(survey.last.year)


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
summary(Shrimp.boot,CI.method = "Percentile",prints=T)
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

############################################### DAVE's NOTES #################################################

#CALC shrimp survey CPUE boostraps CI
#CALC shrimp survey swept area biomass with CI
#TABLE shrimp survey swept area biomass per SFA and total with CIs
#PLOT shrimp survey CPUE by stratum and overall
#PLOT shrimp survey catch map for year=t and t-1 with temperature

##SHRIMP IN SNOWCRAB SURVEY##
#CALC snowcrab survey CPUE (in strata or over all area?)
#CALC snowcrab survey CPUE with boostrap CIs
#PLOT snowcrab survey CPUE map with temperatures (snowcrab) for year = t and t-1 (perhaps more for first time?)
#PLOT Kennorm Fishery Independent Biomass Index



#newpopcm16.qry <- paste("select round(carlen/10) "CL(mm)",
#sum(decode(shrdetail.sfa,13,(1/totals.totnum)*weight*(1.25/dist)*(100000/avewt),0)) "SFA 13",
#sum(decode(shrdetail.sfa,14,(1/totals.totnum)*weight*(1.25/dist)*(100000/avewt),0)) "SFA 14",
#sum(decode(shrdetail.sfa,15,(1/totals.totnum)*weight*(1.25/dist)*(100000/avewt),0)) "SFA 15",
#sum(decode(shrdetail.sfa,17,(1/totals.totnum)*weight*(1.25/dist)*(100000/avewt),0)) "SFA 17"
#from shrdetail, shrsurvey, totals where
#shrdetail.bcode=shrsurvey.cruise
#and shrdetail.fdate=shrsurvey.fdate
#and shrdetail.sfa=shrsurvey.sfa
#and shrdetail.xset=shrsurvey.xset
#and totals.bcode=shrsurvey.cruise
#and totals.fdate=shrsurvey.fdate
#and totals.sfa=shrsurvey.sfa
#and totals.xset=shrsurvey.xset
#and shrsurvey.setcode in(1,2) and shrdetail.bcode='CK1601' 
#group by round(carlen/10)
#order by round(carlen/10)")

##########################################################################

#FISHERY INDEPENDENT DISPERSION

#Select valid survey sets only (SETCODE = 1 and 2) and drops one record in 1999 for SFA=18
vsurv.set<-subset(shrimp.surv2,SETCODE %in% c(1,2) & SFA<18)
#CALC shrimp survey CV by SFA (consider snowcrab CV?)
svyCVsfa<-ddply(vsurv.set,.(YEAR,CRUISE,SFA),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
head(svyCVsfa) 
#YEAR CRUISE SFA COEF_VAR
#1 1982   P270  13    71.69
#2 1982   P270  14    56.54
#3 1982   P270  15    37.28
#4 1982   P270  17     0.00
#5 1982   P281  13    77.07
#6 1982   P281  14    51.37

#CALC shrimp survey CV by YEAR and SFA (consider snowcrab CV?)

ann.svyCV<-ddply(vsurv.set,.(YEAR),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
# These values are to be added to the ess_20XX spreadsheet
head(ann.svyCV) 
#  YEAR COEF_VAR
#1 1982    89.06
#2 1983    78.52
#3 1984    75.84
#4 1985    83.09
#5 1986   106.13
#6 1987    67.53

#Adjustments for plotting the values
#Add missing years to data set:
svyCVsfa2<-merge(as.data.frame(YEAR=rep(seq(1995:2017),4)),svyCVsfa, all=T)

#PLOT survey CV by area and overall
svyCVsfa$GRP=NA
val1 <- which(svyCVsfa$YEAR<1989)
svyCVsfa[val1,"GRP"]=1
val2<- which(svyCVsfa$YEAR>1992)
svyCVsfa[val2,"GRP"]=2
val3<- which(svyCVsfa$YEAR>1993 & svyCVsfa$YEAR<1995)
svyCVsfa[val3,"GRP"]=3

ggplot(svyCVsfa,aes(factor(YEAR),COEF_VAR),group=YEAR)	+ geom_point() + geom_line() + facet_wrap(~SFA) + 
  theme(axis.text.x=element_text(angle=90))

ggplot(svyCVyear,aes(YEAR,COEF_VAR))	+ 
  geom_rect(aes(xmin = 1982, xmax = 2016,ymin = 50, ymax = 70), fill = "red", alpha = 0.02) +
  geom_rect(aes(xmin = 1982, xmax = 2016,ymin = 70.1, ymax = 95), fill = "yellow", alpha = 0.02) + 
  geom_rect(aes(xmin = 1982, xmax = 2016,ymin = 95.1, ymax = 130), fill = "green", alpha = 0.02) +
  geom_point() + geom_line() + scale_x_continuous(breaks=seq(min(svyCVyear$YEAR), max(svyCVyear$YEAR), 2))


################################################################################################################
########################################### Commercial CPUEs ###################################################
## Undstandardized trawl CPUE
#EFFORT quantified by hours - convert hours and minutes to hours and decimal hours
#CALC unstand CPUE by stratum
#PLOT unstand CPUE by stratum
#PLOT unstand CPUE boxplots (overall, by SFA, by month/depletion)

#Calculate Commercial CPUE:
shrimp.db('ComLogs.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('ComLogs', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.COMLOG)

#'data.frame':	47590 obs. of  19 variables:
#$ BCODE  : int  100785 100785 100787 100787 100787 100787 100787 100787 100787 100787 ...
#$ BTYPE  : int  1 1 1 1 1 1 1 1 1 1 ...
#$ LICENCE: int  NA NA NA NA NA NA NA NA NA NA ...
#$ XSET   : int  NA NA NA NA NA NA NA NA NA NA ...
#$ FDATE  : POSIXct, format: "1993-05-17" "1993-05-18" "1993-04-23" "1993-04-26" ...
#$ LDATE  : POSIXct, format: "1946-12-24" "1946-12-24" "1946-12-24" "1946-12-24" ...
#$ BLAT   : int  444437 445659 443823 444337 444440 443911 444504 444035 443755 444925 ...
#$ BLONG  : int  592747 593156 601720 602124 602029 595934 600910 601953 601732 602101 ...
#$ ELAT   : int  NA NA NA NA NA NA NA NA NA NA ...
#$ ELONG  : int  NA NA NA NA NA NA NA NA NA NA ...
#$ SFA    : int  14 14 15 15 15 14 15 15 15 15 ...
#$ DCODE  : int  NA NA NA NA NA NA NA NA NA NA ...
#$ DEPTH  : int  NA NA NA NA NA NA NA NA NA NA ...
#$ FHOURS : int  1800 1800 800 1900 700 2000 2000 1600 2200 1100 ...
#$ WEIGHT : int  2023 1839 157 1520 301 1709 1495 1495 2563 1709 ...
#$ VALUE  : int  2676 2432 204 1977 392 2223 1945 1945 3334 2223 ...
#$ XCOUNT : int  NA NA NA NA NA NA NA NA NA NA ...
#$ NTRAPS : int  NA NA NA NA NA NA NA NA NA NA ...
#$ YEAR   : num  1993 1993 1993 1993 1993 ...
head(shrimp.COMLOG)
#   BCODE BTYPE LICENCE XSET      FDATE      LDATE   BLAT  BLONG ELAT ELONG SFA DCODE DEPTH FHOURS WEIGHT VALUE XCOUNT NTRAPS YEAR
#1 100785     1      NA   NA 1993-05-17 1946-12-24 444437 592747   NA    NA  14    NA    NA   1800   2023  2676     NA     NA 1993
#2 100785     1      NA   NA 1993-05-18 1946-12-24 445659 593156   NA    NA  14    NA    NA   1800   1839  2432     NA     NA 1993
#3 100787     1      NA   NA 1993-04-23 1946-12-24 443823 601720   NA    NA  15    NA    NA    800    157   204     NA     NA 1993
#4 100787     1      NA   NA 1993-04-26 1946-12-24 444337 602124   NA    NA  15    NA    NA   1900   1520  1977     NA     NA 1993
#5 100787     1      NA   NA 1993-04-27 1946-12-24 444440 602029   NA    NA  15    NA    NA    700    301   392     NA     NA 1993
#6 100787     1      NA   NA 1993-05-01 1946-12-24 443911 595934   NA    NA  14    NA    NA   2000   1709  2223     NA     NA 1993

#Gulf Commercial Landings only:
Gulf.comlog<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE==3, na.rm=T)
#Calculate annual CPUE (Kg/Hr):
#Converts effort from FHOURS to HOURS and DECIMAL HOURS
Gulf.cpue<-ddply(Gulf.comlog,.(YEAR),summarize,GULF_CPUE=mean(WEIGHT)/mean(trunc(FHOURS/100)+((FHOURS/100)-trunc(FHOURS/100))/0.6))
Gulf.cpue
#   YEAR GULF_CPUE
#1  1993     187.9
#2  1994     213.5
#3  1995     187.0
#4  1996     244.6
#5  1997     236.3
#6  1998     343.7
#7  1999     395.7
#8  2000     383.7
#9  2001     428.2
#10 2002     572.4
#11 2003     675.4
#12 2004     793.1
#13 2005     683.3
#14 2006     716.4
#15 2007     696.6
#16 2008     664.1
#17 2009     648.8
#18 2010     536.2
#19 2011     671.2
#20 2012     520.9
#21 2013     626.7
#22 2014     418.7
#23 2015     571.0
#24 2016     547.8

# These Gulf CPUE values are in ess_2016 spreadsheet
# PLOT Gulf CPUE:
jpeg(filename="gulf_cpue.jpg")
ry<-quantile(Gulf.cpue$GULF_CPUE[Gulf.cpue$YEAR>1999&Gulf.cpue$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Gulf.cpue$GULF_CPUE[Gulf.cpue$YEAR>1999&Gulf.cpue$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(Gulf.cpue, aes(YEAR,GULF_CPUE)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =595.4 , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 680.1, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(Gulf.cpue$YEAR), max(Gulf.cpue$YEAR), 2)) +
  scale_y_continuous(name="Gulf cpue (kg/hr)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off()    
#############################################################################################################################
#UNSTANDARDIZED TRAWL CPUE:
#CPUE CALCULATIONS:
head(shrimp.COMLOG) #47,590 RECORDS

#Calculate unstandardized annual CPUE (Kg/Hr):
#Converts effort from FHOURS to HOURS and DECIMAL HOURS
shrimp.COMLOG$CPUE<-(shrimp.COMLOG$WEIGHT/((trunc(shrimp.COMLOG$FHOURS/100)+((shrimp.COMLOG$FHOURS/100)-trunc(shrimp.COMLOG$FHOURS/100))/0.6)))
table(shrimp.COMLOG$BTYPE,shrimp.COMLOG$SFA)

# OFFSHORE SELECTION:
off.comlog<-subset(shrimp.COMLOG, !(BLAT>451000 & BLONG>592000) & SFA!=16 & WEIGHT>0 & FHOURS>0 & BTYPE<4 &
                 !(BCODE %in% c(5631,102680)), na.rm=T)
off.comlog$STRATUM<-off.comlog$SFA
offshore.catch<-ddply(off.comlog,.(YEAR,STRATUM),summarize,CATCH_MT=round(sum(WEIGHT/1000)),EFFORT_HR=sum(FHOURS))
offshore.cpue<-ddply(off.comlog,.(YEAR,STRATUM),summarize,CATCH_MT=sum(WEIGHT/1000),AVG_CPUE=mean(CPUE))
# INSHORE SELECTION:
ins.comlog<-subset(shrimp.COMLOG, BLAT>451000 & BLONG>592000 & WEIGHT>0 & FHOURS>0 & BTYPE<4, na.rm=T)
ins.comlog$STRATUM<-17
inshore.catch<-ddply(ins.comlog,.(YEAR,SFA),summarize,CATCH_MT=round(sum(WEIGHT/1000)),EFFORT_HR=sum(FHOURS))
inshore.cpue<-ddply(ins.comlog,.(YEAR,STRATUM),summarize,AVG_CPUE=mean(CPUE))
inshore.cpue

#TRAP SELECTION:
trap.comlog<-subset(shrimp.COMLOG, BTYPE==4)
#Trapping occurs in 2 SFAs, SFGA 16 and 17
#table(trap.comlog$SFA)
#16    17 
#1027 10256 
#Trap catches in the past seem to include SFA 17 only
trap.SFA17<-subset(trap.comlog, SFA==17)
#Trap catches in the past excluded records with FHOURS==0
table(trap.SFA17$FHOURS) #3 records with 0 hours (2014-15), but WEIGHT and NTRAPS - I left in
trap.catch<-ddply(trap.SFA17,.(YEAR,SFA),summarize,CATCH_MT=round(sum(WEIGHT)),EFFORT_TP=sum(NTRAPS,na.rm=T))
trap.cpue<-ddply(trap.catch,.(YEAR,SFA),summarize,AVG_CPUE=CATCH_MT/EFFORT_TP)
trap.cpue

jpeg(filename="trap_cpue.jpg")
ry<-quantile(trap.cpue$AVG_CPUE[trap.cpue$YEAR>1999&trap.cpue$YEAR<2011], probs=.33, na.rm=TRUE)
yg<-quantile(trap.cpue$AVG_CPUE[trap.cpue$YEAR>1999&trap.cpue$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(trap.cpue, aes(YEAR,AVG_CPUE)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = ry), fill = "red", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =ry , ymax = yg), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = yg, ymax = Inf), fill = "green", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(trap.cpue$YEAR), max(trap.cpue$YEAR), 2)) +
  scale_y_continuous(name="Trap cpue (kg/trap haul)") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off()    

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
                                
#MEAN ANNUAL CATCH ACROSS SFAs
str(comlog_4strat.cpue)
comlog_4strat.cpue$MEAN<-rowMeans(x = comlog_4strat.cpue[,2:ncol(comlog_4strat.cpue)], na.rm = TRUE)
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
head(shrimp.COMLOG) #47,590 RECORDS
#Select for weights and hours not equal to zero and boat types from NS fleet only (1 & 2)
comlog.clean<-subset(shrimp.COMLOG, WEIGHT>0 & FHOURS>0 & BTYPE<3, na.rm=T)
str(comlog.clean) #27,024 RECORDS

comlog.dat<-comlog.clean[,c("BCODE", "FDATE", "YEAR", "MONTH", "SFA", "FHOURS", "WEIGHT", "CPUE")]
head(comlog.dat)

#First, select April-July inclusive (months 4 to 7)
month.filter<-comlog.dat[comlog.dat$MONTH>3 & comlog.dat$MONTH<8,]
dim(month.filter)#18,929 RECORDS

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
ggplot(subset(month.filter, YEAR==2016),aes(MONTH,CPUE, group=MONTH)) + geom_boxplot() + theme_bw() +
  scale_x_continuous(name="MONTH", breaks=seq(4,7,1)) +
  scale_y_continuous(name="Unstandardized CPUE (kg/hr)", breaks=seq(0,2000,500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#Second, include only boats that have fished for 7 years or more
fishing.years = ddply(month.filter,.(BCODE), summarize, F.YR=length(unique(YEAR)))
year.7plus.filter<-subset(fishing.years, F.YR>6)
fish.filter<-subset(month.filter,BCODE %in% year.7plus.filter$BCODE)
dim(fish.filter)#13,694 RECORDS

#Finally, remove Final Venture and Island Provider, which are bcodes -'5631', '102680', the factory trawlers
boat.filter<-subset(fish.filter, BCODE!=5631 & BCODE!=102680)

#Create a filtered file containing unstandardized CPUE
write.table(boat.filter,file="filtered_cpue.2016.txt") 
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
highliner.id<-ddply(subset(filtered.cpue,YEAR==2016),.(BCODE), summarize, MEAN_CPUE=mean(UNSTD_CPUE))

#HARD CODED  - 2016 - FIXING BCODE AT MEAN OF HIGHLINER - 104885, MONTH AS MEAN IN JULY, SFA AS MEAN IN SFA13
pred.val = data.frame(BCODE=as.factor(104885), YEAR=sort(unique(filtered.cpue$YEAR)), MONTH=as.factor(7), SFA=as.factor(13))

#Run the GLM (M1) on newdata (predict.val)
cpue.predict = predict( M1, newdata=pred.val, type = c("response"), se.fit=T)
pred.val$cpue = cpue.predict $fit
pred.val$cpue_se = cpue.predict $se.fit

#Define the upper and lower CPUE bounds
pred.val$u.bound = (pred.val$cpue + 2*pred.val$cpue_se)
pred.val$l.bound = (pred.val$cpue - 2*pred.val$cpue_se)

write.table(predict.val,file="Stand.Com.CPUE.txt") 

#Plot Survey, standardized and unstanderdized CPUE when loading the ESS table

###################################################### Summer RV CPUE #######################################################
require(bio.datawrangling)
setwd("C:\\Users\\cassistadarosm\\Documents\\GitHub\\bio_data\\bio.datawrangling")
get_data('rv',fn.oracle.dsn="PTRAN")
#Data loaded:
#Loaded RV.GSCAT...  (Data modified 0 days ago.) - Catch Information
#Loaded RV.GSINF...  (Data modified 0 days ago.) _ Tow Information
#Loaded RV.GSDET...  (Data modified 0 days ago.) - Detail Information
#Loaded RV.GSMISSIONS...  (Data modified 0 days ago.) - Mission Information
#Loaded RV.GSSTRATUM...  (Data modified 0 days ago.) - Stratum Information
#Loaded RV.GSXTYPE...  (Data modified 0 days ago.) - Set Type Information
#Loaded RV.GSSPECIES...  (Data modified 0 days ago.) - Species Information
#Loaded RV.FGP_TOWS_NW2...  (Data modified 0 days ago.) - Don's Species List for Standardized Values

#Select records where tow was successful and within informative strata
GSINF=GSINF[GSINF$TYPE==1 & GSINF$STRAT %in% c(443,444,445,459),]
#Select 'Summer' missions only and within a specified year range:
GSMISSIONS=GSMISSIONS[GSMISSIONS$SEASON=="SUMMER" & GSMISSIONS$YEAR %in% c(1982:2016),]
self_filter()

shrimp.rv.recs = merge(GSCAT[GSCAT$SPEC==2211,], GSINF, all.y=T, by.x=c("MISSION","SETNO"), by.y = c("MISSION","SETNO"))
drops <- c("REMARKS.x","REMARKS.y")
rv.recs.8216<-shrimp.rv.recs[ , !(names(shrimp) %in% drops)]
rv.recs.8216$RATIO=1.75/rv.recs.8216$DIST
rv.recs.8216$STDWGT=rv.recs.8216$TOTWGT*rv.recs.8216$RATIO
rv.recs.8216$STDNO=rv.recs.8216$TOTNO*rv.recs.8216$RATIO
rv.recs.8216$YEAR=year(rv.recs.8216$SDATE)

test=rv.recs.8216[,c("YEAR", "STRAT","MISSION","SETNO","TOTNO","TOTWGT","STDNO","STDWGT")]





#################################################### ESS SHRIMP SIZE INDICES ################################################
shrimp.db('Details.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('Details', oracle.username=oracle.username, oracle.password = oracle.password)
head(shrimp.DETAILS)
#write.csv(shrimp.DETAILS,paste("I:/Offline Data Files/Shrimp/Detail.Data",Sys.Date(),".csv",sep=""), row.names=F)
pdf('Shrimp Indices.pdf',width=8,height=10)
####################################################### Commercial Counts ###################################################
head(shrimp.COMLOG) #47,590 RECORDS
table(shrimp.COMLOG$YEAR)
#1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
#1082 1220 1184 1605 2151 1716 1720 1807 2049 1851 2144 1799 1949 2193 2265 2086 1628 2552 2400 2579 2143 2794 2866 1807

com.count<-subset(shrimp.COMLOG,BTYPE<4 & XCOUNT!=0)
ann.count.dat<-ddply(com.count,.(YEAR),summarize,MEAN=mean(XCOUNT,na.rm=T),STD.ERR=sd(XCOUNT/sqrt(sum(!is.na(XCOUNT)))),
                     MAR.ERR=STD.ERR*2,UPP_BOUND=MEAN+MAR.ERR,LOW_BOUND=MEAN-MAR.ERR)

ggplot(ann.count.dat,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2016,3)) + facet_wrap(~SFA) +
  scale_y_continuous(limits=c(20,75), name="Mean Commercial Count") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#For SAR select time series from 1995 to now
#Insert in ESS_20XX spreadsheet
count.dat<-subset(ann.count.dat,YEAR>1994)

p1<-ggplot(count.dat,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2016,3)) 
  scale_y_continuous(limits=c(52,66), name="Mean Commercial Count") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 1994.5, y = 66, label = "A") 

############################################### Mean Max Survey Shrimp Length ###############################################
head(shrimp.DETAILS)
#Select survey data only
size.select<-subset(shrimp.DETAILS, GEAR==4)
#Calculate max shrimp size per set
max.size<-ddply(size.select,.(BCODE,YEAR,SFA,XSET),summarize,MAX.LEN=max(CARLEN/10))
#Drop single record from 1946
table(max.size$YEAR)
1946 1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 
   1   59   41   61   55   54   55   63   40   48   57   60   58   98   59   65   57   65   60   58   60   59 
2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 
  60   60   60   59   60   60   60   60   60   60 

surv.max.len<-subset(max.size, !YEAR==1946)

#Calculate mean and standard error on the max length
surv.size<-ddply(surv.max.len,.(YEAR),summarize,MEAN=mean(MAX.LEN,na.rm=T),STD.ERR=sd(MAX.LEN, na.rm=T)/
                   sqrt(sum(!is.na(MAX.LEN))),
                 MAR.ERR=STD.ERR*2,UPP_BOUND=MEAN+MAR.ERR,LOW_BOUND=MEAN-MAR.ERR)

#For SAR select time series from 1995 to now
#Insert in ESS_20XX spreadsheet
max.size.dat<-subset(surv.size,YEAR>1994)

p2<-ggplot(max.size.dat,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2017,3)) +
  scale_y_continuous(limits=c(28,31), name="Mean Max Length (mm)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 1994.5, y = 31, label = "B") 

################################################ Mean Female Commercial Size ################################################
head(shrimp.DETAILS)
#Select commercial trawl and female shrimp
fem.select<-subset(shrimp.DETAILS, SEX>4 & GEAR==1)
str(fem.select)
#Calculate mean female shrimp size per set
mean.fem.size<-ddply(fem.select,.(BCODE,FDATE,YEAR,SFA,XSET),summarize,MEAN_SIZE=mean(CARLEN/10))

#Insert in ESS_20XX spreadsheet
ann.fem.size<-ddply(mean.fem.size,.(YEAR), summarize,MEAN=mean(MEAN_SIZE,na.rm=T),STD.ERR=sd(MEAN_SIZE,na.rm=T)/sqrt(sum(!is.na(MEAN_SIZE))),
                 MAR.ERR=STD.ERR*2,UPP_BOUND=MEAN+MAR.ERR,LOW_BOUND=MEAN-MAR.ERR)

p3<-ggplot(ann.fem.size,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2016,3)) +
  scale_y_continuous(limits=c(25,27), name="Mean Female Length (mm)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 1994.5, y = 27, label = "C") 

############################################## Mean Size Sex Transition from Survey #########################################
#MILLIM VIEW QUERY:
shrimp.db('MILLIM.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('MILLIM', oracle.username=oracle.username, oracle.password = oracle.password)
str(MILLIM.VIEW)
#write.csv(MILLIM.VIEW,paste("I:/Offline Data Files/Shrimp/MillimView.Data",Sys.Date(),".csv",sep=""), row.names=F)

#Filter data to select survey trawl data only, transitional individuals only and exclude YEAR 1946
survey.trans=MILLIM.VIEW[MILLIM.VIEW$GEAR==4 & MILLIM.VIEW$YEAR !=1946 & (MILLIM.VIEW$TRAN ==1 | MILLIM.VIEW$PRIMNET==1),]
str(survey.trans)
#Calculate mean and standard error on the mean size at sex transition from survey
#Mean weighted by survey set
surv.set.mean<-ddply(survey.trans,.(BCODE,FDATE,YEAR,SFA,XSET), summarize,MEAN_TRANS=mean(CARLEN/10,na.rm=T))
ann.trans<-ddply(surv.set.mean,.(YEAR), summarize,MEAN=mean(MEAN_TRANS,na.rm=T),STD.ERR=sd(MEAN_TRANS,na.rm=T)/sqrt(sum(!is.na(MEAN_TRANS))),
                MAR.ERR=STD.ERR*2,UPP_BOUND=MEAN+MAR.ERR,LOW_BOUND=MEAN-MAR.ERR)

#For SAR select time series from 1995 to now
#Insert in ESS_20XX spreadsheet
trans.dat<-subset(ann.trans,YEAR>1994)

p4<-ggplot(trans.dat,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2016,3)) +
  scale_y_continuous(limits=c(23,26), name="Mean Length at Sex Transition (mm)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 1994.5, y = 26, label = "D") 

grid.arrange(p1,p2,p3,p4,ncol=2)  
dev.off()

###################################################### Spawning Stock Biomass ###############################################
### SSB - density of females in the survey catch
#Use data in table SHRSURVEY and views: TOTALS and TOTALSFEMTRAN, all in ORACLE
str(shrimp.survey)

#Survey data selection of SETCODE= 1 and 2, random stratified and fixed respectively
surv.data<-subset(shrimp.survey, SETCODE<3) # 68 records dropped
str(surv.data)

#TOTALS View Query:
shrimp.db('TOTALS.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALS', oracle.username=oracle.username, oracle.password = oracle.password)
head(TOTALS.VIEW) # AVEWT = average individual weight, and CTLB = count/lb
#   BCODE GEAR      FDATE SFA XSET TOTNUM TOTWT  AVEWT  CTLB YEAR MONTH
#1 100043    2 1994-11-26  17    1    359  3385  942.9 48.12 1994    11
#2 100043    2 1995-01-19  17    5    194  2059 1061.1 42.76 1995     1
#3 100043    2 1995-04-30  17    1    408  4068  997.0 45.51 1995     4
#4 CK9501    4 1995-06-03  14   23    466  2798  600.4 75.57 1995     6
#5 AC9301    4 1993-08-18  14    7    317  2803  884.1 51.32 1993     8
#6 AC9301    4 1993-08-17  14   12    254  2660 1047.4 43.32 1993     8

#write.csv(TOTALS.VIEW,paste("I:/Offline Data Files/Shrimp/TOTALS.VIEW.Data",Sys.Date(),".csv",sep=""), row.names=F)

#Select survey data only:
surv.totals<-subset(TOTALS.VIEW, GEAR==4)

#TOTALSFEMTRAN View Query:
shrimp.db('TOTALSFEMTRAN.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('TOTALSFEMTRAN', oracle.username=oracle.username, oracle.password = oracle.password)
head(TOTALSFEMTRAN.VIEW)
#   BCODE GEAR      FDATE SFA XSET TOTNUM TOTWT  AVEWT YEAR MONTH
#1 100043    2 1994-11-26  17    1    317  3127  986.3 1994    11
#2 100043    2 1994-11-30  17    1    236  2216  939.0 1994    11
#3 100043    2 1995-01-19  17    5    179  1964 1097.0 1995     1
#4 100043    2 1995-04-30  17    1    377  3858 1023.4 1995     4
#5 100043    2 1995-04-13  17    1    425  4344 1022.1 1995     4
#6 101609    1 1995-04-11  15    1     77   919 1193.5 1995     4

#write.csv(TOTALSFEMTRAN.VIEW,paste("I:/Offline Data Files/Shrimp/TOTALSFEMTRAN.Data",Sys.Date(),".csv",sep=""), row.names=F)

#Select survey data only in:
surv.totalsfemtran<-subset(TOTALSFEMTRAN.VIEW, GEAR==4)

#Annual SSB
survey.dens<-ddply(surv.data,.(YEAR,CRUISE,SFA,XSET),summarize, AVG.DEN=mean((WEIGHT*1000/(DIST*1852*17.705)), na.rm=T))
fem.proportion<-ddply(surv.totalsfemtran,.(YEAR,BCODE,SFA,XSET),summarize,FEM.WGT=sum(TOTWT,na.rm=T))
all.catch<-ddply(surv.totals,.(YEAR,BCODE,SFA,XSET),summarize,TOT.WGT=sum(TOTWT, na.rm=T))
survey.fem.ratio<-merge(fem.proportion,all.catch,by=c('YEAR','BCODE','SFA','XSET'))
survey.fem.ratio<-rename.vars(survey.fem.ratio, from="BCODE",to="CRUISE")
survey.fem.ratio$FEM.PROP<-survey.fem.ratio$FEM.WGT/survey.fem.ratio$TOT.WGT
str(survey.fem.ratio)
str(survey.dens)
survey.SSB<-merge(survey.fem.ratio,survey.dens,by=c('YEAR','CRUISE','SFA','XSET'))
survey.SSB$FEM.DEN<-survey.SSB$AVG.DEN * survey.SSB$FEM.PROP
avg.surv.ssb<-ddply(survey.SSB,.(YEAR),summarize,AVG.SSB=mean(FEM.DEN,na.rm=T))

#Select survey trawl and female shrimp
survey.dens<-ddply(surv.data,.(CRUISE,SFA,XSET),summarize, AVG.DEN=mean((WEIGHT*1000/(DIST*1852*17.705)), na.rm=T))
fem.proportion<-ddply(surv.totalsfemtran,.(BCODE,SFA,XSET),summarize,FEM.WGT=sum(TOTWT,na.rm=T))
all.catch<-ddply(surv.totals,.(BCODE,SFA,XSET),summarize,TOT.WGT=sum(TOTWT, na.rm=T))
survey.fem.ratio<-merge(fem.proportion,all.catch,by=c('BCODE','SFA','XSET'))
survey.fem.ratio<-rename.vars(survey.fem.ratio, from="BCODE",to="CRUISE")
survey.fem.ratio$FEM.PROP<-survey.fem.ratio$FEM.WGT/survey.fem.ratio$TOT.WGT
str(survey.fem.ratio)
str(survey.dens)
survey.SSB<-merge(survey.fem.ratio,survey.dens,by=c('CRUISE','SFA','XSET'))
survey.SSB$FEM.DEN<-survey.SSB$AVG.DEN * survey.SSB$FEM.PROP
avg.surv.ssb<-ddply(survey.SSB,.(CRUISE,SFA),summarize,AVG.SSB=mean(FEM.DEN,na.rm=T))

#Spawning Stock Biomass per SFA area:
#SFA AREA in km square:
area.km2<-c(1620,1517,948,1415)
#Select survey density to look at:
density<-avg.surv.ssb$AVG.SSB[avg.surv.ssb$CRUISE=='CK1601'] # 2016
ssb<-sum(area.km2*density)

###################################################  Percent of Females/Year ################################################

ann.surv.dens<-ddply(surv.data,.(YEAR,SFA),summarize, AVG.DEN=mean((WEIGHT*1000/(DIST*1852*17.705)), na.rm=T))
ann.fem.prop<-ddply(surv.totalsfemtran,.(YEAR,SFA),summarize,FEM.WGT=sum(TOTWT,na.rm=T))
ann.catch<-ddply(surv.totals,.(YEAR,SFA),summarize,TOT.WGT=sum(TOTWT, na.rm=T))
surv.fem.ratio<-merge(ann.fem.prop,ann.catch,by=c('YEAR','SFA'))
surv.fem.ratio$FEM.PROP<-surv.fem.ratio$FEM.WGT/surv.fem.ratio$TOT.WGT
str(surv.fem.ratio)
ann.fem<-subset(surv.fem.ratio,YEAR>1993)

ann.fem.prop<-ddply(ann.fem,.(YEAR), summarize,MEAN=mean(FEM.PROP,na.rm=T),STD.ERR=sd(FEM.PROP,na.rm=T)/sqrt(sum(!is.na(FEM.PROP))),
                    MAR.ERR=STD.ERR*2,UPP_BOUND=MEAN+MAR.ERR,LOW_BOUND=MEAN-MAR.ERR)

ggplot(ann.fem.prop,aes(YEAR,MEAN)) + geom_errorbar(aes(ymin=LOW_BOUND, ymax=UPP_BOUND)) + geom_line() + geom_point() + 
  scale_x_continuous(name="Year", breaks= seq(1995,2016,3)) +
  scale_y_continuous(limits=c(0.30,0.90), name="Mean Female Proportion") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 1996, y = 0.9, label = "Survey SSB") 

###_______________________________________________________________________________________________________________________###

#Plot Standandarzed CPUE by stratum

## Trap CPUE
#CALC trap CPUE

##Fishery Dependent Dispersion
#CALC areas of catch rates >250kg/h (and others)
#PLOT areas of catch rates <150, 150-250, 250-350, 350-450, >450 (or other more suitable)
#PLOT areas of catch rates >450, >350, >250, >150

################# Abundance Indices overall KenNorm #####################

#Use the mogh code in assess2015 for this to build on ess.csv for this and for Jess's effort plots above.
paste(direct,"/R/course_lib.r")
prog_id = "agg_dat"

aggd_shrimp<-read.csv("data/ess_2016.csv",header = TRUE)
head(aggd_shrimp)

trv  = cbind(ken_norm(aggd_shrimp$RV_CPUE),
             ken_norm(aggd_shrimp$G_CPUE),
             ken_norm(aggd_shrimp$St_CPUE),
             ken_norm(aggd_shrimp$Trap_CPTH))
lines(aggd_shrimp$yr, rowMeans(trv,na.rm=T),col=1,lwd = 2)

png(filename="KenNorm_2016_Surv_Gulf_Stand_Trap.jpg")

plot(aggd_shrimp$yr,ken_norm(aggd_shrimp$RV_CPUE),type = "l",pch=1,col="red",xlim=c(1982,2016),ylab="Normalized CPUEs (unitless)",xlab="", lwd=2,cex.axis = 1,cex.lab=1)
lines(aggd_shrimp$yr[aggd_shrimp$G_CPUE >= 0],ken_norm(aggd_shrimp$G_CPUE[aggd_shrimp$G_CPUE >= 0]),col="darkgreen",lwd = 2)
lines(aggd_shrimp$yr,ken_norm(aggd_shrimp$St_CPUE),col="blue",lwd = 2)
lines(aggd_shrimp$yr,ken_norm(aggd_shrimp$Trap_CPTH),col="orange",lwd = 2)

trv  = cbind(ken_norm(aggd_shrimp$RV_CPUE),
             ken_norm(aggd_shrimp$G_CPUE),
             ken_norm(aggd_shrimp$St_CPUE),
             ken_norm(aggd_shrimp$Trap_CPTH))
lines(aggd_shrimp$yr, rowMeans(trv,na.rm=T),col="black",lwd = 5)
surveytxt<-c("Survey CPUE")
gulftxt<-c("Gulf CPUE")
standtxt<-c("Stand. CPUE")
traptxt<-c("Trap CPTH")
meantxt<-c("Mean CPUE")
legend(x=c(1990), y=c(1.5), surveytxt, text.col="red", bty="n", xjust=1)
legend(x=c(1990), y=c(1.3), gulftxt, text.col="darkgreen", bty="n", xjust=1)	
legend(x=c(1990), y=c(1.1), standtxt, text.col="blue", bty="n", xjust=1)
legend(x=c(1990), y=c(0.9), traptxt, text.col="orange", bty="n", xjust=1)	
legend(x=c(1990), y=c(0.7), meantxt, text.col="black", bty="n", xjust=1)

dev.off()


################## Production #############################################
#Survey Age 1 Abundance:
#Use shrimp.juv table which provides details of bellybag samples, sampling year range 2002 to now
#The shrimp.surv2 data is the survey data that excludes unsuccessful tows and comparative 
#work - fixed and random tows, selected years>1994. Standardized catch and density calculated.
str(shrimp.Juv)
bb.detail<-shrimp.Juv[,c(1,5,6,9,15:17)]
str(shrimp.surv2) 
surv.0217<-subset(shrimp.surv2,YEAR>2001)
surv.detail<-surv.0217[,c(1:3,7,18,24,31:35)]
surv.detail<-rename.vars(surv.detail, from="CRUISE",to="BCODE")
bb.calc<-merge(surv.detail,bb.detail, by= c("BCODE","XSET","SFA"))
bb.calc$SA.ABUND<-bb.calc$XCOUNT*bb.calc$RATIO*(1.25/bb.calc$DIST)
bb.calc$CARLEN<-round(bb.calc$CARLEN)
#Total number of XSET per SFA:
tot.set<-ddply(bb.calc,.(YEAR,SFA),summarize,NUM_TOW=length(unique(XSET)))
set.table<-reshape(tot.set,v.names="NUM_TOW",idvar="YEAR",timevar="SFA", direction='wide')

#CREATE Age by SFA abundance table:
SFAarea.dat<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio_data/bio.shrimp/data/Inputs/SFA.area.csv")
str(bb.calc)
belly.freq<-ddply(bb.calc,.(YEAR,SFA,CARLEN),summarize,ABUND=XCOUNT*RATIO*1.25/DIST)
table.edit<-bb.calc[,c(3,9,12,16)]
y=unique(bb.calc$YEAR)
for(i in y){
  bb.table<-reshape(a,v.names="SA.ABUND",idvar="CARLEN",timevar="SFA", direction='wide')
}
print(bb.table)

for()
bb.table<-reshape(bb.calc,v.names="SA.ABUND",idvar="YEAR",timevar="SFA", direction='wide')
comlog_4strat.cpue<-cpue.table[order(cpue.table$YEAR),]

#CALC Bellybag ############ HARD CODED ##################  

##### NOTe: Query would not run in SQLDeveloper. Michele ran and added output to 2016_data....read in csv below. 

belly.qry<-paste("SELECT carlen, 
                 sum(decode(J.sfa,13,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,14,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,15,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,17,(J.xcount*ratio*1.25/dist))),
                 FROM shrjuv AS J, shrsurvey AS S,
                 WHERE J.bcode=s.cruise
                 AND J.sfa = s.sfa
                 and J.xset = s.xset
                 and s.setcode in(1,2) 
                 and J.bcode='CK1601'
                 group by round(carlen)
                 order by round(carlen)")

### Original ###
#belly.qry<-paste("select round(carlen),
#sum(decode(J.sfa,13,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,14,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,15,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,17,(J.xcount*ratio*1.25/dist)))
#from shrjuv J, shrsurvey S
#where J.bcode=s.cruise
#and J.sfa=s.sfa
#and J.xset=s.xset
#and s.setcode in(1,2) and J.bcode='CK1601'
#group by round(carlen)
#order by round(carlen)",sep="")	


belly.dat<-sqlQuery(ch,belly.qry)
head(belly.dat)
colnames(belly.dat)<-c("CarLen(mm)", "Strat13", "Strat14", "Strat15", "Strat17")
head(belly.dat) #works	



###Manual read in: 
belly.dat<-read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/shrjuvsfa.csv")

surv.juv<-shrimp.Juv[,-c(2,3,4,7,8,10,11,12,13,14,15,18,19),]
surv.set<-shrimp.survey[,-c(4:27,30,33)]
summary(shrimp.Juv)

head(shrimp.survey)

#Survey Age 2 Abundance:
#Use shrimp.detail table which provides details of survey main catch samples, sampling year range 2002 to now
#The shrimp.surv2 data is the survey data that excludes unsuccessful tows and comparative 
#work - fixed and random tows, selected years>1994. Standardized catch and density calculated.
str(shrimp.DETAILS)
str(TOTALS.VIEW)
subset(TOTALS.VIEW,GEAR==4)
mt.detail<-shrimp.DETAILS[,c(1,5,6,9:17)]
str(shrimp.surv2) 
surv.9517<-subset(shrimp.surv2,YEAR>1994)
surv.det<-surv.9517[,c(1:3,7,18,24,28,30:32,34,35)]
surv.det<-rename.vars(surv.det, from="CRUISE",to="BCODE")
mt.calc<-merge(surv.det,mt.detail, by= c("BCODE","XSET","SFA"))

mt.calc$SA.ABUND<-1/mt.calc$TOTNUM*mt.calc$WEIGHT*(1.25/mt.calc$DIST)
mt.calc$CARLEN<-round(mt.calc$CARLEN)
#Total number of XSET per SFA:
tot.set<-ddply(mt.calc,.(YEAR,SFA),summarize,NUM_TOW=length(unique(XSET)))
set.table<-reshape(tot.set,v.names="NUM_TOW",idvar="YEAR",timevar="SFA", direction='wide')

#CREATE Age by SFA abundance table:
SFAarea.dat<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio_data/bio.shrimp/data/Inputs/SFA.area.csv")
str(bb.calc)
belly.freq<-ddply(bb.calc,.(YEAR,SFA,CARLEN),summarize,ABUND=XCOUNT*RATIO*1.25/DIST)
table.edit<-bb.calc[,c(3,9,12,16)]
y=unique(bb.calc$YEAR)
for(i in y){
  bb.table<-reshape(a,v.names="SA.ABUND",idvar="CARLEN",timevar="SFA", direction='wide')
}
print(bb.table)

for()
  bb.table<-reshape(bb.calc,v.names="SA.ABUND",idvar="YEAR",timevar="SFA", direction='wide')
comlog_4strat.cpue<-cpue.table[order(cpue.table$YEAR),]

#CALC Bellybag ############ HARD CODED ##################  

##### NOTe: Query would not run in SQLDeveloper. Michele ran and added output to 2016_data....read in csv below. 

belly.qry<-paste("SELECT carlen, 
                 sum(decode(J.sfa,13,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,14,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,15,(J.xcount*ratio*1.25/dist))), 
                 sum(decode(J.sfa,17,(J.xcount*ratio*1.25/dist))),
                 FROM shrjuv AS J, shrsurvey AS S,
                 WHERE J.bcode=s.cruise
                 AND J.sfa = s.sfa
                 and J.xset = s.xset
                 and s.setcode in(1,2) 
                 and J.bcode='CK1601'
                 group by round(carlen)
                 order by round(carlen)")

### Original ###
#belly.qry<-paste("select round(carlen),
#sum(decode(J.sfa,13,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,14,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,15,(J.xcount*ratio*1.25/dist))),
#sum(decode(J.sfa,17,(J.xcount*ratio*1.25/dist)))
#from shrjuv J, shrsurvey S
#where J.bcode=s.cruise
#and J.sfa=s.sfa
#and J.xset=s.xset
#and s.setcode in(1,2) and J.bcode='CK1601'
#group by round(carlen)
#order by round(carlen)",sep="")	


belly.dat<-sqlQuery(ch,belly.qry)
head(belly.dat)
colnames(belly.dat)<-c("CarLen(mm)", "Strat13", "Strat14", "Strat15", "Strat17")
head(belly.dat) #works	



###Manual read in: 
belly.dat<-read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/shrjuvsfa.csv")

surv.juv<-shrimp.Juv[,-c(2,3,4,7,8,10,11,12,13,14,15,18,19),]
surv.set<-shrimp.survey[,-c(4:27,30,33)]
summary(shrimp.Juv)

head(shrimp.survey)


















