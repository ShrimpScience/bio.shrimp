#' @title ess_shrimp
#' @description Perform Shrimp Stock Assessment for the fishing season
#' @param size.range defines the minimum and maximum value and is a filter (default is 0, 220mm CW)
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the survey years to estimate
#' @param mnts months of the survey, defaults to the full year
#' @param bin.size aggregates the abundance into size bins (default is 5mm bins)
#' @param gear.type survey trawl net identification (default is 280 BALLOON)
#' @return data.frame of survey data called 'shrimp.survey'

#' @importFrom plyr dplyr
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @seealso \code{\link{template.function}}, \url{http://www.github.com/Beothuk/bio.template}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: April 3, 2017 
cat('mike')
### Required Packages
library("RODBC")
library("PBSmapping")
library("bio.survey")
library("bio.shrimp")
library("plyr")
library("lubridate")

################### Survey CPUE and Biomass ############################## 
#Survey data query:
shrimp.db('survey.redo')
shrimp.db('survey')
str(shrimp.survey)
#'data.frame':	47590 obs. of  19 variables:
str(shrimp.survey)
#'data.frame':	1832 obs. of  31 variables:
#CRUISE  : chr  "CK9501" "CK9501" "CK9501" "CK9501" ...
#XSET    : int  1 2 3 4 5 6 7 8 9 10 ...
#SFA     : int  17 17 17 17 17 17 13 13 13 13 ...
#STRATUM : logi  NA NA NA NA NA NA ...
#SETCODE : int  1 1 1 1 1 1 1 1 1 1 ...
#GEAR    : int  4 4 4 4 4 4 4 4 4 4 ...
#FDATE   : POSIXct, format: "1995-05-31" "1995-05-31" "1995-05-31" "1995-05-31" ...
#SPEED   : num  2.3 2.5 2.3 2.3 2.1 2.3 2.4 2.4 2.5 2.3 ...
#HOW_SP  : int  NA NA NA NA NA NA NA NA NA NA ...
#BLAT    : int  452176 452700 452402 453306 453423 453636 454161 454199 454782 454528 ...
#BLONG   : int  605822 604146 603250 602310 600581 593840 590383 585130 584847 583979 ...
#ELAT    : int  452171 452668 452416 453323 453454 453597 454233 454269 454662 454577 ...
#ELONG   : int  605657 604322 603081 602151 600435 593999 590214 584992 584824 584122 ...
#BTIME   : chr  "05:38" "08:37" "10:37" "13:18" ...
#ETIME   : chr  "06:08" "09:08" "11:07" "13:48" ...
#BEARING : int  110 110 115 85 90 275 80 76 190 310 ...
#DURATION: int  30 30 30 30 30 30 30 30 30 30 ...
#DIST    : num  1.16 1.25 1.2 1.13 1.1 1.22 1.3 1.22 1.23 1.14 ...
#HOW_DIST: int  1 1 1 1 1 1 1 1 1 1 ...
#H_HEIGHT: num  NA NA NA NA NA NA NA NA NA NA ...
#BDEPTH  : int  61 75 100 95 93 110 104 133 145 141 ...
#EDEPTH  : int  69 NA NA 103 98 99 103 141 145 152 ...
#ADEPTH  : int  65 75 100 99 96 105 104 137 145 147 ...
#TEMP    : num  0.2 0.7 0.9 0.8 0.7 1.4 1.9 2.4 2.6 2.7 ...
#WIND_DIR: int  315 293 293 NA 45 23 NA NA NA NA ...
#WIND_SP : int  10 5 5 0 10 10 0 0 0 0 ...
#WAVE_HT : int  0 1 2 0 1 1 1 1 1 1 ...
#WEIGHT  : int  90 31 40 681 211 16 2 127 172 403 ...
#XCOUNT  : int  71 75 78 84 91 88 45 63 75 78 ...
#WING    : num  16.4 20.7 15.9 15.1 14.8 ...
#YEAR    : num  1995 1995 1995 1995 1995 ...
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
#  61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58   60   60   60   60   60   60   60   60   60   60   60 


#Standardize catch to trawlable unit	
survey.2016<-subset(shrimp.survey,YEAR==2016)
survey.dat<-ddply(survey.2016,.(CRUISE,YEAR,FDATE,SFA,WEIGHT,WING,DIST),summarize,
                  STD_CATCH=WEIGHT*17.4/WING*((1.25*1852)/(DIST*1852)))
head(survey.dat)

#Bootstrap stratified standardized catch rate
#Use Stratify function from S.Smith 
loadfunctions("bio.survey")
#Create dataframe that contains the strata, the area in km2, 
#Data Formatting Requirements:
strata.Shrimp<-data.frame(Strata=c(13,14,15,17),Area=c(1620,1517,948,1415),NH=c(40207.55862,37653.07586,23535.30115,35128.22422))
strata.Shrimp

SurveyCatchStd<-survey.dat[,c("SFA","STD_CATCH")]
SurveyCatchStd$STRATA.ID=SurveyCatchStd$strata=SurveyCatchStd$SFA
str(SurveyCatchStd)
#'data.frame':	60 obs. of  4 variables:
#$ SFA      : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STD_CATCH: num  60 75.4 69.3 98.4 105.3 ...
#$ strata   : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STRATA.ID: int  15 15 15 15 15 15 15 15 15 15 ...

SurveyCatchStd = Prepare.strata.data(SurveyCatchStd)
str(SurveyCatchStd)
#Classes ‘strata.data’ and 'data.frame':	60 obs. of  5 variables:
#$ SFA      : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STD.CATCH: num  60 75.4 69.3 98.4 105.3 ...
#$ strata   : int  15 15 15 15 15 15 15 15 15 15 ...
#$ STRATA.ID: int  15 15 15 15 15 15 15 15 15 15 ...
#$ Strata   : int  15 15 15 15 15 15 15 15 15 15 ...
#strata.Shrimp = Prepare.strata.file(strata.Shrimp)

str(strata.Shrimp)
#List of 2
#$ Strata: num [1:4] 13 14 15 17
#$ NH    : num [1:4] 40208 37653 23535 35128

Shrimp<-Stratify(SurveyCatchStd,strata.group=strata.Shrimp, species=STD.CATCH)
#Shrimp
#    Strata  Sets         Wh                 Mean            Std. Err.        RE(%) Sets.w.Spec
#[1,] "13"   "15" "0.294508742366013" "153.837463874531" "21.9932588482337" "Not.Est" "14"       
#[2,] "14"   "15" "0.275797894683034" "217.455210751274" "25.0407313656335" "Not.Est" "15"       
#[3,] "15"   "15" "0.172389276563638" "117.519140363592" "17.0506820551315" "Not.Est" "15"       
#[4,] "17"   "15" "0.257304086387315" "240.354123751201" "64.3790642220654" "Not.Est" "15"   

Shrimp.boot<-boot.strata(Shrimp,nresamp=1000,method="BWR")
summary(Shrimp.boot,CI.method = "Percentile")
#[[1]]
#2.5% 97.5%   50% 
#153.3 229.2 186.2 

#[[2]]
#2.5%  97.5%    50% 
#0.0883 0.2571 0.1632 

#[[3]]
#2.5% 97.5%   50% 
#1409  1504  1472 

##################################################### DAVE's NOTES ##########################################################

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
vsurv.set<-subset(shrimp.survey,SETCODE %in% c(1,2) & SFA<18)
#CALC shrimp survey CV by SFA (consider snowcrab CV?)
svyCVsfa<-ddply(vsurv.set,.(YEAR,CRUISE,SFA),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
head(svyCVsfa) 
#YEAR COEF_VAR
#1 1982    89.06
#2 1983    78.52
#3 1984    75.84
#4 1985    83.09
#5 1986   106.13
#6 1987    67.53

#CALC shrimp survey CV by YEAR (consider snowcrab CV?)
svyCVyear<-ddply(vsurv.set,.(YEAR),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
head(svyCVyear) 
#   YEAR COEF_VAR
#1  1982    89.06
#2  1983    78.52
#3  1984    75.84
#4  1985    83.09
#5  1986   106.13
#6  1987    67.53

#PLOT survey CV by area and overall
ggplot(svyCVsfa,aes(factor(YEAR),COEF_VAR),group=YEAR)	+ geom_point() + geom_line() + facet_wrap(~SFA) + 
  theme(axis.text.x=element_text(angle=90))
ggplot(svyCVyear,aes(YEAR,COEF_VAR))	+ geom_point() + geom_line()




################################################################################################################


################## Commercial CPUEs ######################################
## Undstandardized trawl CPUE
#CALC unstand CPUE by stratum
#PLOT unstand CPUE by stratum
#PLOT unstand CPUE boxplots (overall, by SFA, by month/depletion)

#Calculate Commercial CPUE:
shrimp.db('ComLog.redo')
shrimp.db('ComLog')
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
