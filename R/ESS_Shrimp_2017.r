### Perform Shrimp Stock Assessment for 2017 fishing season. 
### MCassistaDaRos running and modifying code provided by DHardie/JBroome in ESS_Shrimp_2016.r
### Start: April 3, 2017 

### Set WD
direct <- setwd("C:/Users/cassistadarosm/Documents/R/SHRIMP/2017/")	
getwd()



### Required Packages
library("RODBC")
library("BIOsurvey")
library("PBSmapping")
library("car")
library("effects")
library("BIOsurvey")
library("plotrix")
#library("RMySQL")


### Required functions
source(paste(direct,"/fns/Prepare.strata.data.r",sep=""))

source(paste(direct,"/fns/Prepare.strata.file.r",sep=""))

source(paste(direct,"/fns/course_lib.r",sep=""))



### ROBDC Connection
library("RODBC")
ch<-odbcConnect("PTRAN", uid="x", pwd="y")
ch 


###_________________________________________________________________________________________________________

the.year=2016
the.survey='CK1601'

#example
#query<-paste("select cruise, fdate, to_char(fdate,'YYYY'), weight, wing, dist, sfa 
#from shrsurvey 
#WHERE to_char(fdate, 'YYYY')= ",the.year,"
#order by to_char(fdate, 'YYYY'), sfa",sep="")
#data<-sqlQuery(ch,query)

#ess_tla<-
#what is needed?
	#BIOSurvey
	#PBSmapping
	#mohn course_lib and prog_id aggd_dat (see Mohn course aggd_dat)



################################################################################################################
################################################################################################################
################################################################################################################

################### Survey CPUE and Biomass ############################## 

	##SHRIMP SURVEY CPUE, BIOMASS, CV##

#standardize catch to trawlable unit	
survey.qry<-paste("select cruise, fdate, to_char(fdate,'YYYY'), weight, wing, dist, sfa 
from shrsurvey 
WHERE to_char(fdate, 'YYYY')= ",the.year,"
order by to_char(fdate, 'YYYY'), sfa",sep="")
survey.dat<-sqlQuery(ch,survey.qry)
head(survey.dat)

Std.catch<-c((survey.dat$WEIGHT*17.4/survey.dat$WING)*((1.25*1852)/(survey.dat$DIST*1852)))
survey.dat<-cbind(survey.dat,Std.catch) #works
head(survey.dat)

#Bootstrap stratified standardized catch rate
  require(BIOsurvey)
#to get this from ecomod got Mike McMahon to set it up and use below to get it from my R folder in My Documents
#loadfunctions("BIOsurvey")
#strata.Shrimp<-list(SFA=c(13,14,15,17),Area=c(1620,1517,948,1415),NH=c(40207.55862,37653.07586,23535.30115,35128.22422))

strata.Shrimp<-data.frame(Strata=c(13,14,15,17),Area=c(1620,1517,948,1415),NH=c(40207.55862,37653.07586,23535.30115,35128.22422))
### names(strata.Shrimp)[names(strata.Shrimp)=="strata.Shrimp$SFA"]<-"Strata"
strata.Shrimp

SurveyCatchStd<-cbind.data.frame(survey.dat$SFA,survey.dat$Std.catch,deparse.level=1)
names(SurveyCatchStd)[names(SurveyCatchStd)=="survey.dat$SFA"]<-"Strata"
names(SurveyCatchStd)[names(SurveyCatchStd)=="survey.dat$Std.catch"]<-"Std.catch"
str(SurveyCatchStd)

class(SurveyCatchStd)<-c("data.frame","strata.data")
attach(SurveyCatchStd)


###Load functions
  source(paste(direct,"/fns/Prepare.strata.data.r",sep=""))
  source(paste(direct,"/fns/Prepare.strata.file.r",sep=""))


SurveyCatchStd = Prepare.strata.data(SurveyCatchStd)

strata.Shrimp = Prepare.strata.file(strata.Shrimp)


#Shrimp<-Stratify(data.obj=SurveyCatchStd,strata.group=strata.Shrimp,strata.name="Strata",species="Std.catch",Subset=SurveyCatchStd$Std.catch)
str(SurveyCatchStd)
str(strata.Shrimp)

Shrimp<-Stratify(SurveyCatchStd,strata.Shrimp, strata.name ="Strata", species="Std.catch")
Shrimp
Shrimp.boot<-boot.strata(Shrimp,nresamp=1000,method="BWR")
summary(Shrimp.boot,CI.method = "Percentile")





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


	##FISHERY INDEPENDENT DISPERSION

	#CALC shrimp survey CV (consider snowcrab CV?)

#from surveydispersionsfa.sql
svyCVsfa.qry<-paste("select cruise, sfa, 
stddev(weight+0.000001)/avg(weight+.000001)*100 
from SHRIMP.shrsurvey 
where setcode=1 
or setcode=2
group by cruise, sfa")
svyCVsfa.dat<-sqlQuery(ch,svyCVsfa.qry)
head(svyCVsfa.dat)  #works


#from surveydispersionallsets.sql
svyCVall.qry<-paste("select TO_NUMBER(TO_CHAR(FDATE,'YYYY')) YYYY,
stddev(weight+0.000001)/avg(weight+.000001)*100
from shrsurvey where setcode=1 or setcode=2 
group by TO_NUMBER(TO_CHAR(FDATE,'YYYY'))
order by TO_NUMBER(TO_CHAR(FDATE,'YYYY'))")
svyCVall.dat<-sqlQuery(ch,svyCVall.qry)
head(svyCVall.dat) ##works
	
#PLOT survey CV by area and overall
	
	






################################################################################################################
################################################################################################################
################################################################################################################

################## Commercial CPUEs ######################################

	## Undstandardized trawl CPUE
	#CALC unstand CPUE by stratum
	#PLOT unstand CPUE by stratum
	#PLOT unstand CPUE boxplots (overall, by SFA, by month/depletion)
	
	#CALC gulf CPUE 


gulf.qry<-paste("select to_number(to_char(fdate,'YYYY')) , avg(weight)/avg(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6) 
from shrcomlog
where weight>0 
and fhours>0 
and weight is not null 
and fhours is not null
and btype = 0003
group by to_number(to_char(fdate,'YYYY'))
order by to_number(to_char(fdate,'YYYY'))")

gulf.dat<-sqlQuery(ch,gulf.qry)
head(gulf.dat)

colnames(gulf.dat)<-c("Year", "CPUE")
head(gulf.dat) #works
	
#print(gulf.dat$CPUE, digits=6)



	####    PLOT gulf CPUE  
jpeg(filename="gulf_cpue.jpg")
ry<-quantile(gulf.dat$CPUE[gulf.dat$Year>1999&gulf.dat$Year<2011], probs=.33, na.rm=TRUE)
yg<-quantile(gulf.dat$CPUE[gulf.dat$Year>1999&gulf.dat$Year<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(gulf.dat$CPUE))
colours[gulf.dat$CPUE<ry]<-"red"
colours[gulf.dat$CPUE>yg]<-"green"
plot(gulf.dat$Year,gulf.dat$CPUE, ylab="", xlab="", col=colours, pch=19)
lines(gulf.dat$Year,gulf.dat$CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("Gulf cpue (kg/30 min)",side=2,line=2)
dev.off()    





##############################################################################################	

	## Unstandardized trawl CPUE
	#run NS_comm_cpue_unfilt_unstand.sql 
unstand.qry<-paste("select TO_NUMBER(TO_CHAR(fdate,'YYYY')),   
avg(decode(sfa,13,weight/(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6))) ,  
avg(decode(sfa,14,weight/(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6))) ,  
avg(decode(sfa,15,weight/(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6)))  
from shrcomlog  
where btype not in (4,5) 
and not(blat>451000 and blong>592000) 
and bcode not in ('5631', '102680') 
and weight>0 
and fhours>0 
and weight is not null 
and fhours is not null  
group by TO_NUMBER(TO_CHAR(fdate,'YYYY'))  
order by TO_NUMBER(TO_CHAR(fdate,'YYYY'))")
unstand.dat<-sqlQuery(ch,unstand.qry)
head(unstand.dat)
#colnames(unstand.dat)<-c("Year", "stratum 13", "stratum 14", "stratum 15")
colnames(unstand.dat)<-c("Year", "13", "14", "15")
head(unstand.dat) #works



	#### now for inshore
unstand_inshore.qry<-paste("select TO_NUMBER(TO_CHAR(fdate,'YYYY')) YYYY, 
avg(weight/(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6))
from shrcomlog
where btype not in (4,5) and blat>451000 and blong>592000
and weight>0 and fhours>0 and weight is not null and fhours is not null
group by TO_NUMBER(TO_CHAR(fdate,'YYYY'))
order by TO_NUMBER(TO_CHAR(fdate,'YYYY'))")
unstand_inshore.dat<-sqlQuery(ch,unstand_inshore.qry)
head(unstand_inshore.dat)
#colnames(unstand_inshore.dat)<-c("Year", "stratum 17")
colnames(unstand_inshore.dat)<-c("Year", "17")
head(unstand_inshore.dat)

	
  #now merge them
unstand_4strat.dat<-merge(unstand.dat, unstand_inshore.dat, all.x=TRUE)
head(unstand_4strat.dat)
unstand_4strat.dat
	
#print(unstand_4strat.dat, digits=6)


############################

#### NOTE, NOTE, NOTE  ####  This takes mean catch per year across all sfas. 

############################

#add means

#unstand_4strat.dat$average <-ave(unstand_4strat.dat$13, unstand_4strat.dat$14, unstand_4strat.dat$15, unstand_4strat.dat$17)

str(unstand_4strat.dat)
rowMeans(x = unstand_4strat.dat[,2:ncol(unstand_4strat.dat], na.rm = TRUE)
mean<-rowMeans(x = unstand_4strat.dat[,2:ncol(unstand_4strat.dat)], na.rm = TRUE)
cbind(unstand_4strat.dat, mean)
unstand_4strat.dat<-cbind(unstand_4strat.dat, mean)
unstand_4strat.dat
  

    ###now need to plot out unstand for each stratum and the overall mean (but note, 1993 is missing from inshore, so need to account for that somehow)



##################################################################################################


	##standardized trawl CPUE - data fit to GLM from April to July inclusive and only boats that have fished 7 years or more, do not process on board, and NS fleet only.  
  ##Standarized to highliner, highest catch month and SFA

#############Bring code from standardize_cpue.r and apply it to unstand.dat above
#below query comes from unfilt_cpue.sql in ulfilt_cpue.sql

unfilt.qry<-paste("select bcode,fdate,to_char(fdate,'YYYY'),to_char(fdate,'MM'),sfa,fhours,weight, 
weight/(trunc(fhours/100)+((fhours/100)-trunc(fhours/100))/0.6)
from shrcomlog  
where fhours > 0 
and weight > 0 
and btype not in (3,4,5) 
order by bcode, fdate")
unfilt.dat<-sqlQuery(ch,unfilt.qry)
head(unfilt.dat)
colnames(unfilt.dat)<-c("bcode", "fdate", "year", "month", "sfa", "hours", "weight", "cpue")
head(unfilt.dat) #works	


#first, cut it down to April-July inclusive (months 4 to 7)
unfilt.dat<-unfilt.dat[unfilt.dat$month>3 & unfilt.dat$month<8,]
dim(unfilt.dat)

#second, include only boats that have fished for 7 years or more
attach(unfilt.dat)
#cbind bcode and year and identify unique combinations
dat<-unique(cbind(bcode,year))
#aggregate bcode/year data to find the length of year occurences by boatcode
boatsyears<-aggregate(year~bcode, data=dat, FUN=length)
#now retain only boats where years>6
boats7<-subset(boatsyears, year>6)
#now remove Final Venture and Island Provider, which are bcodes -'5631', '102680'
boats7<-subset(boats7, !bcode==5631 & !bcode==102680)
#vector of bcodes for 7 years and more
boats_filt<-c(boats7$bcode)
#boats_fac<-as.factor(boats_filt)

mmm<-as.data.frame(boats_filt)
names(mmm)[1]<-"bcode"
filt_cpue<-merge(unfilt.dat, mmm)

write.table(filt_cpue,file="filt_cpue.txt") 

#add a column of numeric months "m" to the dataframe
ii = (as.character(filt_cpue$month) )
filt_cpue$m = as.numeric( gsub( "M", "", ii))
attach(filt_cpue)

#Obtain non-standardized mean CPUE for this list of boats for April-July for the Res Doc figure.
unstand_cpue<-aggregate(filt_cpue,by=list(year=year), mean)
#now write it to the directory 
write.table(unstand_cpue,file="unstand_CPUE_april_july-1.txt") 

#transform the GLM factors from numeric to factors
tt<-transform(filt_cpue, bcode=as.factor(bcode), sfa=as.factor(sfa), month=as.factor(month), year=as.factor(year))

#attach(filt_cpue)
attach(tt)
head(tt)


#run the GLM
#with Gaussian error distribution it is basically an ANOVA, but could try other error distributions

#k <- glm(formula = cpue ~ bcode + year + month + sfa, family = gaussian, 
#	data = filt_cpue, na.action = na.exclude, 
#	control = list(epsilon = 0.0001, maxit = 50, trace = F))

k <- glm(formula = cpue ~ bcode + year + month + sfa, family = gaussian, 
	data = tt, na.action = na.exclude, 
	control = list(epsilon = 0.0001, maxit = 50, trace = F))

#look at the summary of k and note the AIC number at the bottom, which is a measure of the complexity and goodness of fit of the GLM as it is defined
summary(k)

#run ANOVA to confirm significance of factors
  require(car) 
  Anova(k) #significance of factors
  
#this code will fix all the factors except the one of interest and plot it out
  require(effects)
  j = allEffects(k)
  plot(j) # plots fixing everything at their means but the var of interest

#then look at the YEAR effect, which is essentially the GLM standardized CPUE estimate
  j$year
  str(j)
  
#what is "o" for here?  o dataframe holds the values at which I want predicted estimates.  
##The example below is arbitrarily defined at the first row of all values except for all years.  
##Because there are no interaction terms, changing this will only move the estimates up or down, but will not actually change the temporal trend.
   
    o = data.frame( 
    bcode=tt$bcode[1], 
    year=sort( unique(tt$year) ), 
    month=tt$month[1],
    sfa=tt$sfa[1]
   )
 
 #### this is to MANUALLY identify the highligher for 2016 and then to manually choose june and SFA 14
 hh<-aggregate(tt$cpue~tt$year+tt$bcode,FUN=mean)
 data.frame(hh)
 names(hh)<-c("year","bcode","cpue")
 hh[hh$year==2016,]
 
 #####HARD CODED  - 2016 - FIXING BCODE AT MEAN OF HIGHLINER - 104885, MONTH AS MEAN IN JULY, SFA AS MEAN IN SFA13
    o = data.frame( 
    bcode=as.factor(104885), 
    year=sort( unique(tt$year) ), 
    month=as.factor(7),
    sfa=as.factor(13)
   )

#run the GLM (k) on newdata (o)
  kk = predict( k, newdata=o, type = c("response" ), se.fit=T )
  o$cpue = kk$fit
  o$cpue_se = kk$se.fit

#below can't find CPUE_SE  Is this because it is a recent addition to the dataframe?  Maybe I need to "re-attach" o?
attach(o)
  
  kk = predict( k, newdata=o, type = c("response" ), se.fit=T )
  o$cpue = kk$fit
  o$cpue_se = kk$se.fit

#yep, that works now
  o$ub = (o$cpue + 2*cpue_se)
  o$lb = (o$cpue - 2*cpue_se)
  o$mean = (o$cpue)

#now "o" has the new columns and upper bound and lower bound are two times the SE (might change this?)
  
#write "o" to the working directory
write.table(o,file="Stand_nonlog_cpue_2016_april_july.txt") 

###PLOT THIS






################################################################################################################
################################################################################################################
################################################################################################################


################################################  Catch Rate Boxplots ##############################################################


unfilt_cpue<-unfilt.dat
head(unfilt_cpue)
dim(unfilt_cpue)

#what does the distribution of the data look like, unstandardised
boxplot(unfilt_cpue$cpue~unfilt_cpue$year, ylab="CPUE (kg/hour)")

#by month over all years
boxplot(unfilt_cpue$cpue~unfilt_cpue$month, ylab="CPUE (kg/hour)")

#by month for a specific years
yrs<-c(1993:2016)

boxplot(unfilt_cpue$cpue[unfilt_cpue$year==2016]~unfilt_cpue$month[unfilt_cpue$year==2016], ylab="CPUE (kg/hour)")

#could truncate this in July and repeat per year with fixed y axis for comparative reasons in a multi-panel plot.  Perhaps all years, then most recent 5 years?

#gg<-unfilt_cpue[unfilt_cpue$month>3,]

gg<-unfilt_cpue

attach(mtcars)

par(mfrow=c(3,2)) 

boxplot(gg$cpue~gg$month,  ylab="CPUE (kg/hour)", ylim=c(0,3500))  
text(c(3500), "All")

#boxplot(gg$cpue[gg$year==2010]~gg$month[gg$year==2010], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "2010")

#boxplot(gg$cpue[gg$year==2011]~gg$month[gg$year==2011], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "2011")

boxplot(gg$cpue[gg$year==2012]~gg$month[gg$year==2012], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2012")

boxplot(gg$cpue[gg$year==2013]~gg$month[gg$year==2013], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2013")

boxplot(gg$cpue[gg$year==2014]~gg$month[gg$year==2014], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2014")

boxplot(gg$cpue[gg$year==2015]~gg$month[gg$year==2015], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2015")

boxplot(gg$cpue[gg$year==2016]~gg$month[gg$year==2016], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2016")




#gg<-unfilt_cpue
gg<-unfilt_cpue[unfilt_cpue$month<8 & unfilt_cpue$month>3,]

attach(mtcars)

par(mfrow=c(3,2)) 

boxplot(gg$cpue~gg$month,  ylab="CPUE (kg/hour)", ylim=c(0,3500))
text(c(3500), "All")

#boxplot(gg$cpue[gg$year==2010]~gg$month[gg$year==2010], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "2010")

#boxplot(gg$cpue[gg$year==2011]~gg$month[gg$year==2011], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "2011")

boxplot(gg$cpue[gg$year==2012]~gg$month[gg$year==2012], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2012")

boxplot(gg$cpue[gg$year==2013]~gg$month[gg$year==2013], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2013")

boxplot(gg$cpue[gg$year==2014]~gg$month[gg$year==2014], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2014")

boxplot(gg$cpue[gg$year==2015]~gg$month[gg$year==2015], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2015")

boxplot(gg$cpue[gg$year==2016]~gg$month[gg$year==2016], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "2016")



#############    By SFA?   #################

###sfa 14

gg<-unfilt_cpue[unfilt_cpue$month<8 & unfilt_cpue$month>3 & unfilt_cpue$sfa==14,]

attach(mtcars)

par(mfrow=c(3,2)) 

boxplot(gg$cpue~gg$month,  ylab="CPUE (kg/hour)", ylim=c(0,3500))
text(c(3500), "14 - All")

#boxplot(gg$cpue[gg$year==2010]~gg$month[gg$year==2010], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "14 - 2010")

#boxplot(gg$cpue[gg$year==2011]~gg$month[gg$year==2011], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "14 - 2011")

boxplot(gg$cpue[gg$year==2012]~gg$month[gg$year==2012], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "14 - 2012")

boxplot(gg$cpue[gg$year==2013]~gg$month[gg$year==2013], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "14 - 2013")

boxplot(gg$cpue[gg$year==2014]~gg$month[gg$year==2014], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "14 - 2014")

boxplot(gg$cpue[gg$year==2015]~gg$month[gg$year==2015], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "14 - 2015")

boxplot(gg$cpue[gg$year==2016]~gg$month[gg$year==2016], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "14 - 2016")



#SFA15

gg<-unfilt_cpue[unfilt_cpue$month<8 & unfilt_cpue$month>3 & unfilt_cpue$sfa==15,]

attach(mtcars)

par(mfrow=c(3,2)) 

boxplot(gg$cpue~gg$month,  ylab="CPUE (kg/hour)", ylim=c(0,3500))
text(c(3500), "15 - All")

#boxplot(gg$cpue[gg$year==2010]~gg$month[gg$year==2010], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "15 - 2010")

#boxplot(gg$cpue[gg$year==2011]~gg$month[gg$year==2011], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "15 - 2011")

boxplot(gg$cpue[gg$year==2012]~gg$month[gg$year==2012], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "15 - 2012")

boxplot(gg$cpue[gg$year==2013]~gg$month[gg$year==2013], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "15 - 2013")

boxplot(gg$cpue[gg$year==2014]~gg$month[gg$year==2014], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "15 - 2014")

boxplot(gg$cpue[gg$year==2015]~gg$month[gg$year==2015], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "15 - 2015")

boxplot(gg$cpue[gg$year==2016]~gg$month[gg$year==2016], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "15 - 2016")



#SFA13

gg<-unfilt_cpue[unfilt_cpue$month<8 & unfilt_cpue$month>3 & unfilt_cpue$sfa==13,]

attach(mtcars)

par(mfrow=c(3,2)) 

boxplot(gg$cpue~gg$month,  ylab="CPUE (kg/hour)", ylim=c(0,3500))
text(c(3500), "13 - All")

#boxplot(gg$cpue[gg$year==2010]~gg$month[gg$year==2010], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "13 - 2010")

#boxplot(gg$cpue[gg$year==2011]~gg$month[gg$year==2011], ylim=c(0,3500), ylab="CPUE (kg/hour)")
#text(c(3500), "13 - 2011")

boxplot(gg$cpue[gg$year==2012]~gg$month[gg$year==2012], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "13 - 2012")

boxplot(gg$cpue[gg$year==2013]~gg$month[gg$year==2013], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "13 - 2013")

boxplot(gg$cpue[gg$year==2014]~gg$month[gg$year==2014], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "13 - 2014")

boxplot(gg$cpue[gg$year==2015]~gg$month[gg$year==2015], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "13 - 2015")

boxplot(gg$cpue[gg$year==2016]~gg$month[gg$year==2016], ylim=c(0,3500), ylab="CPUE (kg/hour)")
text(c(3500), "13 - 2016")






################################################################################################################
################################################################################################################
################################################################################################################

###################################### ESS SHRIMP SIZE INDICES ######################################################

######### Mean Size Sex Transition ###########################################################################

### "millim" is an existing view to which I now have access 2015-10-06. ###
### Still won't work.....


meanltrans.qry<-paste("select bcode,fdate,to_char(fdate,'YYYY'),sfa,xset,sum(carlen/10)/count(*) 
from shrimp.millim
where gear=4 
and tran=1 
or primnet=1 
group by bcode, fdate, sfa, xset
order by fdate")


meanltrans.dat<-sqlQuery(ch,meanltrans.qry)
head(meanltrans.dat)

colnames(meanltrans.dat)<-c("BCODE", "FDATE", "YEAR", "SFA", "XSET", "MEAN")
head(meanltrans.dat) #works	

meanltrans.dat<-subset(meanltrans.dat, !YEAR==1946)
head(meanltrans.dat)

trans<-meanltrans.dat
head(trans) 

attach(trans)

require(plotrix)

mean_trans<-aggregate(trans,by=list(year=YEAR),mean)
error_trans<-aggregate(trans,by=list(year=YEAR),std.error)

year<-mean_trans$YEAR
mean<-mean_trans$MEAN
se<-error_trans$MEAN
se<-2*se
ub<-mean+2*se
lb<-mean-2*se
mean_trans<-cbind(year,mean,se,ub,lb)

mlt<-as.data.frame(mean_trans)

#attach(mlt)

write.table(mean_trans, file="mean_trans.txt")

#plotCI(year,mean,se)
#par(mfrow=c(1,1)) 

plotCI(mlt$year,mlt$mean,2*mlt$se, xlab="year", ylab="mean sex transition (mm)", xlim=c(1995,2016), ylim=c(23,26))
lines(mlt$year,mlt$mean, xlim=c(1995,2016))
legend("topleft", "D", bty="n") 




######### Max Size ######################################################################################

#script max_length.sql
#csv file max_length.csv
#setwd()

par(mfrow=c(1,1))

#### to fit - must namually rename the header to BCODE	FDATE	YEAR	SFA	XSET	MAX_L and manually remove the single 1946 value
maxl.qry<-paste("select bcode,fdate,to_char(fdate,'YYYY'),sfa,xset,max(carlen/10) 
from shrdetail
where gear=4  group
by bcode,fdate,sfa,xset")
maxl.dat<-sqlQuery(ch,maxl.qry)
head(maxl.dat)

colnames(maxl.dat)<-c("BCODE", "FDATE", "YEAR", "SFA", "XSET", "MAX_L")
head(maxl.dat) #works	
maxl.dat<-subset(maxl.dat, !YEAR==1946)
head(maxl.dat)
maxl<-maxl.dat

mean_maxl<-aggregate(maxl,by=list(year=maxl$YEAR),mean)
error_maxl<-aggregate(maxl,by=list(year=maxl$YEAR),std.error)

year<-mean_maxl$YEAR
mean<-mean_maxl$MAX_L
se<-error_maxl$MAX_L
se<-2*se
ub<-mean+2*se
lb<-mean-2*se
mean_maxl<-cbind(year,mean,se,ub,lb)

mxl<-as.data.frame(mean_maxl)

write.table(mean_maxl, file="max_length.txt")

plotCI(mxl$year,mxl$mean,2*mxl$se, xlab="year", ylab="mean max length (mm)", xlim=c(1995,2016), ylim=c(28,31))
lines(mxl$year,mxl$mean)
legend("topleft", "B", bty="n") 




######### commercial counts###################################################################################

count.qry<-paste("select bcode,xset,fdate,to_char(fdate,'YYYY'),sfa,xcount 
from shrcomlog 
where btype not in(4,5)and xcount is not null
order by fdate")
count.dat<-sqlQuery(ch,count.qry)
head(count.dat)
colnames(count.dat)<-c("BCODE", "XSET", "FDATE", "YEAR", "SFA", "XCOUNT")
head(count.dat) #works	
counts<-count.dat
mean_count<-aggregate(counts,by=list(year=counts$YEAR),mean)
error_count<-aggregate(counts,by=list(year=counts$YEAR),std.error)

year<-mean_count$YEAR
mean<-mean_count$XCOUNT
se<-error_count$XCOUNT
ub<-mean+2*se
lb<-mean-2*se
comm_counts<-cbind(year,mean,se,ub,lb)
cc<-as.data.frame(comm_counts)
write.table(comm_counts, file="mean_comm_counts.txt")


par(mfrow=c(2,2))

plotCI(cc$year,cc$mean,2*cc$se, xlab="year", ylab="mean commercial count")
lines(cc$year,cc$mean)
legend("topleft", "A", bty="n") 




######### size of females in catch ###########################################################################
feml.qry<-paste("select bcode,fdate,to_char(fdate,'YYYY'),sfa,xset,avg(carlen/10) 
from shrdetail
where gear=1  and sex>4 
group by bcode,fdate,sfa,xset")
feml.dat<-sqlQuery(ch,feml.qry)
head(feml.dat)

colnames(feml.dat)<-c("BCODE", "FDATE", "YEAR", "SFA","XSET",  "MEAN")
head(feml.dat) #works	
feml<-feml.dat

mean_fem_size<-aggregate(feml,by=list(year=feml$YEAR),mean)
error_fem_size<-aggregate(feml,by=list(year=feml$YEAR),std.error)

year<-mean_fem_size$YEAR
mean<-mean_fem_size$MEAN
se<-error_fem_size$MEAN
ub<-mean+2*se
lb<-mean-2*se
fem_size<-cbind(year,mean,se,ub,lb)

fs<-as.data.frame(fem_size)
attach(fs)
write.table(fem_size, file="mean_fem_size.txt")

plotCI(fs$year,fs$mean,2*fs$se, xlab="year", ylab="mean female size (mm)")
lines(fs$year,fs$mean)
legend("topleft", "C", bty="n") 



#########################################################################################################


### SSB - density of females in the catch

ssb.qry<-paste("select shrsurvey.cruise,totals.sfa,avg(weight*1000/(dist*1852*17.705)*(totalsfemtran.totwt/totals.totwt))
               from shrsurvey, totals, totalsfemtran 
               where shrsurvey.cruise=totals.bcode
               and shrsurvey.cruise=totalsfemtran.bcode
               and shrsurvey.fdate=totals.fdate
               and shrsurvey.fdate=totalsfemtran.fdate
               and shrsurvey.sfa=totals.sfa
               and shrsurvey.sfa=totalsfemtran.sfa
               and shrsurvey.xset=totals.xset
               and shrsurvey.xset=totalsfemtran.xset
               and totalsfemtran.totwt>0 and totals.totwt>0
               and shrsurvey.setcode in(1,2)
               group by shrsurvey.cruise, totals.sfa
               order by shrsurvey.cruise, totals.sfa")


ssb.dat<-sqlQuery(ch,ssb.qry)
head(ssb.dat)


##### Manual read in - query does not work for me............ Michele Queried using SQL+

ssb.dat <- read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/SSB.csv")
colnames(ssb.dat)<-c("CRUISE", "SFA", "DENSITY")

kms2<-c(1620,1517,948,1415)
dd<-ssb.dat$DENSITY[ssb.dat$CRUISE=='CK1601']

ssb<-sum(kms2*dd)




#########################################  uncertainty on SSB #############################################


ssbsets.qry<-paste("select cruise, fdate, to_char(fdate,'YYYY'), xset, sfa, avg(weight*1000/(dist*1852*17.705)*(totalsfemtran.totwt/totals.totwt))
                   from shrsurvey, totals, totalsfemtran 
                   where shrsurvey.cruise=totals.bcode
                   and shrsurvey.cruise=totalsfemtran.bcode
                   and shrsurvey.fdate=totals.fdate
                   and shrsurvey.fdate=totalsfemtran.fdate
                   and shrsurvey.sfa=totals.sfa
                   and shrsurvey.sfa=totalsfemtran.sfa
                   and shrsurvey.xset=totals.xset
                   and shrsurvey.xset=totalsfemtran.xset
                   and totalsfemtran.totwt>0 and totals.totwt>0
                   and shrsurvey.setcode in(1,2)
                   group by shrsurvey.cruise, totals.sfa
                   order by shrsurvey.cruise, totals.sfa")


ssbsets.dat<-sqlQuery(ch,ssbsets.qry)
head(ssbsets.dat)


##### Manual read in - query does not work for me...........

#ssbsets.dat <- read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/filename####.csv")




##############################################################################################

#####################  fem catch prop ########################################################

femprop.qry<-paste("select to_char(totals.fdate,'YYYY'),totals.sfa, sum(totalsfemtran.totwt)/sum(totals.totwt)
                   from totals, totalsfemtran where 
                   totals.bcode=totalsfemtran.bcode
                   and totals.fdate=totalsfemtran.fdate
                   and totals.sfa=totalsfemtran.sfa
                   and totalsfemtran.totwt>0 and totals.totwt>0
                   and totals.gear=1
                   group by to_char(totals.fdate,'YYYY'),totals.sfa
                   order by to_char(totals.fdate,'YYYY'),totals.sfa")

femprop.dat<-sqlQuery(ch,femprop.qry)


##### Manual read in - query does not work for me...... Michele Queried using SQL+
femprop.dat <- read.csv("C:/Users/BroomeJ/Documents/R/SHRIMP/2016/data/annual_by_sfa.csv")

colnames(femprop.dat)<-c("YEAR", "SFA", "Fem_Prop")
head(femprop.dat)


#### From Dave 2016-11-14
TO_C        SFA                   percent weight female
---- ---------- ---------------------------------------
2016         13                                 8.1E-01
2016         14                                 7.3E-01
2016         15                                 5.7E-01




#plot(femprop.dat$YEAR, femprop.dat$Fem_Prop)

###___________________________________________________________________________________________________###

	#PLOT Stand CPUE by stratum
	
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

colnames(belly.dat)<-c("CarLen(mm)", "Strat13", "Strat14", "Strat15", "Strat17")
head(belly.dat) 	
sum(tt)

#Now, for each value in belly.dat, I need to divide by the number of sets (gets average count per set) and multiple by the number of trawlable units (gets estimate of number of shirmp of that carlen by swept area of BB)
#number of sets 15 for each strata if all done
#number of trawlable units is 699784.0173 	655291.5767	409503.2397	611231.1015 for the four strata, respectively.
#take total across the four strata and take sum of estiamte for carlen less than an including 11mm, divide by 1 million to get Index.

sets <- c(15, 15, 15, 15)
strata.area <- c(699784.0173, 655291.5767, 409503.2397, 611231.1015)





##################################################################################################################################################################################

############ ****   TRAFFIC LIGHT INDICATORS#   ****   ########################################################################################

##################################################################################################################################################################################


ess<-read.csv("data/ess_2016.csv", header=T)
attach(ess)
#detach(ess)

#intersect(search(), objects())
#lapply(X = intersect(search(), objects()), 
#      FUN = function(X){detach(name = X, character.only = TRUE)})



##SURVEY CPUE   
jpeg(filename="survey_cpue.jpg")
png(filename="survey_cpue.png")
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(RV_CPUE, probs=.33, na.rm=TRUE)
#yg<-quantile(RV_CPUE, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RV_CPUE))
#colours[RV_CPUE<ry]<-"red"
#colours[RV_CPUE>yg]<-"green"
#plot(yr,RV_CPUE, main="all data", ylab="survey CPUE", xlab="", col=colours, pch=19)
#lines(yr,RV_CPUE)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("survey cpue (kg/30 min)",side=2,line=2)

#2000s thresholds
ry<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(RV_CPUE))
colours[RV_CPUE<ry]<-"red"
colours[RV_CPUE>yg]<-"green"
plot(yr,RV_CPUE, ylab="Survey CPUE", xlab="Survey CPUE", col=colours, pch=19, na.exclude=TRUE)
lines(yr,RV_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("survey cpue (kg/30 min)",side=2,line=2)

dev.off()    


__________________________________________________________

##Gulf CPUE (gfcpue)

jpeg(filename="gfcpue_cpue.jpg")
png(filename="gfcpue_cpue.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(G_CPUE, probs=.33, na.rm=TRUE)
#yg<-quantile(G_CPUE, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(G_CPUE))
#colours[G_CPUE<ry]<-"red"
#colours[G_CPUE>yg]<-"green"
#plot(yr,G_CPUE, main="all data", ylab="Gulf CPUE", xlab="", col=colours, pch=19)
#lines(yr,G_CPUE)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("gulf cpue cpue (kg/h)",side=2,line=2)

#2000s thresholds
ry<-quantile(G_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(G_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(G_CPUE))
colours[G_CPUE<ry]<-"red"
colours[G_CPUE>yg]<-"green"
plot(yr,G_CPUE, ylab="Gulf CPUE", xlab="", col=colours, pch=19)
lines(yr,G_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("gulf cpue (kg/h)",side=2,line=2)

dev.off()   



_______________________________________________________________

## Standardised CPUE (stcpue)

jpeg(filename="stcpue_cpue.jpg")
png(filename="stcpue_cpue.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(St_CPUE, probs=.33, na.rm=TRUE)
#yg<-quantile(St_CPUE, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(St_CPUE))
#colours[St_CPUE<ry]<-"red"
#colours[St_CPUE>yg]<-"green"
#plot(yr,St_CPUE, main="all data", ylab="stcpue CPUE", xlab="", col=colours, pch=19)
#lines(yr,St_CPUE)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("stcpue cpue (kg/h)",side=2,line=2)

#2000s thresholds
ry<-quantile(St_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(St_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(St_CPUE))
colours[St_CPUE<ry]<-"red"
colours[St_CPUE>yg]<-"green"
plot(yr,St_CPUE, ylab="Standardised CPUE", xlab="", col=colours, pch=19)
lines(yr,St_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("stcpue cpue (kg/h)",side=2,line=2)

dev.off()    



______________________________________________________________________

##RV Coefficient of Variation (rvcv) 
#REVERSE POLARITY - swap yg for ry and reverse teh < and > symbols for plots where high is bad and low is good (red)
	
jpeg(filename="rvcv.jpg")
png(filename="rvcv.png")

#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(RV_CV, probs=.33, na.rm=TRUE)
#ry<-quantile(RV_CV, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RV_CV))
#colours[RV_CV>ry]<-"red"
#colours[RV_CV<yg]<-"green"
#plot(yr,RV_CV, main="all data", ylab="rvcv CPUE", xlab="", col=colours, pch=19)
#lines(yr,RV_CV)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("survey coefficient of variation",side=2,line=2)

#2000s thresholds
yg<-quantile(RV_CV[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(RV_CV[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(RV_CV))
colours[RV_CV>ry]<-"red"
colours[RV_CV<yg]<-"green"
plot(yr,RV_CV, ylab="RV c.v.", xlab="", col=colours, pch=19)
lines(yr,RV_CV)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("survey coefficient of variation",side=2,line=2)
dev.off()    



__________________________________________________________________________

## TRAP Standardised CPUE (Trap_CPTH)

jpeg(filename="trap_cpue.jpg")
png(filename="trap_cpue.png")

#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))


#2000s thresholds
ry<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Trap_CPTH))
colours[Trap_CPTH<ry]<-"red"
colours[Trap_CPTH>yg]<-"green"
plot(yr,Trap_CPTH, ylab="Trap CPTH", xlab="", col=colours, pch=19)
lines(yr,Trap_CPTH)
abline(h=ry, col="red")
abline(h=yg, col="green")       
#legend("topleft", "trap cpue", bty="n")
mtext("trap cpue (kg/trap hr)",side=2,line=2)
dev.off()  



____________________________________________________________________________


##commercial fishing area - positive polarity (area)

jpeg(filename="comm_area.jpg")
png(filename="comm_area.png")
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(area, probs=.33, na.rm=TRUE)
#yg<-quantile(area, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(area))
#colours[area<ry]<-"red"
#colours[area>yg]<-"green"
#plot(year,area, main="all data", ylab="area CPUE", xlab="", col=colours, pch=19)
#lines(year,area)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("area with cpue > 250 kg/h",side=2,line=2)

#2000s thresholds
ry<-quantile(Comm_area[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Comm_area[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(Comm_area))
colours[Comm_area<ry]<-"red"
colours[Comm_area>yg]<-"green"
plot(yr,Comm_area, ylab="", xlab="", col=colours, pch=19)
lines(yr,Comm_area)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("area with cpue > 250 kg/h",side=2,line=2)
dev.off()    




_________________________________________________________

##spawning stock biomass (ssb) positive polarity

jpeg(filename="ssb.jpg")
png(filename="ssb.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(RVSSB, probs=.33, na.rm=TRUE)
#yg<-quantile(RVSSB, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RVSSB))
#colours[RVSSB<ry]<-"red"
#colours[RVSSB>yg]<-"green"
#plot(yr,RVSSB, main="all data", ylab="ssb CPUE", xlab="", col=colours, pch=19)
#lines(yr,RVSSB)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("RVSSB (mt)",side=2,line=2)

#2000s thresholds
ry<-quantile(RVSSB[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RVSSB[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(RVSSB))
colours[RVSSB<ry]<-"red"
colours[RVSSB>yg]<-"green"
plot(yr,RVSSB, ylab="SSB Index", xlab="", col=colours, pch=19)
lines(yr,RVSSB)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("spawning stock biomass (mt)",side=2,line=2)
dev.off()    


________________________________________



##bellybag (bb) - positive

jpeg(filename="bb.jpg")
png(filename="bb.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(BB_1, probs=.33, na.rm=TRUE)
#yg<-quantile(BB_1, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(BB_1))
#colours[BB_1<ry]<-"red"
#colours[BB_1>yg]<-"green"
#plot(yr,BB_1, main="all data", ylab="BB_1 CPUE", xlab="", col=colours, pch=19)
#lines(yr,BB_1)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("bellybag index",side=2,line=2)

#2000s thresholds
ry<-quantile(BB_1[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(BB_1[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(BB_1))
colours[BB_1<ry]<-"red"
colours[BB_1>yg]<-"green"
plot(yr,BB_1, ylab="Bellybag Index", xlab="", col=colours, pch=19)
lines(yr,BB_1)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("bellybag index",side=2,line=2)
dev.off()    


________________________________________


##age-s abundance (rv2)

jpeg(filename="rv2.jpg")
png(filename="rv2.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(RV_2, probs=.33, na.rm=TRUE)
#yg<-quantile(RV_2, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RV_2))
#colours[RV_2<ry]<-"red"
#colours[RV_2>yg]<-"green"
#plot(yr,RV_2, main="all data", ylab="RV_2 CPUE", xlab="", col=colours, pch=19)
#lines(yr,RV_2)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("age-2 abundance index",side=2,line=2)

#2000s thresholds
ry<-quantile(RV_2[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_2[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(RV_2))
colours[RV_2<ry]<-"red"
colours[RV_2>yg]<-"green"
plot(yr,RV_2, ylab="Age 2 index", xlab="", col=colours, pch=19)
lines(yr,RV_2)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("age-2 abundance index",side=2,line=2)
dev.off()    



__________________________________________

##age-4 abundance index (rv4) - positive

jpeg(filename="rv4.jpg")
png(filename="rv4.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(RV_4, probs=.33, na.rm=TRUE)
#yg<-quantile(RV_4, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RV_4))
#colours[RV_4<ry]<-"red"
#colours[RV_4>yg]<-"green"
#plot(yr,RV_4, main="all data", ylab="RV_4 CPUE", xlab="", col=colours, pch=19)
#lines(yr,RV_4)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("age-4 abundance index",side=2,line=2)
#2000s thresholds
ry<-quantile(RV_4[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_4[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(RV_4))
colours[RV_4<ry]<-"red"
colours[RV_4>yg]<-"green"
plot(yr,RV_4, ylab="Age 4 index", xlab="", col=colours, pch=19)
lines(yr,RV_4)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("age-4 abundance index",side=2,line=2)
dev.off() 


_______________________________________


### size at sex trans (sexmm)

jpeg(filename="sexmm.jpg")
png(filename="sexmm.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(sex_mm, probs=.33, na.rm=TRUE)
#yg<-quantile(sex_mm, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(sex_mm))
#colours[sex_mm<ry]<-"red"
#colours[sex_mm>yg]<-"green"
#plot(yr,sex_mm, main="all data", ylab="sex_mm CPUE", xlab="", col=colours, pch=19)
#lines(yr,sex_mm)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("mean size at sex transition (mm)",side=2,line=2)

#2000s thresholds
ry<-quantile(sex_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(sex_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(sex_mm))
colours[sex_mm<ry]<-"red"
colours[sex_mm>yg]<-"green"
plot(yr,sex_mm, ylab="Sex transition (mm)", xlab="", col=colours, pch=19)
lines(yr,sex_mm)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("mean size at sex transition (mm)",side=2,line=2)
dev.off() 


_______________________________________


### maximum size (maxmm)

jpeg(filename="maxmm.jpg")
png(filename="maxmm.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(max_mm, probs=.33, na.rm=TRUE)
#yg<-quantile(max_mm, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(max_mm))
#colours[max_mm<ry]<-"red"
#colours[max_mm>yg]<-"green"
#plot(yr,max_mm, main="all data", ylab="max_mm CPUE", xlab="", col=colours, pch=19)
#lines(yr,max_mm)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("mean maximum size (mm)",side=2,line=2)

#2000s thresholds
ry<-quantile(max_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(max_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(max_mm))
colours[max_mm<ry]<-"red"
colours[max_mm>yg]<-"green"
plot(yr,max_mm, ylab="maximum length (mm)", xlab="", col=colours, pch=19)
lines(yr,max_mm)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("mean maximum size (mm)",side=2,line=2)
dev.off() 


__________________________________________________________



### predation (confirmed shrimp predators) - negative
	
jpeg(filename="pred.jpg")
png(filename="pred.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(pred, probs=.33, na.rm=TRUE)
#ry<-quantile(pred, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(pred))
#colours[pred>ry]<-"red"
#colours[pred<yg]<-"green"
#plot(yr,pred, main="all data", ylab="pred CPUE", xlab="", col=colours, pch=19)
#lines(yr,pred)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("predator abundance index",side=2,line=2)

#2000s thresholds
yg<-quantile(pred[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(pred[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(pred))
colours[pred>ry]<-"red"
colours[pred<yg]<-"green"
plot(yr,pred, ylab="Predator abundance index", xlab="", col=colours, pch=19)
lines(yr,pred)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("predator abundance index",side=2,line=2)
dev.off()    




______________________________________________________________


### count  - NEGATIVE
	
jpeg(filename="count.jpg")
png(filename="count.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(count, probs=.33, na.rm=TRUE)
#ry<-quantile(count, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(count))
#colours[count>ry]<-"red"
#colours[count<yg]<-"green"
#plot(yr,count, main="all data", ylab="count CPUE", xlab="", col=colours, pch=19)
#lines(yr,count)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("commercial count (shimp per pound)",side=2,line=2)
#2000s thresholds
yg<-quantile(count[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(count[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(count))
colours[count>ry]<-"red"
colours[count<yg]<-"green"
plot(yr,count, ylab="count", xlab="", col=colours, pch=19)
lines(yr,count)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("commercial count (shimp per pound)",side=2,line=2)
dev.off()    



___________________________________________________

## exploitation (NEGATIVE - exp)


#### Note do not yet have 2016 data for ess_2016 file. 



	
jpeg(filename="exp.jpg")
png(filename="exp.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(Exp_tot, probs=.33, na.rm=TRUE)
#ry<-quantile(Exp_tot, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(Exp_tot))
#colours[Exp_tot>ry]<-"red"
#colours[Exp_tot<yg]<-"green"
#plot(yr,Exp_tot, main="all data", ylab="Exp_tot CPUE", xlab="", col=colours, pch=19)
#lines(yr,Exp_tot)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("Exp_totloitation index",side=2,line=2)

#2000s thresholds
yg<-quantile(Exp_tot[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Exp_tot[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(Exp_tot))
colours[Exp_tot>ry]<-"red"
colours[Exp_tot<yg]<-"green"
plot(yr,Exp_tot,  ylab="Total Exploitation", xlab="", col=colours, pch=19)
lines(yr,Exp_tot)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("Exploitation index",side=2,line=2)
dev.off()    



_____________________________________________________

## female exploitation index (NEGATIVE - femexp)


	
jpeg(filename="femexp.jpg")
png(filename="femexp.png")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(Exp_fem, probs=.33, na.rm=TRUE)
#ry<-quantile(Exp_fem, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(Exp_fem))
#colours[Exp_fem>ry]<-"red"
#colours[Exp_fem<yg]<-"green"
#plot(yr,Exp_fem, main="all data", ylab="Exp_fem CPUE", xlab="", col=colours, pch=19)
#lines(yr,Exp_fem)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("female exploitation index",side=2,line=2)

#2000s thresholds
yg<-quantile(Exp_fem[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Exp_fem[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(Exp_fem))
colours[Exp_fem>ry]<-"red"
colours[Exp_fem<yg]<-"green"
plot(yr,Exp_fem, ylab="Female Exploitation", xlab="", col=colours, pch=19)
lines(yr,Exp_fem)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("female exploitation index",side=2,line=2)
dev.off()    




__________________________________________________

## proportion of females in the catch (POSITIVE) 


jpeg(filename="femprop.jpg")
png(filename="femprop.png")

#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(femcatch_prop, probs=.33, na.rm=TRUE)
#yg<-quantile(femcatch_prop, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(femcatch_prop))
#colours[femcatch_prop<ry]<-"red"
#colours[femcatch_prop>yg]<-"green"
#plot(yr,femcatch_prop, main="all data", ylab="femcatch_prop CPUE", xlab="", col=colours, pch=19)
#lines(yr,femcatch_prop)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("proportion of females in catch (mm)",side=2,line=2)
#2000s thresholds
ry<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(femcatch_prop))
colours[femcatch_prop<ry]<-"red"
colours[femcatch_prop>yg]<-"green"
plot(yr,femcatch_prop, ylab="Proportion females in catch", xlab="", col=colours, pch=19)
lines(yr,femcatch_prop)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("proportion of females in catch (mm)",side=2,line=2)
dev.off() 



____________________________________________________________________

## female size (femsize) - positive
				
jpeg(filename="femsize.jpg")
png(filename="femsize.png")

#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(ess$fem_size, probs=.33, na.rm=TRUE)
#yg<-quantile(ess$fem_size, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(ess$fem_size))
#colours[ess$fem_size<ry]<-"red"
#colours[ess$fem_size>yg]<-"green"
#plot(yr,ess$fem_size, main="all data", ylab="ess$fem_size CPUE", xlab="", col=colours, pch=19)
#lines(yr,ess$fem_size)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("average female size (mm)",side=2,line=2)

#2000s thresholds
ry<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(ess$fem_size))
colours[ess$fem_size<ry]<-"red"
colours[ess$fem_size>yg]<-"green"
plot(yr,ess$fem_size, ylab="average female size (mm)", xlab="", col=colours, pch=19)
lines(yr,ess$fem_size)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("average female size (mm)",side=2,line=2)

dev.off() 

_______________________________________________________________________

### Effort - negative

jpeg(filename="effort.jpg")
png(filename="effort.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))

#2000s thresholds
yg<-quantile(effort[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(effort[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(effort))
colours[effort>ry]<-"red"
colours[effort<yg]<-"green"
plot(yr,effort, ylab="Effort", xlab="", col=colours, pch=19)
lines(yr,effort)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("total effort (hrs)",side=2,line=2)

dev.off()





_______________________________________________________________________


##Population Age-length evenness (popeven) - POSTIVIE
				
jpeg(filename="popeven.jpg")
png(filename="popeven.png")
par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(popeven, probs=.33, na.rm=TRUE)
#yg<-quantile(popeven, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(popeven))
#colours[popeven<ry]<-"red"
#colours[popeven>yg]<-"green"
#plot(yr,popeven, main="all data", ylab="popeven CPUE", xlab="", col=colours, pch=19)
#lines(yr,popeven)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("population length evenness",side=2,line=2)

#2000s thresholds
ry<-quantile(pop_even[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(pop_even[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(pop_even))
colours[pop_even<ry]<-"red"
colours[pop_even>yg]<-"green"
plot(yr,pop_even, ylab="", xlab="", col=colours, pch=19)
lines(yr,pop_even)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("population length evenness",side=2,line=2)
dev.off() 




________________________________________________________________________

##bottom temperatures (botemp - NEGATIVE) 
	
jpeg(filename="botemp.jpg")
png(filename="botemp.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(Rvbotemp, probs=.33, na.rm=TRUE)
#ry<-quantile(Rvbotemp, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(Rvbotemp))
#colours[Rvbotemp>ry]<-"red"
#colours[Rvbotemp<yg]<-"green"
#plot(yr,Rvbotemp, main="all data", ylab="survey bottom temperature (C)", xlab="", col=colours, pch=19)
#lines(yr,Rvbotemp)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("mean survey bottom temperature (C)",side=2,line=2)
#2000s thresholds
yg<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(Rvbotemp))
colours[Rvbotemp>ry]<-"red"
colours[Rvbotemp<yg]<-"green"
plot(yr,Rvbotemp, ylab="RV Bottom Temperature (C)", xlab="", col=colours, pch=19)
lines(yr,Rvbotemp)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("mean survey bottom temperature (C)",side=2,line=2)
dev.off()    





________________________________________________________________________

##spring sea surface temperature (surtemp) NEGATIVE
	
jpeg(filename="surtemp.jpg")
png(filename="surtemp.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(SSTs, probs=.33, na.rm=TRUE)
#ry<-quantile(SSTs, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(SSTs))
#colours[SSTs>ry]<-"red"
#colours[SSTs<yg]<-"green"
#plot(yr,SSTs, main="all data", ylab="SSTs CPUE", xlab="", col=colours, pch=19)
#lines(yr,SSTs)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("spring sea surface temperature (C)",side=2,line=2)
#2000s thresholds
yg<-quantile(SSTs[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(SSTs[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(SSTs))
colours[SSTs>ry]<-"red"
colours[SSTs<yg]<-"green"
plot(yr,SSTs, ylab="", xlab="", col=colours, pch=19)
lines(yr,SSTs)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("spring sea surface temperature (C)",side=2,line=2)
dev.off()    






_______________________________________________________________________

##Needler bottom temperatures (botemp - NEGATIVE) - NOTE: Removed as indice following 2014 framework. 

jpeg(filename="needlertemp.jpg")
png(filename="needlertemp.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))


#2000s thresholds
yg<-quantile(needlerbotemp[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(needlerbotemp[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(needlerbotemp))
colours[needlerbotemp>ry]<-"red"
colours[needlerbotemp<yg]<-"green"
plot(yr,needlerbotemp, ylab="", xlab="", col=colours, pch=19)
lines(yr,needlerbotemp)
abline(h=ry, col="red")
abline(h=yg, col="green")       
#legend("topleft", "Needler temp", bty="n")
mtext("Needler temperature (C)",side=2,line=2)
dev.off()     



_______________________________________________________________________

##crab (positive)


jpeg(filename="crab.jpg")
png(filename="crab.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(snow_c, probs=.33, na.rm=TRUE)
#yg<-quantile(snow_c, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(snow_c))
#colours[snow_c<ry]<-"red"
#colours[snow_c>yg]<-"green"
#plot(yr,snow_c, main="all data", ylab="snow_c CPUE", xlab="", col=colours, pch=19)
#lines(yr,snow_c)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("snow_c abundance index",side=2,line=2)

#2000s thresholds
ry<-quantile(snow_c[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(snow_c[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(snow_c))
colours[snow_c<ry]<-"red"
colours[snow_c>yg]<-"green"
plot(yr,snow_c, ylab="snowcrab index", xlab="", col=colours, pch=19)
lines(yr,snow_c)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("snow_c abundance index",side=2,line=2)
dev.off() 





_________________________________________________________________________

## cod (negative)
	
jpeg(filename="cod.jpg")
png(filename="cod.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#yg<-quantile(Cod_R, probs=.33, na.rm=TRUE)
#ry<-quantile(Cod_R, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(Cod_R))
#colours[Cod_R>ry]<-"red"
#colours[Cod_R<yg]<-"green"
#plot(yr,Cod_R, main="all data", ylab="Cod_R CPUE", xlab="", col=colours, pch=19)
#lines(yr,Cod_R)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("Cod_R recruitment index",side=2,line=2)
#2000s thresholds
yg<-quantile(Cod_R[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Cod_R[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(Cod_R))
colours[Cod_R>ry]<-"red"
colours[Cod_R<yg]<-"green"
plot(yr,Cod_R, ylab="Cod index", xlab="", col=colours, pch=19)
lines(yr,Cod_R)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("cod recruitment index",side=2,line=2)
dev.off()    




______________________________________________________________________________


##turbot (positive)
			
jpeg(filename="turbot.jpg")
png(filename="turbot.png")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(G_halibut, probs=.33, na.rm=TRUE)
#yg<-quantile(G_halibut, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(G_halibut))
#colours[G_halibut<ry]<-"red"
#colours[G_halibut>yg]<-"green"
#plot(yr,G_halibut, main="all data", ylab="G_halibut CPUE", xlab="", col=colours, pch=19)
#lines(yr,G_halibut)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("G_halibut abundance index",side=2,line=2)
#2000s thresholds
ry<-quantile(G_halibut[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(G_halibut[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("yellow", length(G_halibut))
colours[G_halibut<ry]<-"red"
colours[G_halibut>yg]<-"green"
plot(yr,G_halibut, ylab="Turbot index", xlab="", col=colours, pch=19)
lines(yr,G_halibut)
abline(h=ry, col="red")
abline(h=yg, col="green")       
mtext("greenland halibut abundance index",side=2,line=2)
dev.off() 








#################################################
#################################################

##Now characteristics
#chars<-read.csv("ess_16_CHARS.csv", header=TRUE)
chars<-read.csv("data/ess_2016.csv",header = TRUE)
attach(chars)
detach(chars)

ess<-read.csv("data/ess_2016.csv",header = TRUE)
attach(ess)

#################################################
###################################################################################################################################
#traffic light summaries and overall mean
#use function below to get percentiles in R
scale(BB_1,center=min(BB_1,na.rm=TRUE),scale=diff(range(BB_1,na.rm=TRUE)))

#do this for each column in the ess DF to make a new DF of percentiles
options(stringsAsFactors = FALSE)

pctls<-function(x){
  pctl<-scale(x,center=min(x,na.rm=TRUE),scale=diff(range(x,na.rm=TRUE)))
  return(pctl)
}

ess_pctls<-apply(ess[,-1], 2, pctls) #this works but it outputs as a matrix instead of a dataframe, so causes me trouble with calculations below.  How do I fix it?
ess_pctls<-cbind(ess$yr, ess_pctls)
ess_pctls<-as.data.frame(ess_pctls)
colnames(ess_pctls)[1] <- "yr"

#### Abundance Characteristic
Abund<-cbind(ess_pctls$yr, ess_pctls$RV_CPUE, ess_pctls$G_CPUE, ess_pctls$St_CPUE, ess_pctls$Trap_CPTH, (1- ess_pctls$RV_CV), ess_pctls$Comm_area)
Abund_pctls<-rowMeans(Abund[,-1], na.rm=TRUE)

#### Production Characteristic
Prod<-cbind(ess_pctls$yr, ess_pctls$RVSSB, ess_pctls$BB_1, ess_pctls$RV_2, ess_pctls$RV_4, ess_pctls$sex_mm, ess_pctls$max_mm, (1-ess_pctls$pred)) 
Prod_pctls<-rowMeans(Prod[,-1], na.rm=TRUE)

#### Fishing Effects Characteristic
Fish<-cbind(ess_pctls$yr, (1-ess_pctls$Exp_tot), (1-ess_pctls$Exp_fem), (1-ess_pctls$effort), ess_pctls$femcatch_prop, ess_pctls$fem_size, (1-ess_pctls$count))
Fish_pctls<-rowMeans(Fish[,-1], na.rm=TRUE)

#### Ecosystem Characteristic    Note: Needler Bottom Temp suggested for removal as per 2014 FW....DH kept in 2015, suggested keeping figure in 2016 documents for context - figure kept .  
Eco<-cbind(ess_pctls$yr, (1-ess_pctls$Rvbotemp), (1-ess_pctls$SSTs), (1-ess_pctls$Cod_R), ess_pctls$G_halibut, ess_pctls$snow_c)
Eco_pctls<-rowMeans(Eco[,-1], na.rm=TRUE)

#### Total Characteristics

Total<-cbind(Abund,Prod[,-1],Fish[,-1],Eco[,-1])
TotalTLA<-rowMeans(Total[,-1], na.rm=TRUE)

TLAtable<-cbind(ess_pctls$y, Abund_pctls,Prod_pctls,Fish_pctls,Eco_pctls,TotalTLA)
colnames(TLAtable)<-c("yr", "Abundance", "Productivity", "Fishing", "Ecosystem", "Mean")
TLAtable<-as.data.frame(TLAtable)

write.table(TLAtable, file="TLAtable_2016_Dec21.2016.txt")


####################################################

#now plot chars and total TLAs  

### Abundance ###

#jpeg(filename="abundance.jpg")
#png(filename="abundance.png")

jpeg(filename="summary_char_Dec14h.jpg", res=120)
png(filename="summary_char_Dec14h.png", res=120)

par(mfcol=c(5,1), oma=c(2,0.5,0.5,0.5), mar=c(0.2,0,2.0,0.2))

ry<-0.33#quantile(TLAtable$Abundance[TLAtable$yr>1999&TLAtable$yr<2011], probs=.33, na.rm=TRUE)
yg<-0.66#quantile(TLAtable$Abundance[TLAtable$yr>1999&TLAtable$yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(TLAtable$Abundance))
colours[TLAtable$Abundance<ry]<-"red"
colours[TLAtable$Abundance>yg]<-"green"
plot(TLAtable$yr,TLAtable$Abundance, xlab="", yaxt='n', xaxt='n', xlim=c(1985,2015), ylim=c(0,1), col=colours, pch=19, main="ABUNDANCE")
lines(TLAtable$yr,TLAtable$Abundance)
abline(h=ry, col="red",lty=2)
abline(h=yg, col="green")       
#legend("topleft", "ABUNDANCE", bty="n")
#dev.off()


### Production ###

#jpeg(filename="productivity.jpg")
#png(filename="productivity.png")
#par(mfcol=c(1,1), oma=c(0.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))

ry<-0.33#quantile(TLAtable$Productivity[TLAtable$yr>1999&TLAtable$yr<2011], probs=.33, na.rm=TRUE)
yg<-0.66#quantile(TLAtable$Productivity[TLAtable$yr>1999&TLAtable$yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(TLAtable$Productivity))
colours[TLAtable$Productivity<ry]<-"red"
colours[TLAtable$Productivity>yg]<-"green"
plot(TLAtable$yr,TLAtable$Productivity, xlab="", yaxt='n', xaxt='n', xlim=c(1985,2015), ylim=c(0,1), col=colours, pch=19,main="PRODUCTION")
lines(TLAtable$yr,TLAtable$Productivity)
abline(h=ry, col="red", lty=2)
abline(h=yg, col="green")       
#legend("topleft", "Productivity", bty="n")
#dev.off()


### Fishing ###

#jpeg(filename="fishing.jpg")
#png(filename="fishing.png")
#par(mfcol=c(1,1), oma=c(0.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))

ry<-0.33#quantile(TLAtable$Fishing Effects[TLAtable$yr>1999&TLAtable$yr<2011], probs=.33, na.rm=TRUE)
yg<-0.66#quantile(TLAtable$Fishing Effects[TLAtable$yr>1999&TLAtable$yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(TLAtable$Fishing))
colours[TLAtable$Fishing<ry]<-"red"
colours[TLAtable$Fishing>yg]<-"green"
plot(TLAtable$yr,TLAtable$Fishing, xlab="", yaxt='n', xaxt='n', xlim=c(1985,2015), ylim=c(0,1), col=colours, pch=19, main="FISHING EFFECTS")
lines(TLAtable$yr,TLAtable$Fishing)
abline(h=ry, col="red", lty=2)
abline(h=yg, col="green")       
#legend("topleft", "Fishing Effects", bty="n")
#dev.off()



### Ecosystem ###

#jpeg(filename="ecosystem.jpg")
#ng(filename="ecosystem.png")
#par(mfcol=c(1,1), oma=c(0.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))

ry<-0.33#quantile(TLAtable$Ecosystem[TLAtable$yr>1999&TLAtable$yr<2011], probs=.33, na.rm=TRUE)
yg<-0.66#quantile(TLAtable$Ecosystem[TLAtable$yr>1999&TLAtable$yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(TLAtable$Ecosystem))
colours[TLAtable$Ecosystem<ry]<-"red"
colours[TLAtable$Ecosystem>yg]<-"green"
plot(TLAtable$yr,TLAtable$Ecosystem, xlab="", yaxt='n', xaxt='n', xlim=c(1985,2015), ylim=c(0,1), col=colours, pch=19, main="ECOSYSTEM")
lines(TLAtable$yr,TLAtable$Ecosystem)
abline(h=ry, col="red", lty=2)
abline(h=yg, col="green")       
#legend("topleft", "Ecosystem", bty="n")
#dev.off()



### Mean Indicator ### 
#jpeg(filename="mean_indicator.jpg")
#png(filename="mean_indicator.png")
#par(mfcol=c(1,1), oma=c(0.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))

ry<-0.33#quantile(TLAtable$Mean Indicator[TLAtable$yr>1999&TLAtable$yr<2011], probs=.33, na.rm=TRUE)
yg<-0.66#quantile(TLAtable$Mean Indicator[TLAtable$yr>1999&TLAtable$yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(TLAtable$Mean))
colours[TLAtable$Mean<ry]<-"red"
colours[TLAtable$Mean>yg]<-"green"
plot(TLAtable$yr,TLAtable$Mean, xlab="", yaxt='n', xlim=c(1985,2015), ylim=c(0,1), col=colours, pch=19, main="OVERALL MEAN INDICATOR")
lines(TLAtable$yr,TLAtable$Mean)
abline(h=ry, col="red", lty=2)
abline(h=yg, col="green")       
#legend("topleft", "Mean Indicator", bty="n")
#dev.off() 







#################################################
#################################################

####################ABUNDANCE####################

jpeg(filename="abund_char.jpg")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
ry<-0.33
yg<-0.66
colours<-rep("yellow", length(Abundchar))
colours[Abundchar<ry]<-"red"
colours[Abundchar>yg]<-"green"
plot(yr,Abundchar, main="Abundance Chacteristic (all)", ylab="", xlab="", xaxt='n', col=colours, pch=19)
lines(yr,Abundchar)
abline(h=ry, col="red")
abline(h=yg, col="green")

colours<-rep("yellow", length(Abundchar_2000))
colours[Abundchar_2000<ry]<-"red"
colours[Abundchar_2000>yg]<-"green"
plot(yr,Abundchar_2000, main="Abundance Characteristic (2000-2010)", ylab="", xlab="", col=colours, pch=19)
lines(yr,Abundchar_2000)
abline(h=ry, col="red")
abline(h=yg, col="green")

dev.off()






#####################PRODUCTION###################

jpeg(filename="prod_char.jpg")

par(mfcol=c(1,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
ry<-0.33
yg<-0.66
colours<-rep("yellow", length(ProdChar))
colours[ProdChar<ry]<-"red"
colours[ProdChar>yg]<-"green"
plot(yr,ProdChar, main="Production Chacteristic (all)", ylab="", xlab="", xaxt='n', col=colours, pch=19)
lines(yr,ProdChar)
abline(h=ry, col="red")
abline(h=yg, col="green")
colours<-rep("yellow", length(ProdChar_2000))
colours[ProdChar_2000<ry]<-"red"
colours[ProdChar_2000>yg]<-"green"
plot(yr,ProdChar_2000, main="Production Characteristic (2000-2010)", ylab="", xlab="", col=colours, pch=19)
lines(yr,ProdChar_2000)
abline(h=ry, col="red")
abline(h=yg, col="green")

dev.off()




###################FISHING EFFECTS##############

jpeg(filename="fish_char.jpg")

par(mfcol=c(2,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
ry<-0.33
yg<-0.66
colours<-rep("yellow", length(FishChar))
colours[FishChar<ry]<-"red"
colours[FishChar>yg]<-"green"
plot(yr,FishChar, main="Fishing Effects Chacteristic (all)", ylab="", xlab="", xaxt='n', col=colours, pch=19)
lines(yr,FishChar)
abline(h=ry, col="red")
abline(h=yg, col="green")
colours<-rep("yellow", length(FishChar_2000))
colours[FishChar_2000<ry]<-"red"
colours[FishChar_2000>yg]<-"green"
plot(yr,FishChar_2000, main="Fishing Effects Characteristic (2000-2010)", ylab="", xlab="", col=colours, pch=19)
lines(yr,FishChar_2000)
abline(h=ry, col="red")
abline(h=yg, col="green")

dev.off()





#####################ECOSYSTEM####################

jpeg(filename="eco_char.jpg")

par(mfcol=c(2,1), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
ry<-0.33
yg<-0.66
colours<-rep("yellow", length(EcoChar))
colours[EcoChar<ry]<-"red"
colours[EcoChar>yg]<-"green"
plot(yr,EcoChar, main="Ecosystem Chacteristic (all)", ylab="", xlab="", xaxt='n', col=colours, pch=19)
lines(yr,EcoChar)
abline(h=ry, col="red")
abline(h=yg, col="green")
colours<-rep("yellow", length(EcoChar2000))
colours[EcoChar2000<ry]<-"red"
colours[EcoChar2000>yg]<-"green"
plot(yr,EcoChar2000, main="Ecosystem Characteristic (2000-2010)", ylab="", xlab="", col=colours, pch=19)
lines(yr,EcoChar2000)
abline(h=ry, col="red")
abline(h=yg, col="green")

dev.off()








########################################

########################################

########################################

##############################################################################
##   Indicator Quantile Composite Plot 
########################################


########################################



ess<-read.csv("data/ess_2016.csv", header=T)
attach(ess)




jpeg(filename="indicator_composite_Dec.14.2016h.jpeg", res=90)
png(filename="indicator_composite_Dec.14.2016.png", res=90)
#Set up a big plot
par(mfcol=c(8,3), oma=c(2.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))
## add Effort (Fishery dependent index of exploitation)
## add Trap CPUE 
## add Needler botemp - not used in 2016
## add 

##SURVEY CPUE   

#jpeg(filename="survey_cpue.jpg")
#par(mfcol=c(1,2), oma=c(2.5,3.5,2.5,0.5), mar=c(1.5,0,1.5,0.5))
#full dataset      
#ry<-quantile(RV_CPUE, probs=.33, na.rm=TRUE)
#yg<-quantile(RV_CPUE, probs=.66, na.rm=TRUE) 
#colours<-rep("yellow", length(RV_CPUE))
#colours[RV_CPUE<ry]<-"red"
#colours[RV_CPUE>yg]<-"green"
#plot(yr,RV_CPUE, main="all data", ylab="survey CPUE", xlab="", col=colours, pch=19)
#lines(yr,RV_CPUE)
#abline(h=ry, col="red")
#abline(h=yg, col="green")
#mtext("survey cpue (kg/30 min)",side=2,line=2)
#2000s thresholds



#jpeg(filename="survey_cpue.jpg")
#par(mfcol=c(1,1), oma=c(2.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))
ry<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(RV_CPUE))
colours[RV_CPUE<ry]<-"red"
colours[RV_CPUE>yg]<-"green"
plot(yr,RV_CPUE,  ylab="Survey CPUE", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,RV_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "survey", bty="n")
#dev.off()    

##Gulf CPUE (gfcpue)
#2000s thresholds
ry<-quantile(G_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(G_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(G_CPUE))
colours[G_CPUE<ry]<-"red"
colours[G_CPUE>yg]<-"green"
plot(yr,G_CPUE, ylab="Gulf CPUE", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,G_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "gulf", bty="n")
#dev.off()    

## Standardised CPUE (stcpue)
#2000s thresholds
ry<-quantile(St_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(St_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(St_CPUE))
colours[St_CPUE<ry]<-"red"
colours[St_CPUE>yg]<-"green"
plot(yr,St_CPUE, ylab="Standardised CPUE", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,St_CPUE)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "std cpue", bty="n")
#dev.off()    

## TRAP Standardised CPUE (Trap_CPTH)
#2000s thresholds
ry<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Trap_CPTH))
colours[Trap_CPTH<ry]<-"red"
colours[Trap_CPTH>yg]<-"green"
plot(yr,Trap_CPTH, ylab="Trap CPTH", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,Trap_CPTH)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "trap cpue", bty="n")
#dev.off()  

##RV Coefficient of Variation (rvcv) 
#REVERSE POLARITY - swap yg for ry and reverse teh < and > symbols for plots where high is bad and low is good (red)

#2000s thresholds
yg<-quantile(RV_CV[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(RV_CV[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(RV_CV))
colours[RV_CV>ry]<-"red"
colours[RV_CV<yg]<-"green"
plot(yr,RV_CV, xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,RV_CV)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "survey cv", bty="n")
#dev.off()    

#commercial fishing area - positive polarity (area)
#2000s thresholds
ry<-quantile(Comm_area[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(Comm_area[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Comm_area))
colours[Comm_area<ry]<-"red"
colours[Comm_area>yg]<-"green"
plot(yr,Comm_area, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,Comm_area)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "comm. area", bty="n")
#dev.off()    

##spawning stock biomass (ssb) positive polarity
#2000s thresholds
ry<-quantile(RVSSB[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RVSSB[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(RVSSB))
colours[RVSSB<ry]<-"red"
colours[RVSSB>yg]<-"green"
plot(yr,RVSSB, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,RVSSB)
abline(h=ry, col="red")
abline(h=yg, col="green")             
legend("topleft", "ssb", bty="n")
#dev.off()    

##bellybag (bb) - positive
#2000s thresholds
ry<-quantile(BB_1[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(BB_1[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(BB_1))
colours[BB_1<ry]<-"red"
colours[BB_1>yg]<-"green"
plot(yr,BB_1, ylab="Bellybag Index", xlab="", yaxt='n', col=colours, pch=19)
lines(yr,BB_1)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "bellybag", bty="n")
#dev.off()    

##age-s abundance (rv2)
#2000s thresholds
ry<-quantile(RV_2[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_2[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(RV_2))
colours[RV_2<ry]<-"red"
colours[RV_2>yg]<-"green"
plot(yr,RV_2, ylab="Age 2 index", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,RV_2)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "age 2", bty="n")
#dev.off()    

##age-4 abundance index (rv4) - positive
#2000s thresholds
ry<-quantile(RV_4[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(RV_4[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(RV_4))
colours[RV_4<ry]<-"red"
colours[RV_4>yg]<-"green"
plot(yr,RV_4, ylab="Age 4 index", xlab="", yaxt='n',  xaxt='n',col=colours, pch=19)
lines(yr,RV_4)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "age 4", bty="n")
#dev.off() 

### size at sex trans (sexmm)
#2000s thresholds
ry<-quantile(sex_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(sex_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(sex_mm))
colours[sex_mm<ry]<-"red"
colours[sex_mm>yg]<-"green"
plot(yr,sex_mm, ylab="Sex transition (mm)", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,sex_mm)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "sex trans mm", bty="n")
#dev.off() 

### maximum size (maxmm)
#2000s thresholds
ry<-quantile(max_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(max_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(max_mm))
colours[max_mm<ry]<-"red"
colours[max_mm>yg]<-"green"
plot(yr,max_mm, ylab="maximum length (mm)", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,max_mm)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "max mm", bty="n")
#dev.off() 

### predation (confirmed shrimp predators) - negative
#2000s thresholds
yg<-quantile(pred[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(pred[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(pred))
colours[pred>ry]<-"red"
colours[pred<yg]<-"green"
plot(yr,pred, ylab="Predator abundance index", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,pred)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "predation", bty="n")
#dev.off()    

### count  - NEGATIVE
#2000s thresholds
yg<-quantile(count[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(count[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(count))
colours[count>ry]<-"red"
colours[count<yg]<-"green"
plot(yr,count, ylab="count", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,count)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "count", bty="n")
#dev.off()    

## exploitation (NEGATIVE - exp)
#2000s thresholds
yg<-quantile(Exp_tot[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Exp_tot[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Exp_tot))
colours[Exp_tot>ry]<-"red"
colours[Exp_tot<yg]<-"green"
plot(yr,Exp_tot,  ylab="Total Exploitation", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,Exp_tot)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "exploitation", bty="n")
#dev.off()    

## female exploitation index (NEGATIVE - femexp)
#2000s thresholds
yg<-quantile(Exp_fem[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Exp_fem[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Exp_fem))
colours[Exp_fem>ry]<-"red"
colours[Exp_fem<yg]<-"green"
plot(yr,Exp_fem, ylab="Female Exploitation", xlab="", yaxt='n', col=colours, pch=19)
lines(yr,Exp_fem)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "fem exploitation", bty="n")
#dev.off()    

## Effort fishery dependent exploitation index (NEGATIVE)
#2000s thresholds
yg<-quantile(effort[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(effort[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(effort))
colours[effort>ry]<-"red"
colours[effort<yg]<-"green"
plot(yr,effort, ylab="Effort", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,effort)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "effort", bty="n")
#dev.off()    

## proportion of females in the catch (POSITIVE) 
#2000s thresholds
ry<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(femcatch_prop))
colours[femcatch_prop<ry]<-"red"
colours[femcatch_prop>yg]<-"green"
plot(yr,femcatch_prop, ylab="Proportion females in catch", xlab="", yaxt='n',  xaxt='n', col=colours, pch=19)
lines(yr,femcatch_prop)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "fem. catch", bty="n")
#dev.off() 

## female size (femsize) - positive
#2000s thresholds
ry<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(ess$fem_size))
colours[ess$fem_size<ry]<-"red"
colours[ess$fem_size>yg]<-"green"
plot(yr,ess$fem_size, ylab="average female size (mm)", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,ess$fem_size)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "fem. mm", bty="n")
#dev.off() 

##Population Age-length evenness (popeven) - POSITIVE
####Has this been removed?

#2000s thresholds
#ry<-quantile(popeven[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
#yg<-quantile(popeven[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
#colours<-rep("darkgoldenrod1", length(popeven))
#colours[popeven<ry]<-"red"
#colours[popeven>yg]<-"green"
#plot(yr,popeven, main= "2000-2010", ylab="", xlab="", yaxt='n', col=colours, pch=19)
#lines(yr,popeven)
#abline(h=ry, col="red")
#abline(h=yg, col="green")       

#dev.off() 

##bottom temperatures (botemp - NEGATIVE) 
#2000s thresholds
yg<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Rvbotemp))
colours[Rvbotemp>ry]<-"red"
colours[Rvbotemp<yg]<-"green"
plot(yr,Rvbotemp, ylab="RV Bottom Temperature (C)", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,Rvbotemp)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "Survey temp", bty="n")
#dev.off()   

##Needler bottom temperatures (botemp - NEGATIVE) - NOTE: Removed as indice following 2014 framework. 
#2000s thresholds
#yg<-quantile(needlerbotemp[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
#ry<-quantile(needlerbotemp[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
#colours<-rep("darkgoldenrod1", length(needlerbotemp))
#colours[needlerbotemp>ry]<-"red"
#colours[needlerbotemp<yg]<-"green"
#plot(yr,needlerbotemp, ylab="RV Bottom Temperature (C)", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
#lines(yr,needlerbotemp)
#abline(h=ry, col="red")
#abline(h=yg, col="green")       
#legend("topleft", "Needler temp", bty="n")
#dev.off()     

##spring sea surface temperature (surtemp) NEGATIVE
#2000s thresholds
yg<-quantile(SSTs[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(SSTs[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(SSTs))
colours[SSTs>ry]<-"red"
colours[SSTs<yg]<-"green"
plot(yr,SSTs, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,SSTs)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "SST", bty="n")
#dev.off()    

##crab (positive)
#2000s thresholds
ry<-quantile(snow_c[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(snow_c[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(snow_c))
colours[snow_c<ry]<-"red"
colours[snow_c>yg]<-"green"
plot(yr,snow_c, ylab="snowcrab index", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,snow_c)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "snowcrab", bty="n")
#dev.off() 

## cod (negative)
#2000s thresholds
yg<-quantile(Cod_R[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
ry<-quantile(Cod_R[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(Cod_R))
colours[Cod_R>ry]<-"red"
colours[Cod_R<yg]<-"green"
plot(yr,Cod_R, ylab="Cod index", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
lines(yr,Cod_R)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "cod", bty="n")

##turbot (positive)

ry<-quantile(G_halibut[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
yg<-quantile(G_halibut[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
colours<-rep("darkgoldenrod1", length(G_halibut))
colours[G_halibut<ry]<-"red"
colours[G_halibut>yg]<-"green"
plot(yr,G_halibut, ylab="Turbot index", xlab="", yaxt='n',  col=colours, pch=19)
lines(yr,G_halibut)
abline(h=ry, col="red")
abline(h=yg, col="green")       
legend("topleft", "turbot", bty="n")


dev.off() 

##############################################################################
####################### MEAN INDICATOR #######################################

overall<-mean(EcoChar,FishChar,ProdChar,Abundchar, na.rm=TRUE)
overall_2000<-


	#CALC Mix Analysis - too hard to code for 2015
	
	#CALC Age2 - too hard to code for 2015

	#CALC Age4 - too hard to code for 2015
	
	#CALC SSB
		
	#run dens_fem_trans_forBIOSurvey.sql, format and save .csv for 2014
	
	SurveyCatchStd.2016<-read.csv("SurveyFemDens.csv",head=T)

	SurveyCatchStd.2016
	
	#CALC SSB bootstrap CIs
	
	#PLOT Precautionary Approach
	
	#TABLE population size index by age/stage
	
	#IDEA all population only by BB, age 2, male, trans, primP, mutliP?
	#IDEA Kilada aging against MIX analysis


	
	
	################ Population #############################################

	#CALC totpop for LFs from Survey (BB, MultiP, PrimiP, Trans, Males(rest))
	
	#PLOT pop LFs by year, multipanel
	
	#CALC port sample LFs
	
	#PLOT port sample LFs
	
	#PLOT year t, t-1, t-2 LFs over time series plot 4+, PrimiP, MultiP


	
	
	################ Fishing Mortlaity ######################################
	#CALC Effort (hours)
	
	#CALC Landings
	
	#CALC Tot Expl
	
	#CALC Fem Expl
	
	#TABLE Tot Expl with Biomass by stratum


	
	
	############### Ecosystem ################################################
	#CALC turbot (what size is right size - DOESN'T eat shrimp)
	
	#CALC cod (what size is right size - DOES eat shrimp)
	
	#CALC predators (refine)
	
	#PLOT predators
	
	#CALC sst
	
	#CALC bottomtemp
	
	#PLOT sst, bottomtemp, predators
	
	#IDEA snowcrab temp


	
	
	
################ other plots ###########################################
###get effort
				
effort.qry<-paste("select to_char(fdate,'YYYY') YYYY, 
sum (fhours)/1000				
from shrcomlog				
where btype in (1,3)				
group by to_char(fdate,'YYYY')				
order by to_char(fdate, 'YYYY')")
effort.dat<-sqlQuery(ch,effort.qry)
effort.dat



#for 2015 I manually pasted the effort into ess_2015.csv, but work on merging these data into the dataframe throughout this script.
aggd_shrimp<-read.csv("ess_2016.csv",header = TRUE)
head(aggd_shrimp)


	#PLOT landings vs effort (does it take more to land more?, of course it does!)
	plot(aggd_shrimp$effort,aggd_shrimp$la,xlim=c(500,1800), ylim=c(1800,6000), ylab = "landings",xlab = "Effort (1000s of hours)",lwd = 2,main = "Landings vs Effort")
	alist = lm(formula = aggd_shrimp$la~aggd_shrimp$effort)
	ab = as.numeric(alist$coefficients)
	cmod = seq(568,1750,.01)
	lines(cmod,ab[1] + ab[2]*cmod,col = 2)
	text(aggd_shrimp$effort, aggd_shrimp$la, aggd_shrimp$yr, cex=0.8, pos=4, col="red") 
	
	#PLOT landings vs biomass phase plot
	hrelf = aggd_shrimp$la/aggd_shrimp$svyb
	plot(aggd_shrimp$svyb,hrelf, xlab = "biomass",ylab = "Relative F (landings/survey biomass)",lwd = 1,main = "RelF vs Biomass")
	lines(aggd_shrimp$svyb,hrelf)
	text(aggd_shrimp$svyb,hrelf, aggd_shrimp$yr, cex=0.9, col="black")
	
	#PLOT effort vs biomass phase plot
	plot(aggd_shrimp$svyb,aggd_shrimp$effort, xlab = "biomass",ylab = "Effort (1000s of hours)",lwd = 1,main = "Effort vs Biomass")
	lines(aggd_shrimp$svyb,aggd_shrimp$effort)
	text(aggd_shrimp$svyb,aggd_shrimp$effort, aggd_shrimp$yr, cex=0.9, col="black")
	

	
	
	
	
#PLOT survey vs commercial CPUE with outlier consideration (unusual residual)
		
	lm_gf<-lm(gfcpue~survey)
	summary(lm_gf)
	
	lm_st<-lm(stcpue~survey)
	summary(lm_st)
	
	plot(gfcpue ~ survey,
		col="red")
		points(stcpue~survey,
		col="blue")
		survey_fit<-survey[!is.na(survey)]
		lines(survey_fit, fitted(lm_gf), col="red")
		lines(survey, fitted(lm_st), col="blue") #problem here is that lm_st is only 21 long, whereas survey is 28 (which just happens to match gf, which is the only reason it worked above)
	#OR, from aggd_dat_shrimp_plus_trap
	surv<-2*aggd_shrimp$RV_CPUE
	plot(2*aggd_shrimp$RV_CPUE,aggd_shrimp$St_CPUE,xlab = "Survey CPUE (kg/hour)",ylab = "Standaridized CPUE (kg/hour)",lwd = 2,main = "Commercial vs. Commercial CPUE")
	#lines(c(0,4000),c(1,1),lty = "dotted")
	alist = lm(formula = aggd_shrimp$St_CPUE~ surv)
	ab = as.numeric(alist$coefficients)
	cmod = seq(100,700,.01)
	lines(cmod,ab[1] + ab[2]*cmod,col = 2)
	
	#PLOT spawner-recruit
	jpeg(filename="bb_ssb.jpg")
	ssb_pred<-ssb[20:32]
	bb_pred<-bb[21:33]
	plot(bb_pred~ssb_pred,
		xlab="spawning stock biomass index (mt in year t)",
		ylab="bellybag index (millions in year t+1)")
	dev.off()	


	
	
	
	############CORRELATIONS##############################
	corr_ess<-ess[,2:26]
	corr_mtrx<-cor(corr_ess, use="complete.obs", method="kendall") 
	corr_mtrx
	summary(corr_mtrx)
	write.table(corr_mtrx,file="ess_correlation_matrix.txt") 
	
	
	
	########################################################
	
	
	
	
	
	
	
	
	
	########################################################
	########################################################
	
### Produce paneled indicator figure with columns representing indicators contributing to each characteristic, 
### and for accessability add textured lines for seperation between red and green zones. Landscape layout, increase resolution if possible. 
	
	
	ess<-read.csv("data/ess_2016.csv", header=T)
	attach(ess)
	
	
	jpeg(filename="indicator_composite_Dec.20.2016_accessibility.jpeg", res=90)
	png(filename="indicator_composite_Dec.20.2016_accessibility", res=90)
	#Set up a big plot
	par(mfcol=c(7,4), oma=c(2.5,0.5,0.5,0.5), mar=c(0.2,0,1.5,0.2))
	
###	NOTE  ###
	## add Effort (Fishery dependent index of exploitation)
	## add Trap CPUE 
	## Needler botemp - not used in 2016
	## Pop Age Length Evenness - not used in 2016. 
	## Capelin Abundance - not used since 2015. 
	
	### ABUNDANCE ####
	
	##SURVEY CPUE   
	#jpeg(filename="survey_cpue.jpg")
	#par(mfcol=c(1,1), oma=c(2.5,0.5,0.5,0.5), mar=c(0.2,0,0,0.2))
	ry<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(RV_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(RV_CPUE))
	colours[RV_CPUE<ry]<-"red"
	colours[RV_CPUE>yg]<-"green"
	plot(yr,RV_CPUE,  ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19, main="ABUNDANCE")
	lines(yr,RV_CPUE)
	abline(h=ry, col="red",lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Survey CPUE", bty="n")
	
	
	#dev.off()    
	
	##Gulf CPUE (gfcpue)
	#2000s thresholds
	ry<-quantile(G_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(G_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(G_CPUE))
	colours[G_CPUE<ry]<-"red"
	colours[G_CPUE>yg]<-"green"
	plot(yr,G_CPUE, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,G_CPUE)
	abline(h=ry, col="red",lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Gulf CPUE", bty="n")
	#dev.off()    
	
	## Standardised CPUE (stcpue)
	#2000s thresholds
	ry<-quantile(St_CPUE[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(St_CPUE[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(St_CPUE))
	colours[St_CPUE<ry]<-"red"
	colours[St_CPUE>yg]<-"green"
	plot(yr,St_CPUE, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,St_CPUE)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Std. CPUE", bty="n")
	#dev.off()    
	
	## TRAP Standardised CPUE (Trap_CPTH)
	#2000s thresholds
	ry<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(Trap_CPTH[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Trap_CPTH))
	colours[Trap_CPTH<ry]<-"red"
	colours[Trap_CPTH>yg]<-"green"
	plot(yr,Trap_CPTH, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,Trap_CPTH)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Trap CPUE", bty="n")
	#dev.off()  
	
	
	##RV Coefficient of Variation (rvcv) 
	#REVERSE POLARITY - swap yg for ry and reverse teh < and > symbols for plots where high is bad and low is good (red)
		#2000s thresholds
	yg<-quantile(RV_CV[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(RV_CV[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(RV_CV))
	colours[RV_CV>ry]<-"red"
	colours[RV_CV<yg]<-"green"
	plot(yr,RV_CV, xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,RV_CV)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Survey CV", bty="n")
	#dev.off()  
	
	
	
	#commercial fishing area - positive polarity (area)
	#2000s thresholds
	ry<-quantile(Comm_area[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(Comm_area[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Comm_area))
	colours[Comm_area<ry]<-"red"
	colours[Comm_area>yg]<-"green"
	plot(yr,Comm_area, ylab="", xlab="", yaxt='n', col=colours, pch=19)
	lines(yr,Comm_area)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Comm. Area", bty="n")
	#dev.off()  
	
	
		#### skip a position in panelled figure. 
	plot.new()	
	
	
	
	
	
	
	### PRODUCTION  ###
	
	##spawning stock biomass (ssb) positive polarity
	#2000s thresholds
	ry<-quantile(RVSSB[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(RVSSB[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(RVSSB))
	colours[RVSSB<ry]<-"red"
	colours[RVSSB>yg]<-"green"
	plot(yr,RVSSB, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19, main="PRODUCTION")
	lines(yr,RVSSB)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")             
	legend("topleft", "SSB", bty="n")
	#dev.off()    
	
	
	##bellybag (bb) - positive
	#2000s thresholds
	ry<-quantile(BB_1[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(BB_1[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(BB_1))
	colours[BB_1<ry]<-"red"
	colours[BB_1>yg]<-"green"
	plot(yr,BB_1, ylab="", xlab="", xaxt='n', yaxt='n', col=colours, pch=19)
	lines(yr,BB_1)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Bellybag", bty="n")
	#dev.off()  
	
	
	##age-A abundance (rv2)
	#2000s thresholds
	ry<-quantile(RV_2[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(RV_2[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(RV_2))
	colours[RV_2<ry]<-"red"
	colours[RV_2>yg]<-"green"
	plot(yr,RV_2, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,RV_2)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Age-2", bty="n")
	#dev.off()  
	
	
	##age-4 abundance index (rv4) - positive
	#2000s thresholds
	ry<-quantile(RV_4[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(RV_4[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(RV_4))
	colours[RV_4<ry]<-"red"
	colours[RV_4>yg]<-"green"
	plot(yr,RV_4, ylab="", xlab="", yaxt='n',  xaxt='n',col=colours, pch=19)
	lines(yr,RV_4)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Age-4", bty="n")
	#dev.off() 
	
	
	### size at sex trans (sexmm)
	#2000s thresholds
	ry<-quantile(sex_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(sex_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(sex_mm))
	colours[sex_mm<ry]<-"red"
	colours[sex_mm>yg]<-"green"
	plot(yr,sex_mm, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,sex_mm)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Mean Length Sex Trans.", bty="n")
	#dev.off() 
	
	
	### maximum size (maxmm)
	#2000s thresholds
	ry<-quantile(max_mm[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(max_mm[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(max_mm))
	colours[max_mm<ry]<-"red"
	colours[max_mm>yg]<-"green"
	plot(yr,max_mm, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,max_mm)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Max Length", bty="n")
	#dev.off() 
	
	
	### predation (confirmed shrimp predators) - negative
	#2000s thresholds
	yg<-quantile(pred[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(pred[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(pred))
	colours[pred>ry]<-"red"
	colours[pred<yg]<-"green"
	plot(yr,pred, ylab="", xlab="", yaxt='n', col=colours, pch=19)
	lines(yr,pred)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Predation", bty="n")
	#dev.off()   
	
	
	
	
	
	### FISHING EFFECTS  ###
	
	
	### count  - NEGATIVE
	#2000s thresholds
	yg<-quantile(count[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(count[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(count))
	colours[count>ry]<-"red"
	colours[count<yg]<-"green"
	plot(yr,count, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19, main="FISHING EFFECTS")
	lines(yr,count)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Comm. Count", bty="n")
	#dev.off()   
	
	
	## exploitation (NEGATIVE - exp)
	#2000s thresholds
	yg<-quantile(Exp_tot[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(Exp_tot[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Exp_tot))
	colours[Exp_tot>ry]<-"red"
	colours[Exp_tot<yg]<-"green"
	plot(yr,Exp_tot,  ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,Exp_tot)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Exploitation", bty="n")
	#dev.off()  
	
	
	## female exploitation index (NEGATIVE - femexp)
	#2000s thresholds
	yg<-quantile(Exp_fem[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(Exp_fem[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Exp_fem))
	colours[Exp_fem>ry]<-"red"
	colours[Exp_fem<yg]<-"green"
	plot(yr,Exp_fem, ylab="", xlab="", xaxt='n', yaxt='n', col=colours, pch=19)
	lines(yr,Exp_fem)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Fem. Exploitation", bty="n")
	#dev.off()   
	
	
	## Effort fishery dependent exploitation index (NEGATIVE)
	#2000s thresholds
	yg<-quantile(effort[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(effort[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(effort))
	colours[effort>ry]<-"red"
	colours[effort<yg]<-"green"
	plot(yr,effort, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,effort)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Effort", bty="n")
	#dev.off()   
	
	
	## proportion of females in the catch (POSITIVE) 
	#2000s thresholds
	ry<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(femcatch_prop[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(femcatch_prop))
	colours[femcatch_prop<ry]<-"red"
	colours[femcatch_prop>yg]<-"green"
	plot(yr,femcatch_prop, ylab="", xlab="", yaxt='n',  xaxt='n', col=colours, pch=19)
	lines(yr,femcatch_prop)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Fem. Catch Prop.", bty="n")
	#dev.off() 
	
	
	## female size (femsize) - positive
	#2000s thresholds
	ry<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(ess$fem_size[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(ess$fem_size))
	colours[ess$fem_size<ry]<-"red"
	colours[ess$fem_size>yg]<-"green"
	plot(yr,ess$fem_size, ylab="", xlab="", yaxt='n', col=colours, pch=19)
	lines(yr,ess$fem_size)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Fem. Mean Length", bty="n")
	#dev.off() 
	
	
	### Add empty plot to fill column
	plot.new()
	
	
	
	#### ECOSYSTEM ###
	
	##bottom temperatures (botemp - NEGATIVE) 
	#2000s thresholds
	yg<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(Rvbotemp[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Rvbotemp))
	colours[Rvbotemp>ry]<-"red"
	colours[Rvbotemp<yg]<-"green"
	plot(yr,Rvbotemp, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19, main="ECOSYSTEM")
	lines(yr,Rvbotemp)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Survey Temp.", bty="n")
	#dev.off()   
	

	##spring sea surface temperature (surtemp) NEGATIVE
	#2000s thresholds
	yg<-quantile(SSTs[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(SSTs[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(SSTs))
	colours[SSTs>ry]<-"red"
	colours[SSTs<yg]<-"green"
	plot(yr,SSTs, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,SSTs)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "SST", bty="n")
	#dev.off()    
	

	## cod (negative)
	#2000s thresholds
	yg<-quantile(Cod_R[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	ry<-quantile(Cod_R[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(Cod_R))
	colours[Cod_R>ry]<-"red"
	colours[Cod_R<yg]<-"green"
	plot(yr,Cod_R, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,Cod_R)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Cod Recruit.", bty="n")	
	
	
	##crab (positive)
	#2000s thresholds
	ry<-quantile(snow_c[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(snow_c[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(snow_c))
	colours[snow_c<ry]<-"red"
	colours[snow_c>yg]<-"green"
	plot(yr,snow_c, ylab="", xlab="", yaxt='n', xaxt='n', col=colours, pch=19)
	lines(yr,snow_c)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Snow Crab Recruit.", bty="n")
	#dev.off() 
	
	
	##turbot (positive)
	ry<-quantile(G_halibut[yr>1999&yr<2011], probs=.33, na.rm=TRUE)
	yg<-quantile(G_halibut[yr>1999&yr<2011], probs=.66, na.rm=TRUE) 
	colours<-rep("darkgoldenrod1", length(G_halibut))
	colours[G_halibut<ry]<-"red"
	colours[G_halibut>yg]<-"green"
	plot(yr,G_halibut, ylab="", xlab="", yaxt='n',  col=colours, pch=19)
	lines(yr,G_halibut)
	abline(h=ry, col="red", lty=2)
	abline(h=yg, col="green")       
	legend("topleft", "Turbot Abundance", bty="n")
	
	
	#dev.off() 
	
	
	
	
	
	#####################
	#####################
	
	Characteristic Summary Panel - Accessability
	
	
	
	
