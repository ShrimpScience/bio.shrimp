#' @title 4a.Survey Temp
#' @description Calculates an annual mean of survey bottom temperatures as an indicator of the Ecosystem Characteristics in our assessment 
#' @param sfa defines the specific SFA for the survey
#' @param yrs is the years to estimate
#' @param mnts months of available data, defaults to the full year
#' @return data.frame of survey data called 'shrimp.survey'
#' 
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
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
### Start: July 20, 2018 
require(bio.shrimp)

#SURVEY DETAILS:
#1 nautical mile = 1852 metres
#1.25 nautical miles travelled during 30 minute tow at 2.5 knots
#standard unit = 1.25 nm x 17.4 m = 40,281 m2
strata.area.data<-read.csv("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Survey/Strata.Surface.Area.csv",header=T)
strata.area.data$Num_Units<-((strata.area.data$km2/40281)*1000000)

################################ Survey Bottom Temperatures ################################## 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
write.csv(shrimp.survey,paste("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/ShrimpSurvey.Data",Sys.Date(),".csv",sep=""), row.names=F)
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 
#  61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58 
#2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
#  60   60   60   60   60   60   60   60   60   60   60   60   60 

#Select valid survey sets only (SETCODE = 1 and 2)
surv.btemp<-subset(shrimp.survey,SETCODE %in% c(1,2))
#CALC shrimp survey mean bottom temperature by STRATUM
svy.BT.strat<-ddply(surv.btemp,.(YEAR,STRATUM),summarize,MEAN_TEMP=mean(TEMP, na.rm=T))
head(svy.BT.strat) 
#  YEAR STRATUM MEAN_TEMP
#1 1982      13  3.666667
#2 1982      14  1.750000
#3 1982      15  2.941667
#4 1983      13  4.400000
#5 1983      14       NaN
#6 1983      15  2.825000

ann.svy.BT<-ddply(surv.btemp,.(YEAR),summarize,MEAN_TEMP= mean(TEMP, na.rm=T))
head(ann.svy.BT) 
#  YEAR MEAN_TEMP
#1 1982  2.825000
#2 1983  3.140000
#3 1984  4.300000
#4 1985  3.362963
#5 1986  3.610526
#6 1987  2.496364 

#PLOT survey bottom temperature by stratum area:
bt.temp<-expand.grid.df(data.frame(YEAR=1982:2018),data.frame(STRATUM=c(13,14,15,17)))
survey.bt.strat<-merge(svy.BT.strat,bt.temp,id.vars=c('YEAR','STRATUM'),all=TRUE)

ggplot(survey.bt.strat,aes(x=YEAR,y=MEAN_TEMP)) + geom_line() + 
  geom_point() + scale_x_continuous(name="YEAR",breaks=seq(1982, 2018, 5)) +
  scale_y_continuous(name="Survey Bottom Temperatures", breaks=seq(0,10,2)) +
  theme_bw() + ggtitle("Survey Mean Bottom Temperature by Stratum") + facet_wrap(~STRATUM) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#PLOT survey BT by year:
bt.temp.2<-expand.grid.df(data.frame(YEAR=1982:2018))
ann.survey.bt<-merge(ann.svy.BT,bt.temp.2,id.vars=c('YEAR'),all=TRUE)

jpeg(filename="C://Users//cassistadarosm//Documents//GitHub//bio.shrimp//Figures//1d.survey_bt.2018.jpg")
yg<-quantile(ann.svy.BT$MEAN_TEMP[ann.svy.BT$YEAR>1999&ann.svy.BT$YEAR<2011], probs=.33, na.rm=TRUE)
ry<-quantile(ann.svy.BT$MEAN_TEMP[ann.svy.BT$YEAR>1999&ann.svy.BT$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.survey.bt, aes(YEAR,MEAN_TEMP)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = yg), fill = "green", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =2.300026 , ymax = ry), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 2.504458, ymax = Inf), fill = "red", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.svy.BT$YEAR), max(ann.svy.BT$YEAR), 5)) +
  scale_y_continuous(name="Bottom Temperature") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 
