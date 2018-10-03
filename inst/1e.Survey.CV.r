#' @title 1d.Survey.CV
#' @description Calculates Survey an annual coefficient of variation indicator as part of the Abundance Characteristics in our assessment 
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
#' @importFrom reshape expand.grid.df
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


################################ Survey Coefficient of Variation ################################## 
#Survey data query:
shrimp.db('survey.redo', oracle.username=oracle.username, oracle.password = oracle.password)
shrimp.db('survey', oracle.username=oracle.username, oracle.password = oracle.password)
str(shrimp.survey) #1952 RECORDS
head(shrimp.survey)

#TABLE DATA:
#Number of stations per year the survey has been running
table(shrimp.survey$YEAR)
#1982 1983 1984 1985 1986 1987 1988 1993 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
#  61   44   67   57   61   69   66   40   51   71   69   71   63   59   75   60   69   61   58   60 
#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
#  60   60   60   60   60   60   60   60   60   60   60   60

#Select valid survey sets only (SETCODE = 1 and 2)
vsurv.set<-subset(shrimp.survey,SETCODE %in% c(1,2))
#CALC shrimp survey CV by STRATUM (consider snowcrab CV?)
svy.CV.strat<-ddply(vsurv.set,.(YEAR,STRATUM),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
head(svy.CV.strat) 
#  YEAR STRATUM  COEF_VAR
#1 1982      13 105.28436
#2 1982      14  64.14700
#3 1982      15  73.58836
#4 1983      13  68.58501
#5 1983      14  81.34083
#6 1983      15  73.00175

ann.svy.CV<-ddply(vsurv.set,.(YEAR),summarize,COEF_VAR=(sd(WEIGHT+.000001)/mean(WEIGHT+.000001))*100)
head(ann.svy.CV) 
#  YEAR  COEF_VAR
#1 1982  89.05901
#2 1983  78.52027
#3 1984  75.84277
#4 1985  83.08645
#5 1986 106.12680
#6 1987  67.52873

#PLOT survey CV by stratum area:
cv.temp<-expand.grid.df(data.frame(YEAR=1982:2018),data.frame(STRATUM=c(13,14,15,17)))
survey.cv.strat<-merge(svy.CV.strat,cv.temp,id.vars=c('YEAR','STRATUM'),all=TRUE)

ggplot(survey.cv.strat,aes(x=YEAR,y=COEF_VAR)) + geom_line() + 
  geom_point() + scale_x_continuous(name="Year",breaks=seq(1982, 2017, 5)) +
  scale_y_continuous(name="Coefficient of Variation", breaks=seq(0,250,50)) +
  theme_bw() + ggtitle("Survey CV by Stratum") + facet_wrap(~STRATUM) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#PLOT survey CV by year:
cv.temp.2<-expand.grid.df(data.frame(YEAR=1982:2018))
ann.survey.cv<-merge(ann.svy.CV,cv.temp.2,id.vars=c('YEAR'),all=TRUE)

jpeg(filename="C://Users//cassistadarosm//Documents//GitHub//bio.shrimp//Figures//1d.survey_cv.2018.jpg")
yg<-quantile(ann.svy.CV$COEF_VAR[ann.svy.CV$YEAR>1999&ann.svy.CV$YEAR<2011], probs=.33, na.rm=TRUE)
ry<-quantile(ann.svy.CV$COEF_VAR[ann.svy.CV$YEAR>1999&ann.svy.CV$YEAR<2011], probs=.66, na.rm=TRUE)
xmin=-Inf
xmax=Inf
ggplot(ann.survey.cv, aes(YEAR,COEF_VAR)) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = -Inf, ymax = yg), fill = "green", alpha = 0.015) +
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin =76.5 , ymax = ry), fill = "yellow", alpha = 0.015) + 
  geom_rect(aes(xmin = xmin, xmax = xmax,ymin = 99.37, ymax = Inf), fill = "red", alpha = 0.015) +
  geom_hline(yintercept = yg,colour="dark green") + geom_hline(yintercept = ry,colour="red") + 
  geom_point() + geom_line() + scale_x_continuous(name="Year",breaks=seq(min(ann.svy.CV$YEAR), max(ann.svy.CV$YEAR), 5)) +
  scale_y_continuous(name="Coefficient of Variation") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off() 
