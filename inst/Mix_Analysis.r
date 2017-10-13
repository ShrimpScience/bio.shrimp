#' @title Mix_Analysis
#' @description Perform mixture analysis for survey size frequency of main trawl catch
#' @return Analytical figure and estimate of abundance at age 2
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

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in Mix_code.r
### Start: October 11, 2017 
require(bio.shrimp)
####### FILE PREPERATION
#This analysis requires work in a MS Excel workbook first - 95-17_pop_clean.
#Follow instructions to produce a size freq distribition in the newpopcalc(2005+) spreadsheet.
#Copy the grey column - total population divided by 1 million into another spreadsheet with ages 
#Then, replace the last age bin with "Inf", close save as *.txt

#Prepare the initparm.  Test some 6 and 7 age-class initparm files, although the last two age 
#classes get pooled in the Traffic Light approach anyways (into 5+) so the difference seems to be nearlly
#null.

#Calculate number of shrimp per size:

#Get library for Mix analysis
library(mixdist)

#Load size frequency distribution:
mix_2015 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/Mix2015.txt", header = T)
mix_2016 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/Mix2016.txt", header = T)
mix_2017 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/Mix2017.txt", header = T)
mix_2015
mix_2016
mix_2017


###############################################################

#Trying versions of initparm based on those used in past years.
#The 7-year distributions do not seem to fit the 2010 distribution (2001 cohort died off?)
#Do not forget to adjust fixsigma values in mix()to match initparm file 
initparm_5 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/initparm5.txt", header = T)
initparm_6 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/initparm6.txt", header = T)
initparm_7 <- read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/initparm7.txt", header = T)
  
Mix_Fit.1 <- mix(mixdat = mix_2017, mixpar = initparm_5, dist = "lnorm", 
             constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", 
             fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9), cov = NULL, size = NULL), emsteps = 100)

# Defined 5 classes:
Mix_Fit.1
summary(Mix_Fit.1)
#Parameters:
#       pi    mu sigma
#1 0.05056 13.54   0.9
#2 0.18420 16.59   0.9
#3 0.37934 19.03   0.9
#4 0.19304 22.77   0.9
#5 0.19287 25.72   0.9
#
# Standard Errors:
#     pi.se   mu.se sigma.se
#1 0.004696 0.10019       NA
#2 0.011212 0.09749       NA
#3 0.012590 0.05382       NA
#4 0.007684 0.06538       NA
#5 0.007927 0.05068       NA
#
# Analysis of Variance Table
#
#           Df  Chisq   Pr(>Chisq)    
# Residuals 10 302.19  < 2.2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(Mix_Fit.1)
#############################################################
#Defined 6 classes:
Mix_Fit.2
summary(Mix_Fit.2)
#Parameters:
#       pi    mu sigma
#1 0.04373 13.38   0.9
#2 0.15431 16.28   0.9
#3 0.38151 18.76   0.9
#4 0.13132 21.65   0.9
#5 0.16960 24.10   0.9
#6 0.11953 26.26   0.9
#
#Standard Errors:
#     pi.se   mu.se sigma.se
#1 0.004662 0.11411       NA
#2 0.009877 0.10720       NA
#3 0.011189 0.06554       NA
#4 0.010781 0.21255       NA
#5 0.010186 0.23388       NA
#6 0.017776 0.14166       NA
#
# Analysis of Variance Table
#
#           Df  Chisq   Pr(>Chisq)    
# Residuals  9 162.66  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(Mix_Fit.2)
################################################
#Defined 7 classes
Mix_Fit.3
summary(Mix_Fit.3)
#Parameters:
#       pi    mu sigma
#1 0.04212 13.34   0.9
#2 0.14699 16.20   0.9
#3 0.37180 18.66   0.9
#4 0.11334 21.11   0.9
#5 0.15730 23.46   0.9
#6 0.14062 25.59   0.8
#7 0.02783 27.47   0.9
#
#Standard Errors:
#     pi.se   mu.se sigma.se
#1 0.004474 0.11302       NA
#2 0.009159 0.09962       NA
#3 0.012118 0.06448       NA
#4 0.009481 0.21991       NA
#5 0.010523 0.17027       NA
#6 0.010811 0.14278       NA
#7 0.008558 0.26357       NA
#
#
# Analysis of Variance Table
#
#           Df Chisq   Pr(>Chisq)    
# Residuals  7 116.2  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(Mix_Fit.3)
##########################################################
