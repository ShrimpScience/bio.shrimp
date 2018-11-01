#' @title Mix_Analysis
#' @description Perform mixture analysis for survey size frequency of main trawl catch
#' @return Analytical figure and estimates of abundance at age 2 and age 4
#'
#' @importFrom plyr ddply
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom ggplot2 ggplot
#' @importFrom reshape melt
#' @importFrom car Anova
#' @importFrom effects allEffects
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr select
#' 
#' @author Manon Cassista-Da Ros, \email{manon.cassista-daros@@dfo-mpo.gc.ca}
#' @export

### MCassistaDaRos running and modifying code provided by DHardie/JBroome in Mix_code.r
### Start: October 11, 2017 
require(bio.shrimp)

####### FILE PREPERATION
#This analysis requires work with scripts 2c and 2d and replaces MS Excel workbook - 95-17_pop_clean.
surv.pop<-read.csv("C:/Users/cassistadarosm/Documents/SHRIMP/Data/Offline Data Files/SurvMTPopCalc95-18.Data2018-10-16.csv",header=T)
#Some years ('95 and '96)  have lengths that are < 7 mm, these are removed:
surv.pop.lf<-subset(surv.pop,CL_MM>6)

#Divide by a Million:
surv.pop.lf$FREQ_MIL<-surv.pop.lf$ess.FREQ/1000000

#Set working Directory to send files to:
setwd("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Mix_Param")
#For the entire data set, find the annual max carapace length values and set them to Inf (i.e. 1/0)
cl_max<- aggregate(CL_MM ~ YEAR, data = surv.pop.lf, max)
y=unique(surv.pop$YEAR)
for(j in 1:length(y)){
for (i in 1:nrow(cl_max)){
surv.pop[which(surv.pop$YEAR==cl_max$YEAR[i] & surv.pop$CL_MM==cl_max$CL_MM[i]), "CL_MM"]<-1/0
}
#annual.dat<-cbind(YEAR=surv.pop$YEAR[surv.pop$YEAR==y[j]],CL_MM=surv.pop$CL_MM[surv.pop$YEAR==y[j]],FREQ=surv.pop$FREQ_MIL[surv.pop$YEAR==y[j]])
annual.dat<-cbind(length=surv.pop.lf$CL_MM[surv.pop.lf$YEAR==y[j]],frequency=surv.pop.lf$FREQ_MIL[surv.pop.lf$YEAR==y[j]])
write.table(annual.dat,paste(y[j], "Mix",".txt"), row.names = FALSE, sep = "\t", quote = FALSE)
}  

#Prepare the initparm.  Test some 6 and 7 age-class initparm files, although the last two age classes get pooled in the Traffic Light approach anyways (into 5+) so the difference seems to be nearly null.
library(data.table)

param_list <- list.files(path = "C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Mix_Param", recursive = TRUE,pattern = "initparm.txt", full.names = FALSE)

#Load size frequency distribution:
Mix_1995<-read.table("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Mix_Param/1995 Mix .txt",header=T)

list_of_files <- list.files(path = "C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Mix_Param", recursive = TRUE,pattern = "Mix.txt", full.names = FALSE)

# Read all the files and create a FileName column to store filenames
param.table <- rbindlist( sapply(param_list, fread, simplify = FALSE),
                 use.names = TRUE, idcol = "FileName" )
write.table(param.table,paste('Parameters.Mix', ".txt", sep = ""), col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)

# annf.table <- rbindlist( sapply(list_of_files, fread, simplify = FALSE),
#                  use.names = TRUE, idcol = "FileName" )


txt_files_df <- lapply(list_of_files, function(x) {read.table(file = x, header = F, sep ="\t")})
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame))
names(combined_df)<-c("YEAR","CL_MM","FREQ")
out.file<-combined_df
# out.file<-""
# file.names <- dir("C:/Users/cassistadarosm/Documents/GitHub/bio.shrimp/datadirectory/data/Inputs/Mix_Param", pattern ="Mix.txt")
# for(i in 1:length(file.names)){
#   file <- read.table(file.names[i],header=F, sep="\t", stringsAsFactors=FALSE)
#   out.file <- rbind(out.file, file)
# }


# write.table(annf.table,paste("Annual.Freq.Mix", ".txt", sep = ""), col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
###############################################################
#Trying versions of initparm based on those used in past years.
#The 7-year distributions do not seem to fit the 2010 distribution (2001 cohort died off?)
#Do not forget to adjust fixsigma values in mix()to match initparm file 

#Get library for Mix analysis
library(mixdist)
y=unique(out.file[,1])
z=unique(param.table$FileName)
browser()
for(i in 1:length(y)){
for(j in 1:length(z)){
#outfile <- paste(y[j],"-Mix.txt",sep="") 

mix(mixdat = out.file[!is.na(out.file$CL_MM) & out.file$YEAR ==y[i],c("CL_MM","FREQ")], mixpar = param.table[param.table$FileName==z[j],c("pi","mu","sigma")], dist = "lnorm", 
constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9), cov = NULL, size = NULL), emsteps = 100)
 # consigma = "SFX"
  #fixsigma = param.table$sigma[param.table$FileName==z[j]]
}
#outfile <- paste(y[j],"-Mix.txt",sep="")  
  
}
mix(mixdat = mix_1995, mixpar = initparm_6, dist = "lnorm", constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9,0.9), cov = NULL, size = NULL), emsteps = 100)
  
mix(mixdat = Mix_2008, mixpar = InitParm608, dist = "lnorm", constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", fixpi = NULL, fixmu = NULL, fixsigma = c(1,1.1,1.2,1.3,1.3,1.5), cov = NULL, size = NULL), emsteps = 100)

write.table(data,quote=FALSE,sep=", ",outfile)
}  

Mix_Fit.2005.5 <- mix(mixdat = DT$mix_2005, mixpar = initparm_5, dist = "lnorm", 
             constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", 
             fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9), cov = NULL, size = NULL), emsteps = 100)
Mix_Fit.2005.6 <- mix(mixdat = mix_2005, mixpar = initparm_6, dist = "lnorm", 
                      constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", 
                                         fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9,0.9), cov = NULL, size = NULL), emsteps = 100)

Mix_Fit.2005.7 <- mix(mixdat = mix_2005, mixpar = initparm_7, dist = "lnorm", 
                      constr = mixconstr(conpi = "NONE", conmu = "NONE", consigma = "SFX", 
                                         fixpi = NULL, fixmu = NULL, fixsigma = c(0.9,0.9,0.9,0.9,0.9,0.8,0.9), cov = NULL, size = NULL), emsteps = 100)


# Defined 5 classes:
Mix_Fit.2005.5
summary(Mix_Fit.2005.5)
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

plot(Mix_Fit.2005.5)
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
