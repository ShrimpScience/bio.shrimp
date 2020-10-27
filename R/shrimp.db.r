#' @title shrimp.db
#' @description This function is the main workhorse to pull data from databases and some initial filtering of data used in shrimp stock assessments. Results are saved and can be reloaded using this function.
#' @param DS default is \code{'complete.redo'} This is the main switch that selects which data source to load or operate.
#' Options for DS include: 'complete','survey','comlogs','details','observer','millim','totals','totalsfemtran' and 'juveniles'.
#' Any of these arguments called as listed return the data object - 'complete' loads ALL data sources.
#' To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above.
#' @param this.oracle.server This is the server
#' @param this.oracle.username This is the username
#' @param this.oracle.password This is the password
#' @param datadirectory This is where you want to store your data (or where your data is already stored)
#' @param showprogress default is FALSE
#' @importFrom lubridate year
#' @importFrom utils write.csv
#' @importFrom lubridate month
#' @return Data objects that contain the data for use in further analyses.
# @examples shrimp.db('survey.redo') # makes the data objects for the survey data.
# shrimp.db('survey') #loads the object survey
#' @export
#'
utils::globalVariables(c("oracle.server", "oracle.username", "oracle.password"))
shrimp.db = function( DS="complete.redo",
                      this.oracle.server=oracle.server,
                      this.oracle.username=oracle.username,
                      this.oracle.password=oracle.password,
                      datadirectory = datadirectory,
                      showprogress = F) {

  DS = tolower(DS)   #make DS parameter case-insensitive
  ts <- Sys.Date()   #create time stamp

  #create the folder to store extractions products (rdata and csvs)
  if (is.null(datadirectory)){
    cat("Requires a value for datadirectory.  Aborting\n")
    return()
  }

  if (!dir.exists(datadirectory)){
    #if the specified datadirectory doesn't exist, it could be an error or intentional -
    #ask the user if they want to create it
    #if they do, it is implied we are now doing an extraction - not a load
    #ensure that the values for DS have ".redo" on the end to force the extraction
    create_dir = toupper(readline(prompt = "The specified data directory does not exist.\nType 'y' to create this folder and extract the data into it (i.e. do a *.redo).  Press any other key to cancel the operation. \n"))
    if (create_dir !="Y")return()
    dir.create(datadirectory, recursive = TRUE, showWarnings = FALSE )
    if (showprogress) cat(paste("<new folder> datadirectory: ",datadirectory))
    if (!all(grepl(x = DS,pattern = ".redo"))){
      goodDS = DS[grepl('.redo$', DS)]
      badDS = DS[!grepl('.redo$', DS)]
      badDS=paste(badDS,".redo",sep="")
      DS = c(goodDS,badDS)
    }
  }else{
    if (showprogress) cat(paste("datadirectory:",datadirectory,"\n"))
  }

  rdataPath = file.path(datadirectory, 'ODBCDump')
  csvPath = file.path(rdataPath,'csv')

  if (!dir.exists(rdataPath)){
    #check if necessary folders exist, create them if necessary
    dir.create(rdataPath, recursive = TRUE, showWarnings = FALSE )
    if (showprogress) cat(paste("<new folder> .rdata files:",rdataPath,"\n"))
  }else{
    if (showprogress) cat(paste(".rdata files:",rdataPath,"\n"))
  }

  if (!dir.exists(csvPath)){
    dir.create(csvPath, recursive = TRUE, showWarnings = FALSE )
    if (showprogress) cat(paste("<new folder> .csv files:",csvPath,"\n"))
  }else{
    if (showprogress) cat(paste(".csv files:",csvPath,"\n"))
  }

  ############################# HELPER FUNCTIONS ##########################
  convert.dd.dddd<-function(x){
    #stolen on 20190226 from
    #https://github.com/PopulationEcologyDivision/bio.utilities/blob/master/R/convert.dd.dddd.r
    #simplified since all shrimp coords used the default dec.deg format
      dat<-data.frame(ddmm.mm=x,dd.dddd=NA)
      #degrees-minutes-seconds -> degrees
      ddmmss<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]
      ddmm.ss<-ddmmss/100
      ddmm<-trunc(ddmm.ss)
      ss<-(ddmm.ss-ddmm)*100
      dd.mm<-ddmm/100
      dd<-trunc(dd.mm)
      mm<-(dd.mm-dd)*100
      dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]<-dd+mm/60+ss/3600
      #degrees-minutes -> degrees
      dd.mmmm<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]/100
      dd<-trunc(dd.mmmm)
      mm.mm<-(dd.mmmm-dd)*100
      dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]<-dd+mm.mm/60
      #degrees -> degrees
      dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]
      return(dat$dd.dddd)
  }

  ############################# SHRIMP DATA HANDLING FUNCTIONS  #############
  # The processes below are now discrete functions. Each takes a 'redo'
  # parameter.  If redo=T, than the data is re-extracted from Oracle prior to
  # loading.  If F, than the data is simply loaded from the
  do.survey<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP SURVEY DATA ##########################
    r_nm = file.path(rdataPath, "shrimp.survey.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("Survey.Data.",ts)),".csv")

      shrimp.survey<-ROracle::dbGetQuery(con,"select * from SHRIMP.SHRSURVEY")
      shrimp.survey$CV_LAT<-convert.dd.dddd(shrimp.survey$BLAT/100)
      shrimp.survey$CV_LONG<-convert.dd.dddd(shrimp.survey$BLONG/100)*-1
      shrimp.survey$YEAR<-lubridate::year(shrimp.survey$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                     sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                     sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(shrimp.survey, file=r_nm, compress=T)
      utils::write.csv(shrimp.survey, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.comlogs<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP COMMERCIAL LOG DATA ##########################
    r_nm = file.path(rdataPath, "shrimp.comlog.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("Comlog.Data.",ts)),".csv")

      shrimp.COMLOG<-ROracle::dbGetQuery(con,"select * from SHRIMP.SHRCOMLOG")
      shrimp.COMLOG$CV_LAT<-convert.dd.dddd(shrimp.COMLOG$BLAT/100)
      shrimp.COMLOG$CV_LONG<-convert.dd.dddd(shrimp.COMLOG$BLONG/100)*-1
      shrimp.COMLOG$YEAR<-lubridate::year(shrimp.COMLOG$FDATE)
      shrimp.COMLOG$MONTH<-lubridate::month(shrimp.COMLOG$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                   sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                   sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(shrimp.COMLOG, file=r_nm, compress=T)
      utils::write.csv(shrimp.COMLOG, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.details<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP DETAILS FROM SAMPLING DATA #####################
    r_nm = file.path(rdataPath, "shrimp.detail.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("Details.Data.",ts)),".csv")

      shrimp.DETAILS<-ROracle::dbGetQuery(con,"select * from SHRIMP.SHRDETAIL")
      shrimp.DETAILS$CV_LAT<-convert.dd.dddd(shrimp.DETAILS$LAT/100)
      shrimp.DETAILS$CV_LONG<-convert.dd.dddd(shrimp.DETAILS$XLONG/100)*-1
      shrimp.DETAILS$YEAR<-lubridate::year(shrimp.DETAILS$FDATE)
      shrimp.DETAILS$MONTH<-lubridate::month(shrimp.DETAILS$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                   sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                   sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(shrimp.DETAILS, file=r_nm, compress=T)
      utils::write.csv(shrimp.DETAILS, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.observer<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP OBSERVER DATA ##########################
    r_nm = file.path(rdataPath, "shrimp.observer.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("Observer.Data.",ts)),".csv")

      shrimp.observer<-ROracle::dbGetQuery(con,"select to_number(to_char(setdate,'YYYY')) year, to_char(s.fishset_id) fishset_id, trip,v.vessel_name, s.set_no, (to_char(setdate,'DD-MM-YYYY')) setdate, settime,longitude lon, latitude lat, sc.speccd_id, common species, est_num_caught, est_kept_wt,est_discard_wt, est_reduction_wt,est_combined_wt, s.source, p.pntcd_id gear_cd
                                  from  observer.istrips t,
                                  observer.isfishsets s,
                                  observer.issetprofile p,
                                  observer.isgears g,
                                  observer.isvessels v,
                                  observer.iscatches c,
                                  observer.isspeciescodes sc
                                  where   s.fishset_id=p.fishset_id
                                  and     s.fishset_id=c.fishset_id
                                  and     c.speccd_id=sc.speccd_id
                                  and     g.trip_id=t.trip_id
                                  and     p.pntcd_id=
                                  DECODE(g.GearCd_Id,1,2,2,2,3,2,4,2,6,2,7,2,8,2,9,2,10,2,11,2,12,2,13,2,14,2,15,2,16,2,17,2,19,2,20,2,21,2,22,2,23,2,24,2,30,2,31,2,39,1,40,1,41,1,42,1,49,1,50,1,51,1,52,1,53,1,54,1,55,2,58,1,60,1,61,1,62,1,63,1,71,2,72,2,81,1,0)
                                  and s.gear_id=g.gear_id
                                  and v.vess_id=t.vess_id
                                  and tripcd_id = 2210
                                  and ctrycd_id = 2
                                  and tonccd_id <4
                                  order by 1,2")
      save(shrimp.observer, file=r_nm, compress=T)
      utils::write.csv(shrimp.observer, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.millim<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP DETAIL IN MILLIM VIEW ##########################
    r_nm = file.path(rdataPath, "MILLIM.VIEW.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("MILLIM.VIEW.",ts)),".csv")

      MILLIM.VIEW<-ROracle::dbGetQuery(con,"select * from SHRIMP.MILLIM")
      MILLIM.VIEW$YEAR<-lubridate::year(MILLIM.VIEW$FDATE)
      MILLIM.VIEW$MONTH<-lubridate::month(MILLIM.VIEW$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                   sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                   sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(MILLIM.VIEW, file=r_nm, compress=T)
      utils::write.csv(MILLIM.VIEW, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.totals<- function(con=NULL, redo = F, this_showprogress=showprogress){
    ######################### SHRIMP SURVEY SPAWNING BIOMASS IN TOTALS VIEW ##########################
    r_nm = file.path(rdataPath, "TOTALS.VIEW.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("TOTALS.VIEW.",ts)),".csv")

      TOTALS.VIEW<-ROracle::dbGetQuery(con,"select * from SHRIMP.TOTALS")
      TOTALS.VIEW$YEAR<-lubridate::year(TOTALS.VIEW$FDATE)
      TOTALS.VIEW$MONTH<-lubridate::month(TOTALS.VIEW$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                   sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                   sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(TOTALS.VIEW, file=r_nm, compress=T)
      utils::write.csv(TOTALS.VIEW, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.totalsfemtran<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ####################### SHRIMP SURVEY SPAWNING BIOMASS IN TOTALSFEMTRAN VIEW ######################
    r_nm = file.path(rdataPath, "TOTALSFEMTRAN.VIEW.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("TOTALSFEMTRAN.VIEW.",ts)),".csv")

      TOTALSFEMTRAN.VIEW<-ROracle::dbGetQuery(con,"select * from SHRIMP.TOTALSFEMTRAN")
      TOTALSFEMTRAN.VIEW$YEAR<-lubridate::year(TOTALSFEMTRAN.VIEW$FDATE)
      TOTALSFEMTRAN.VIEW$MONTH<-lubridate::month(TOTALSFEMTRAN.VIEW$FDATE)
      shrimp.survey$DATE <- paste0(lubridate::year(shrimp.survey$FDATE),"-",
                                   sprintf("%02d",lubridate::month(shrimp.survey$FDATE)),"-",
                                   sprintf("%02d",lubridate::day(shrimp.survey$FDATE)))
      save(TOTALSFEMTRAN.VIEW, file=r_nm, compress=T)
      utils::write.csv(TOTALSFEMTRAN.VIEW, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
      load(r_nm, .GlobalEnv)
      if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  do.juveniles<-function(con=NULL, redo = F, this_showprogress=showprogress){
    ############################# SHRIMP SURVEY JUVENILES ##########################
    r_nm = file.path(rdataPath, "shrimp.Juvenile.rdata")
    if (redo){
      c_nm = paste0(file.path(csvPath,paste0("shrimp.Juv.data.",ts)),".csv")
      shrimp.Juv<-ROracle::dbGetQuery(con,"select * from SHRIMP.SHRJUV")

      # shrimp.Juv$YEAR<-lubridate::year(shrimp.Juv$FDATE)
      # shrimp.Juv$MONTH<-lubridate::month(shrimp.Juv$FDATE)
      cat("Manon's code tried to create YEAR and MONTH from FDATE, but that field doesn't exist\n")
      save(shrimp.Juv, file=r_nm, compress=T)
      utils::write.csv(shrimp.Juv, c_nm,row.names = F)
      if (this_showprogress)cat(paste("Saved:\n\t",r_nm,"\n\t",c_nm,"\n"))
    }
    load(r_nm, .GlobalEnv)
    if (this_showprogress)cat(paste("Loaded:",r_nm,"\n"))
  }
  # make the oracle connection
  thiscon <- ROracle::dbConnect(DBI::dbDriver("Oracle"), this.oracle.username, this.oracle.password, this.oracle.server)
  if (is.null(thiscon)){
    cat("No valid connection, aborting\n")
    return()
  }

  if (any(DS %in% c("complete","complete.redo"))) {
    complete.flag = ifelse(any(DS %in% c("complete.redo")),T,F)
    do.survey(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.comlogs(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.details(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.observer(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.millim(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.totals(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.totalsfemtran(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
    do.juveniles(con=thiscon,redo=complete.flag, this_showprogress=showprogress)
  }else{
    if (grepl(DS, pattern = "survey")){
      survey.flag = ifelse(DS %in% c("survey.redo"),T,F)
      do.survey(con=thiscon,redo = survey.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "comlogs")){
      comlogs.flag = ifelse(DS %in% c("comlogs.redo"),T,F)
      do.comlogs(con=thiscon,redo = comlogs.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "details")){
      details.flag = ifelse(DS %in% c("details.redo"),T,F)
      do.details(con=thiscon,redo=details.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "observer")){
      observer.flag = ifelse(DS %in% c("observer.redo"),T,F)
      do.observer(con=thiscon,redo=observer.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "millim")){
      millim.flag = ifelse(DS %in% c("millim.redo"),T,F)
      do.millim(con=thiscon,redo= millim.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "totalsfemtran")){
      totalsfemtran.flag = ifelse(DS %in% c("totalsfemtran.redo"),T,F)
      do.totalsfemtran(con=thiscon,redo=totalsfemtran.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "totals") & nchar(DS) < 12){
      # to prevent this check from accidentally grabbing
      # 'totalsfemtran', it is more specific than the others (i.e. nchar(DS) <12)
      # this will catch 'totals' or 'totals.redo'
      totals.flag = ifelse(DS %in% c("totals.redo"),T,F)
      do.totals(con=thiscon,redo=totals.flag, this_showprogress=showprogress)
    }
    if (grepl(DS, pattern = "juveniles")){
      juveniles.flag = ifelse(DS %in% c("juveniles.redo"),T,F)
      do.juveniles(con=thiscon,redo=juveniles.flag, this_showprogress=showprogress)
    }
  }
  gc()
  #RODBC::odbcClose(thiscon)
}
