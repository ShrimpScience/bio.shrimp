#' shrimp.db
#' 
#' This function is the main workhorse to pull data from databases and some initial filtering of data used in shrimp stock assessments. Results are saved and can be reloaded using this function.
#' @param DS is the main switch that selects which data source to load or operate. 
#' Options for DS include: 'complete','survey','ComLogs'. Any of these arguements called as listed return the data object. To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above. 
#' @param oracle.server This is the server
#' @param oracle.username This is the username
#' @param oracle.password This is the password
#' @param datadirectory This is where your data will be saved
#' @importFrom lubridate year
#' @importFrom RODBC sqlQuery
#' @importFrom RODBC odbcConnect
#' @return Data objects that contain the data for use in further analyses.
# @examples shrimp.db('survey.redo') # makes the data objects for the survey data.
# shrimp.db('survey') #loads the object survey
#' @export

  shrimp.db = function( DS="complete.redo", oracle.server="PTRAN", oracle.username=NULL, oracle.password=NULL, 
                        datadirectory=file.path(getwd(), "data")) {
    options(stringsAsFactors=F)

    fn.root =  file.path('datadirectory', 'data') 
    fnODBC  =  file.path(fn.root, 'ODBCDump')
    fnProducts = file.path(fn.root,'Products')
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnODBC, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnProducts, recursive = TRUE, showWarnings = FALSE )
    
    if (DS %in% c("complete.redo") ) {

        # ODBC data dump of shrimp data
        shrimp.db( DS="survey.redo")
        shrimp.db( DS="ComLogs")
        }
      
    ### Shrimp survey  
    if (DS %in% c("survey.redo", "survey") ) {
      
      if (DS=="survey.redo") {
        # survey
        con = odbcConnect(oracle.server, uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.survey<-sqlQuery(con,"select * from SHRIMP.SHRSURVEY")
        shrimp.survey$CV_LAT<-convert.dd.dddd(shrimp.survey$BLAT)
        shrimp.survey$CV_LONG<-convert.dd.dddd(shrimp.survey$BLONG)*-1
        shrimp.survey$YEAR<-year(shrimp.survey$FDATE)
        save(shrimp.survey, file=file.path( fnODBC, "shrimp.survey.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.survey.rdata"), .GlobalEnv)
    
  }

    ### Shrimp Commercial Logs 
    if (DS %in% c("ComLogs.redo", "ComLogs") ) {
      
      if (DS=="ComLogs.redo") {
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.COMLOG<-sqlQuery(con,"select * from SHRIMP.SHRCOMLOG")
        shrimp.COMLOG$CV_LAT<-convert.dd.dddd(shrimp.COMLOG$BLAT)
        shrimp.COMLOG$CV_LONG<-convert.dd.dddd(shrimp.COMLOG$BLONG)*-1
        shrimp.COMLOG$YEAR<-year(shrimp.COMLOG$FDATE)
        shrimp.COMLOG$MONTH<-month(shrimp.COMLOG$FDATE)
        save(shrimp.COMLOG, file=file.path( fnODBC, "shrimp.comlog.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.comlog.rdata"), .GlobalEnv)
      
    }
    ### Shrimp Details from any sampling sources available 
    if (DS %in% c("Details.redo", "Details") ) {
      
      if (DS=="Details.redo") {
        #browser()
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.DETAILS<-sqlQuery(con,"select * from SHRIMP.SHRDETAIL")
        shrimp.DETAILS$YEAR<-year(shrimp.DETAILS$FDATE)
        shrimp.DETAILS$MONTH<-month(shrimp.DETAILS$FDATE)
        save(shrimp.DETAILS, file=file.path( fnODBC, "shrimp.detail.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.detail.rdata"), .GlobalEnv)
      
    }
    ### Shrimp Detail in MILLIM VIEW 
    if (DS %in% c("MILLIM.redo", "MILLIM") ) {
      
      if (DS=="MILLIM.redo") {
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        MILLIM.VIEW<-sqlQuery(con,"select * from CASSISTAM.MILLIM")
        MILLIM.VIEW$YEAR<-year(MILLIM.VIEW$FDATE)
        MILLIM.VIEW$MONTH<-month(MILLIM.VIEW$FDATE)
        save(MILLIM.VIEW, file=file.path( fnODBC, "MILLIM.VIEW.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "MILLIM.VIEW.rdata"), .GlobalEnv)
      
    }
    ### Shrimp Survey Spawning Biomass in TOTALS VIEW 
    if (DS %in% c("TOTALS.redo", "TOTALS") ) {
      
      if (DS=="TOTALS.redo") {
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        TOTALS.VIEW<-sqlQuery(con,"select * from CASSISTAM.TOTALS")
        TOTALS.VIEW$YEAR<-year(TOTALS.VIEW$FDATE)
        TOTALS.VIEW$MONTH<-month(TOTALS.VIEW$FDATE)
        save(TOTALS.VIEW, file=file.path( fnODBC, "TOTALS.VIEW.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "TOTALS.VIEW.rdata"), .GlobalEnv)
      
    }
    ### Shrimp Survey Spawning Biomass in TOTALSFEMTRAN VIEW 
    if (DS %in% c("TOTALSFEMTRAN.redo", "TOTALSFEMTRAN") ) {
      
      if (DS=="TOTALSFEMTRAN.redo") {
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        TOTALSFEMTRAN.VIEW<-sqlQuery(con,"select * from CASSISTAM.TOTALSFEMTRAN")
        TOTALSFEMTRAN.VIEW$YEAR<-year(TOTALSFEMTRAN.VIEW$FDATE)
        TOTALSFEMTRAN.VIEW$MONTH<-month(TOTALSFEMTRAN.VIEW$FDATE)
        save(TOTALSFEMTRAN.VIEW, file=file.path( fnODBC, "TOTALSFEMTRAN.VIEW.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "TOTALSFEMTRAN.VIEW.rdata"), .GlobalEnv)
      
    }
    ### Shrimp Survey Juveniles 
    if (DS %in% c("Juveniles.redo", "Juveniles") ) {
      
      if (DS=="Juveniles.redo") {
        #browser()
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.Juv<-sqlQuery(con,"select * from SHRIMP.SHRJUV")
        shrimp.Juv$YEAR<-year(shrimp.Juv$FDATE)
        shrimp.Juv$MONTH<-month(shrimp.Juv$FDATE)
        save(shrimp.Juv, file=file.path( fnODBC, "shrimp.Juvenile.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.Juvenile.rdata"), .GlobalEnv)
      
    }
    
  }    
  

