#' shrimp.db
#' 
#' This function is the main workhorse to pull data from databases and some initial filtering of data used in shrimp stock assessments. Results are saved and can be reloaded using this function.
#' @param DS is the main switch that selects which data source to load or operate. 
#' Options for DS include: 'complete','annual.landings','logs','logs41','logs41jonah','observer41','atSea','cris','port','vlog','fsrs','scallop','survey','annual.landings'.  Any of these arguements called as listed return the data object. To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above. 
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

    fn.root =  file.path( datadirectory('bio.shrimp'), "data") 
    fnODBC  =  file.path(fn.root, "ODBCDump")
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
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.survey<-sqlQuery(con,"select * from SHRIMP.SHRSURVEY")
        shrimp.survey$YEAR<-year(shrimp.survey$FDATE)
        save(shrimp.survey, file=file.path( fnODBC, "shrimp.survey.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.survey.rdata"), .GlobalEnv)
    
  }

    ### Shrimp Gulf Commercial Logs 
    if (DS %in% c("ComLogs.redo", "ComLogs") ) {
      
      if (DS=="ComLogs.redo") {
        # survey
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        shrimp.COMLOG<-sqlQuery(con,"select * from SHRIMP.SHRCOMLOG")
        shrimp.COMLOG$YEAR<-year(shrimp.COMLOG$FDATE)
        save(shrimp.COMLOG, file=file.path( fnODBC, "shrimp.comlog.rdata"), compress=T)
        
        gc()
      }
      load(file.path( fnODBC, "shrimp.survey.rdata"), .GlobalEnv)
      
    }
    
  }    
  

