#' @export
#' @title convert.ddmmmm
#' @description This function can convert coordinates formatted as DDMMMM to 
#' decimal degrees.  This script will ignore NA values, and will retain the sign 
#' of the input coord
#' @param \code{df} = dataframe with coordinate fields
#' @param lat.field the default is \code{"LAT"}. the name of the 
#' field holding latitude values (in DDMMMM)
#' @param lon.field the default is \code{"LON"}.  the name of the 
#' field holding longitude values (in DDMMMM)
#' @author Mike McMahon 
#' @examples
#' lats = c(430925,-430925,432519,432519,430954,NA)
#' longs = c(341249,474550,-474550,NA,695860,-625950)
#' test=as.data.frame(cbind(lats,longs))
#' colnames(test)=c("LATITUDEDDMMMM","LONGITUDEDDMMMM")
#'  convert.ddmmmm(test, lat.field = "LATITUDEDDMMMM", lon.field = "LONGITUDEDDMMMM")
#' @family coordinate converters
#' @export

convert.ddmmmm <-function(df = NULL, lat.field="LAT", lon.field="LON"){
  df$LAT_DD<-NA
  df$LON_DD<-NA
  df[!is.na(lat.field),"LAT_DD"]<- ifelse(test = sign(df[!is.na(lat.field), lat.field])==1,
                                          no = -1* (as.numeric(substr(df[!is.na(lat.field), lat.field],2,3)) +
                                                      as.numeric(substr(df[!is.na(lat.field), lat.field], 4, 5)) / 60 +  
                                                      as.numeric(substr(df[!is.na(lat.field), lat.field], 6, 7)) / 3600),
                                          yes = as.numeric(substr(df[!is.na(lat.field), lat.field],1,2)) +
                                            as.numeric(substr(df[!is.na(lat.field), lat.field], 3, 4)) / 60 +  
                                            as.numeric(substr(df[!is.na(lat.field), lat.field], 5, 6)) / 3600)
  
  df[!is.na(lon.field),"LON_DD"]<- ifelse(test = sign(df[!is.na(lon.field), lon.field])==1,
                                          no = -1* (as.numeric(substr(df[!is.na(lon.field), lon.field],2,3)) +
                                                      as.numeric(substr(df[!is.na(lon.field), lon.field], 4, 5)) / 60 +  
                                                      as.numeric(substr(df[!is.na(lon.field), lon.field], 6, 7)) / 3600),
                                          yes = as.numeric(substr(df[!is.na(lon.field), lon.field],1,2)) +
                                            as.numeric(substr(df[!is.na(lon.field), lon.field], 3, 4)) / 60 +  
                                            as.numeric(substr(df[!is.na(lon.field), lon.field], 5, 6)) / 3600)
  
  
  
  return(df)
}