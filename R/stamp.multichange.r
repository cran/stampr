# ---- roxygen documentation TEMPLATE----
#
#' @title run stamp function for multiple years of polygons at once
#'
#' @description
#'  The function \code{stamp.multichange} is a wrapper function that makes multiple calls to the stamp 
#'  function to ease spatial-temporal analysis of multiple years of polygon data
#'
#' @details
#'  \code{stamp.multichange} is a simple wrapper function for the \code{stamp} function. The two options for data structure
#'  are those in the \code{katrina} data, where each time period is a row, and rows are time-ordered, and the structure of 
#'  the \code{mpb} data, where time period is specified by a column. Time periods should be ordered from 1 through T.
#'
#' @param polys a \code{SpatialPolygonsDataFrame} wiht 2+ years of data to run through the \code{stamp} function.
#' @param changeByRow logical, whether or not each time period is a separate unique row of data (e.g., as per the \code{katrina} data)
#' @param changeByField logical, whether or not time period data is given by a specific field. If this is TRUE, changeByRow should be false
#' @param changeField string, name of the field which contains time period if changeByField is TRUE
#' @param ... list of paramater values to provide to the \code{stamp} function
#'
#' @return
#'  A \code{SpatialPolygonsDataFrame} which includes all outputs from the calls to the \code{stamp} function. If there are T time periods,
#'  there will be T-1 time periods in the resulting \code{SpatialPolygonsDataFrame} object.
#'
#' @keywords stamp
#' @examples
#' library(sp)
#' data("katrina")
#' katrina$ID <- katrina$Id
#' ch <- stamp.multichange(katrina, changeByRow = TRUE, dc = 0, distance = TRUE, direction = FALSE)
#' STGroup <- stamp.stgroup.summary(ch)
#' head(STGroup)
#' @export

#
# ---- End of roxygen documentation ----

stamp.multichange <- function(polys, changeByRow=TRUE, changeByField = FALSE, changeField="", ...) {
  outEvents <- list()
  lSum <- 1
  if(changeByRow==TRUE) {
    for(i in 1:(nrow(polys@data)-1)) {
      Ti <- polys[i,]
      Ti_1 <- polys[i+1,]
      ch <- stamp(Ti, Ti_1, ...)
      xx1 <- spChFIDs(ch, paste(i, "-", as.character((lSum):(lSum+length(ch)-1)), sep=""))
      lSum <- length(ch) + lSum
      outEvents[[i]] <- xx1 #@data
    } 
  }
  if(changeByField == TRUE) {
    #polys$ID <- 1:(length(polys@data[,changeField]))
    for(i in 1:(length(unique(polys@data[,changeField]))-1)) {
      Ti <- subset(polys, polys@data[,changeField] == unique(sort(polys@data[,changeField]))[i])
      Ti_1 <- subset(polys, polys@data[,changeField] == unique(sort(polys@data[,changeField]))[i+1])
      ch <- stamp(Ti, Ti_1, ...)
      xx1 <- spChFIDs(ch, paste(i, "-", as.character((lSum):(lSum+length(ch)-1)), sep=""))
      lSum <- length(ch) + lSum
      outEvents[[i]] <- xx1 #@data
    } 
  }
  
  outEvents <- do.call("rbind", outEvents)
  grps <- row.names(outEvents) #get row names
  outEvents$TGROUP <- substr(grps, 1, as.numeric(unlist(lapply(strsplit(grps, ''), function(x) which(x == '-'))))-1) #extract time period to distinguish change time periods
  outEvents$STGROUP <- as.numeric(paste(outEvents$TGROUP,outEvents$GROUP, sep="")) #generate 
  return(outEvents)
}