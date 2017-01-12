# ---- roxygen documentation ----
#
#' @title Mapping (plotting) functionality for \code{stamp} output
#'
#' @description
#'  This function maps STAMP output for visual assessment of STAMP events and groupings.
#'  Choice of which aspect of the stamp output to be visualized is controlled by passing
#'  the column name to the \code{stamp.map} function.
#'
#' @details
#'  The \code{stamp.map} function can be used to visualize any of the stamp event designation levels
#'  (e.g., \code{"LEV1"}, \code{"LEV2"}, \code{"LEV3"}, \code{"LEV4"}, or the STAMP groupings
#'  (based off of parameter \code{dc} in the \code{stamp} function).
#'
#' @param stmp output from the \code{stamp} function, i.e., a (\code{SpatialPolygonsDataFrame}).
#' @param by tells the function which attribute to visualize, one of \code{"LEV1"},
#'           \code{"LEV2"}, \code{"LEV3"}, \code{"LEV4"}, or \code{"GROUP"}
#' @param ... additional parameters to be passed to the plot function
#'
#' @return
#'   \code{stamp.map} returns a map of the \code{stamp} output using the \code{spplot} functionality. 
#'   It implements a pre-defined coloring scheme.
#'
#' @keywords stamp plot
#' @seealso stamp
#' data("fire1")
#' data("fire2")
#' fire1$ID <- 1:nrow(fire1) 
#' fire2$ID <- (max(fire1$ID)+1):(max(fire1$ID) + nrow(fire2))
#' ch <- stamp(fire1, fire2, dc=1, direction=FALSE, distance=FALSE)
#' stamp.map(ch, "LEV1") 
#' stamp.map(ch, "LEV2") 
#' stamp.map(ch, "LEV3") 
#' stamp.map(ch, "LEV4")
#' 
#' @export
#
# ---- End of roxygen documentation ----

stamp.map <- function(stmp,by="LEV1", ...){
  if (by %in% colnames(stmp@data)){
    if (by == "LEV1"){
      stmp@data[,by] <- factor(stmp@data[,by],levels=c("DISA","STBL","GENR"))
      cols <- c("red","gray25","blue")
      }
    else if (by == "LEV2"){
      stmp@data[,by] <- factor(stmp@data[,by],levels=c("DISA","CONT","STBL","EXPN","GENR"))
      cols <- c("red","darkred","gray25","royalblue4","blue")
      }
    else if (by == "LEV3"){
      stmp@data[,by] <- factor(stmp@data[,by],levels=c("DISA","DISP1","CONV","CONC","CONT","STBL","EXPN","FRAG","DIVR","DISP2","GENR"))
      cols <- c("red","indianred1","darkorange","violetred","darkred","gray25","royalblue4","deepskyblue4","dodgerblue","skyblue","blue")
      }
    else if (by == "LEV4"){
      stmp@data[,by] <- factor(stmp@data[,by],levels=c('N/A',"UNION","DIVISION","BOTH"))
      cols <- c("grey","red","blue","purple")
      }
    else {
      stmp@data[,by] <- factor(stmp@data[,by])
      cols <- rainbow(length(unique(stmp@data[,by])))
      }
    spplot(stmp,zcol=by,col.regions=cols, ...)
    }
  else {stop(paste("The column ",by," does not exist.",sep=""))}
  }
#------ end of stamp.map function ---------------------------------------------