# ---- roxygen documentation TEMPLATE----
#
#' @title Compile stamp summary statistics by space-time group
#'
#' @description
#'  The function \code{stamp.stgroup.summary} compiles summary statistics for each STAMP grouping.
#'  Specifically, it computes the area of each STAMP event type (e.g., generation, expansion, etc.)
#'  within each grouping. It also computes the number of events belonging to each event type.
#'
#' @details
#'  \code{stamp.group.summary} computes area and count summary statistics of STAMP output. Note that if
#'  both \code{area} and \code{count} are set to \code{FALSE}, \code{stamp.group.summary} returns a
#'  \code{data.frame} with just the stgroup IDs as the only column.
#'
#' @param stmp a \code{SpatialPolygonsDataFrame} generated from the \code{stamp} function.
#' @param area logical, whether or not to compute the STAMP event areas.
#' @param count logical, whether or not to compute the count of STAMP evets within each group.
#'
#' @return
#'  A \code{data.frame} where rows are stamp groups and columns correspond to the STAMP event types (ID, areas, and counts).
#'
#' @keywords stamp
#' @examples
#' library(sp)
#' data("katrina")
#' katrina$ID <- katrina$Id
#' ch <- stamp.multichange(katrina, changeByRow = TRUE, dc = 0, distance = TRUE, direction = FALSE)
#' STGroup <- stamp.stgroup.summary(ch)
#' head(STGroup)
#' 
#' @export

#
# ---- End of roxygen documentation ----

stamp.stgroup.summary <- function(stmp,area=TRUE,count=TRUE){
  grps <- unique(stmp$STGROUP)
  outdf <- data.frame(STGROUP=grps,nEVENTS=0,AREA=0)
  evnts <- c("CONV","CONC","CONT","DISP1","DISA","STBL","EXPN","FRAG","DIVR","DISP2","GENA")
  for (i in 1:length(grps)){
      ind1 <- which(stmp$STGROUP == grps[i])
      outdf$nEVENTS[i] <- length(ind1)
      outdf$AREA[i] <- sum(stmp$AREA[ind1])
      }
  if (count==TRUE){
    nevnts <- paste("n",evnts,sep="")
    outdf[,nevnts] <- 0
    for (i in 1:length(grps)){
      ind1 <- which(stmp$STGROUP == grps[i])
      for (j in 1:length(evnts)){
        ind <- which(stmp$STGROUP == grps[i] & stmp$LEV3 == evnts[j])
        if (length(ind) > 0){ outdf[i,nevnts[j]] <- length(ind) }
        }
      }
    }
  if (area==TRUE){
    aevnts <- paste("a",evnts,sep="")
    outdf[,aevnts] <- 0
    for (i in 1:length(grps)){
      ind1 <- which(stmp$STGROUP == grps[i])
      for (j in 1:length(evnts)){
        ind <- which(stmp$STGROUP == grps[i] & stmp$LEV3 == evnts[j])
        if (length(ind) > 0){ outdf[i,aevnts[j]] <- sum(stmp$AREA[ind]) }
        }
      }
    }
  #return a dataframe with group summaries
  return(outdf)
  }