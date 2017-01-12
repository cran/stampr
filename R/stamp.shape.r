# ---- roxygen documentation ----
#
#' @title Compute shape indices on \code{stamp} output  
#'
#' @description
#'  This function computes a suite of shape complexity metrics on STAMP polygons
#'  facilitating shape analysis.
#'
#' @details
#'  The \code{stamp.shape} function can be used to perform polygon shape analysis on output
#'  polygons from function \code{stamp}. Shape indices are computed on each output polygon.
#'  Five shape indices are available:
#'  \cr
#'  \code{"PER"} -- Shape perimeter, in appropriate units.
#'    \cr
#'  \code{"PAR"} -- Perimeter-area ratio, in appropriate units; \deqn{\mathtt{PAR} = \frac{p}{a}}{PAR=p/a}
#'    \cr
#'  \code{"FRAC"} -- Fractal dimension (Mandelbrot 1977, Lovejoy 1982); \deqn{\mathtt{FRAC} =\frac{2\log(p)}{\log(a)}}{FRAC=2log(p)/log(a)}
#'    \cr
#'  \code{"SHPI"} -- Shape index (Patton 1975); \deqn{\mathtt{SHPI} = \frac{p}{2*\sqrt{\pi*a}}}{SHPI=(p)/2*sqrt(pi*a)}
#'    \cr
#'  \code{"LIN"} -- Linearity index  (Baker and Cai 1992); \deqn{\mathtt{LIN} = 1 - \frac{a}{a_{circ}}}{LIN=1-(a/a_circ)}
#'    \cr
#'  Where \eqn{a} is polygon area, \eqn{p} is polygon perimeter, and \eqn{a_{circ}}{a_circ} is the
#'  area of the circumscribing (encompassing) circle of a polygon.
#'  \cr \cr
#'  Some Notes:
#'  \cr
#'  PER is simply the length of the perimeter, and is not an overly useful measure of shape, but may be useful
#'  in direct comparisons. PAR > 0, without limit with larger values sugesting more complex, irregular shapes.
#'  The range of FRAC is [1, 2]. FRAC approaches 1 for very simple shapes (squares, circles, etc.) and approaches
#'  2 for complex, convoluted shapes. SHPI > 1 without limit, as SHPI increase, the complexity of the shape 
#'  increases. The range of LIN is [0, 1]. A perfect circle will have a LIN of 0, while more linear shapes will approach 1.
#'  \cr
#'  \cr
#'  The indices PAR, FRAC, SHPI, and LIN are all essentially measures of shape complexity. LIN is unique in that it 
#'  tries to focus on the  linearity of the shape by comparing the area to a circle. LIN is however, less useful with
#'  STAMP events containing multiple polygons, as the calculation for the circumscribing circle will include all
#'  polygon objects within the group and artificially increase the LIN scores.
#' 
#' @param T1 a \code{SpatialPolygons} object of polygons from time 1.
#' @param T2 a \code{SpatialPolygons} object of polygons from time 2.
#' @param stmp output \code{SpatialPolygonsDataFrame} generated from the \code{stamp} function.
#' @param index a character item identifying which shape metric is to be computed. See \bold{Details}.
#'
#' @return
#'  A \code{DataFrame} with four columns:
#'  \cr
#'  \code{GROUP} -- STAMP polygon groups from the \code{stamp} function.
#'  \code{T1.INDEX} -- shape index value for T1 polygons for each group. \code{INDEX} is replaced by name of index.
#'  \code{T2.INDEX} -- shape index value for T2 polygons for each group. \code{INDEX} is replaced by name of index.
#'  \code{d.INDEX} -- change (t2 - t1) in shape value for each group. \code{INDEX} is replaced by name of index.
#'  
#' @references
#'  Baker, W.L. and Cai, Y. (1992) The r.le programs for multiscale analysis of landscape structure using
#'    the GRASS geographical information system. \emph{Landscape Ecology}, 7(4):291-302. \cr\cr
#'  Lovejoy, S. (1982) Area-perimeter relation for rain and cloud areas. \emph{Science}, 216(4542):185-187.  \cr\cr
#'  Mandlebrot, B.B. (1977) Fractals, Form, Chance and Dimension. W.H Freeman and Co., New York.  \cr\cr
#'  Patton, D.R. (1977) A diversity index for quantifying habitat "edge". \emph{Wildlife Society Bulletin}, 3:171-173.
#'
#' @keywords stamp
#' @seealso stamp 
#' @examples
#' library(sp)
#' library(rgeos)
#' library(raster)
#' data("fire1")
#' data("fire2")
#' #set globally unique ID column required for stamp function
#' fire1$ID <- 1:nrow(fire1) 
#' #set globally unique ID column required for stamp function
#' fire2$ID <- (max(fire1$ID)+1):(max(fire1$ID) + nrow(fire2)) 
#' ch <- stamp(fire1, fire2, dc=1, direction=FALSE, distance=FALSE)
#' ch.sh <- stamp.shape(T1 = fire1, T2 = fire2, stmp = ch, index = 'LIN')
#' @export
#
# ---- End of roxygen documentation ----


# Polygon Shape Metrics
# - Perimeter            (PER)
# - Perimeter-Area Ratio (PAR)
# - Fractal Dimension    (FRAC)
# - SHAPE index          (SHPI)
# - Linearity/RCC        (LIN)
#-------------------------------------------------------------------------------
stamp.shape <- function(T1,T2,stmp,index='PAR'){
  grps <- unique(stmp$GROUP)
  
  #Function for computing the linearity index
  lin.fun <- function(pol){
    ch <- gConvexHull(pol)
    tol <- gLength(ch)*0.001
    ch <- gSimplify(ch,tol=tol)
    coords <- ch@polygons[[1]]@Polygons[[1]]@coords
    coords <- coords[which(!duplicated(coords)),]
    f <- function(p) { max(pointDistance(rbind(p), coords, lonlat=FALSE)) }
    p <- optim(colMeans(coords), f)
    cc <- buffer(SpatialPoints(rbind(p$par)), width=p$value, quadsegs=45)
    #circ <- circumcircle(coords[,1],coords[,2],num.touch=2)   #SLOW
    #Acirc <- (circ$radius^2)*pi
    #circum function from https://stat.ethz.ch/pipermail/r-sig-geo/2016-August/024773.html
    1 - (gArea(pol)/gArea(cc))
  }
  
  #switch function for computing shape metrics
  shape.fun <- function(pol,index){
    switch(index,
           PER = gLength(pol),
           PAR = gLength(pol)/gArea(pol),
           FRAC = (2*log(gLength(pol)))/gArea(pol),
           SHPI = gLength(pol)/(2*sqrt(pi*gArea(pol))),
           LIN = lin.fun(pol),
           stop('input index parameter is invalid, check spelling.')
          )
  }
  
  t1. <- rep(NA,length(grps))
  t2. <- t1.
  for (i in grps){
    stmp. <- stmp[which(stmp$GROUP==i),]
    t1.ind <- unique(stmp.$ID1)[which(!is.na(unique(stmp.$ID1)))]
    t1.ind <- which(T1$ID %in% t1.ind)
    
    t2.ind <- unique(stmp.$ID2)[which(!is.na(unique(stmp.$ID2)))]
    t2.ind <- which(T2$ID %in% t2.ind)
    
    if (length(t1.ind) > 0){ t1.[i] <- shape.fun(T1[t1.ind,],index) }
    if (length(t2.ind) > 0){ t2.[i] <- shape.fun(T2[t2.ind,],index) }
    
    }
  
  outdf <- data.frame(GROUP=grps, a=t1., b=t2.)
  outdf[,4] <- outdf[,3] - outdf[,2]

  names(outdf) <- c('GROUP', paste('T1.',index,sep=''), paste('T2.',index,sep=''), paste('d.',index,sep=''))
  
  return(outdf)
  
  }