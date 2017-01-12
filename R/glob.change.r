# ---- roxygen documentation ----
#
#' @title glob.change
#'
#' @description
#'  The function \code{glob.change} computes a set of three global change metrics for comparison
#'  between two polygon sets. These metrics are outlined in Robertson et al. (2007; Table 4).
#'
#' @details
#'  \code{glob.change} computes three change metrics, detailed below, that can be used to quantify changes
#'  between two polygon sets:
#'  \cr\cr
#'  \code{NumRatio} -- ratio between the number of polygons in \code{T2} and \code{T1};
#'  \deqn{\mathtt{NumRatio} = \frac{\#(T1)}{\#(T2)}}{NumRatio=#T1/#T2}
#'  \cr
#'  \code{AreaRatio} -- ratio between the areas of polygons in T2 and T1;
#'  \deqn{\mathtt{AreaRatio} = \frac{A(T2)}{A(T1)}}{AreaRatio=A(T1)/A(T2)}
#'  \cr
#'  \code{AvgAreaRatio} -- ratio between the \code{AreaRatio} and \code{NumRatio};
#'  \deqn{\mathtt{AvgAreaRatio} = \frac{\mathtt{AreaRatio}}{\mathtt{NumRatio}} = \frac{\frac{A(T2)}{A(T1)}}{\frac{\#(T1)}{\#(T2)}}}{AvgAreaRatio=AreaRatio/NumRatio}
#'
#' @param T1 a \code{SpatialPolygonsDataFrame} object of polygons from time 1.
#' @param T2 a \code{SpatialPolygonsDataFrame} object of polygons from time 2.

#'
#' @return
#'  A \code{list} object with three elements - Results for the \code{NumRatio}, \code{AreaRatio}, and \code{AvgAreaRatio} metrics.
#'
#' @keywords metrics
#' @export
# ---- End of roxygen documentation ----

glob.change <- function(T1,T2){
  NumRatio <- length(T1)/length(T2)
  AreaRatio <- gArea(T1)/gArea(T2)
  AvgAreaRatio <- AreaRatio/NumRatio
  return(list(NumRatio=NumRatio, AreaRatio=AreaRatio, AvgAreaRatio = AvgAreaRatio))
  }