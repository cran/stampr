# ---- roxygen documentation TEMPLATE----
#
#' @title Perform polygon directional analysis
#'
#' @description
#'  \code{stamp.direction} facilitates polygon directional analysis using a variety of methods.
#'
#' @details
#'  The \code{stamp.direction} function can be used to facilitate directional analysis on output
#'  \code{stamp.obj} objects from function \code{stamp}. Currently, four directional analysis methods
#'  are available:  
#'  \itemize{
#'  \item \code{"CentroidAngle"} -- The centroid angle is simply the angle between the centroids of two polygons.
#'    The centroid angle method is computed on STAMP objects by first grouping all T1 polygons (by STAMP group)
#'    and computing their centroid. Then, the angle from each T1 group centroid, to the centroid of each STAMP event
#'    within the group is calculated. Centroid angles are recorded in degrees, with North having a value of 0, East
#'    90, and so on. \code{"CentroidAngle"} ignores the \code{ndir} parameter.
#'  \item \code{"ConeModel"} -- The cone model method calculates areas of STAMP event polygons within cones radiating from
#'    the centroid of the origin polygon.  The cone model method first computes the centroid of all T1 polygons in a STAMP grouping. It then
#'    computes \code{ndir} equally spaced cones radiating outward from the T1 centroid. The first cone is always
#'    centered on North, but there can be any number of cones. The area of each STAMP event, in each cone (specifying direction),
#'    is then calculated. See Peuquet and Zhang (1987) for more detailed information
#'  \item \code{"MBRModel"} --   The minimum bounding rectangle (MBR) method first computes the MBR for all T1 events
#'    in a STAMP grouping. Then the lines of four edges of the MBR are extended outwards to infinity creating
#'    sections for the eight cardinal directions around the MBR, along with the MBR itself. The area
#'    of each stamp event within each of the nine sections is then computed. See Skiadopoulos et al. (2005) for
#'    more detailed information. \code{"MBRModel"} ignores the \code{ndir} parameter.
#'  \item \code{"ModConeModel"} -- The modified cone model first computes the centroid of the T1 event that includes a stable event type.
#'    Then \code{ndir = 4 or 8} cones are created outward from this centroid to the minimum bounding rectangle
#'    of the entire grouping. As described by Robertson et al. (2007) this approach is more accomodating
#'    to polygon groups that are irregular in size or shape. If there is more than 1 stable event (as flagged
#'    by the \code{stamp.obj LEV4} column, the Voronoi segregation method defined by Robertson et al. (2007)
#'    is employed. The modified cone model method first computes the centroid of all T1 polygons in a STAMP grouping.
#'    It then computes the bounding box of ALL events in a STAMP grouping. Then, \code{ndir=4} or \code{8}
#'    cones are computed. In the case of \code{ndir=4}, cones radiate from the T1 centroid to the four
#'    corners of the bounding box. The result of the modified cone model method is that the cones
#'    are not equally spaced, but tailored to the individual STAMP groupings shape. See Robertson et al.
#'    (2007) for more detailed information.
#'  }
#'
#' @param stmp a \code{SpatialPolygonsDataFrame} object generated from the \code{stamp} function.
#' @param dir.mode a character item identifying which directional relations method is to be used. See \bold{Details}
#'  for information on each individual method.
#' @param group (optional) a logical value identifying whether direction should be computed on groups or individual
#'  event polygons (only used with \code{CentroidAngle} method).  
#' @param ndir (optional) parameter identifying the number of directions to be computed. See inidividual method
#'             \bold{Details} for appropriate usage.
#' 
#' @return
#'  Appends the input \code{stamp} object with appropriate columns for the directional analysis chosen, if
#'  \code{dir.mode} is:
#'  \item{"CentroidAngle"}{A single column with centroid angle results, in degrees (North = 0 degrees). If
#'    \code{group=TRUE} then values are identical for all event polygons in the group.}
#'  \item{"ConeModel"}{\code{ndir} new columns with the area of the STAMP event in each direction,
#'    named appropriately (e.g., as \code{DIR45}, where 45 refers to the mid-point of that directional cone).}
#'  \item{"MBRModel"}{9 new columns with the area of the STAMP event in each direction,
#'    named appropriately as "SW","S","SE","W","SAME","E","NW","N","NE".}
#'  \item{"ModConeModel"}{\code{ndir} new columns  with the area of the STAMP event in each direction,
#'    named appropriately as, for example, "N","E","S","W" with \code{ndir=4}.}
#'  Note: STAMP events that are singular (i.e., only 1 polygon in the group)
#'  will have \code{NA}'s from directional analysis.
#'
#' @references
#' Robertson, C., Nelson, T., Boots, B., and Wulder, M. (2007) STAMP: Spatial-temporal analysis of moving polygons.
#'  \emph{Journal of Geographical Systems}, 9:207-227. \cr\cr
#'Peuquet, D., Zhang, C.X. (1987) An algorithm to determine the directional relationship between arbitrarily-shaped
#'  polygons in the plane. \emph{Pattern Recognition}, 20:65-74. \cr\cr
#'Skiadopoulos, S. Giannoukos, C., Sarkas, N., Vassiliadis, P., Sellis, T., and Koubarakis, M. (2005) Computing and
#'  managing directional relations. \emph{IEEE Transactions on Knowledge and Data Engineering}, 17:1610-1623.
#'
#' @keywords stamp
#' @seealso stamp, stamp.distance, stamp.shape
#' @export
#
# ---- End of roxygen documentation ----


stamp.direction <- function(stmp,dir.mode="CentroidAngle",ndir=4,group=FALSE){  
#===============================================================================
# Directional Analysis functions
#  - Centroid Angle
#  - Cone Model
#  - Minimum Bounding Rectangle (MBR) method
#  - Modified Cone model
#===============================================================================

#----- Centroid Angle Function -------------------------------------------------
CentroidAngle <- function(stmp,group=FALSE){
  stmp$CENDIR <- NA
  grps <- unique(stmp$GROUP)
  for (i in grps){
    ind <- which(stmp$GROUP == i)
    if (length(ind) > 1){   #no movement for individual events
      #Assumes that all T1 events in a group form the basis of the centroid.
      #  i.e., does not separate stable, from concentration, or displacement.
      #  Also, does not account for problems arising from multiple stable events.
      t1.base <- stmp[which(stmp$GROUP == i & is.na(stmp$ID1) == FALSE),]
      c1 <- coordinates(gCentroid(t1.base))   #gCentroid is an "area-weighted" centroid function
      if (group==FALSE){
        for (j in ind){
          #Compute Centroid Angle
          c2 <- coordinates(gCentroid(stmp[j,]))
          dy <- c2[2] - c1[2]
          dx <- c2[1] - c1[1]
          temp <- atan2(dx,dy)*180/pi
          if (temp < 0){temp <- 360 + temp}
          stmp$CENDIR[j] <- temp        
          }
        } else {
          t2.base <- stmp[which(stmp$GROUP == i & is.na(stmp$ID2) == FALSE),]
          #Compute Centroid Angle
          c2 <- coordinates(gCentroid(t2.base))
          dy <- c2[2] - c1[2]
          dx <- c2[1] - c1[1]
          temp <- atan2(dx,dy)*180/pi
          if (temp < 0){temp <- 360 + temp}
          stmp$CENDIR[ind] <- temp  
        }
      }
    }
  return(stmp)
  }
#----------- end of CentroidAngle Function------------------------------------

#---- ConeModel function -----------------------------------------------------
ConeModel <- function(stmp,ndir=4){
  #params for cone analysis
  cwidth <- 2*pi/ndir
  origins <- seq(0,2*pi,by=cwidth)
  dexpand <- sqrt( (bbox(stmp)[1,2]-bbox(stmp)[1,1])^2 + (bbox(stmp)[2,2]-bbox(stmp)[2,1])^2 )
  #Create NA columns
  cols <- paste("DIR",round(origins*180/pi),sep="")[1:ndir]
  stmp@data[,cols] <- NA
  #go through groups
  grps <- unique(stmp$GROUP)
  for (i in grps){
    ind <- which(stmp$GROUP == i)
    if (length(ind) > 1){   #no movement for individual events
      #Assumes that all T1 events in a group form the basis of the centroid.
      #  i.e., does not separate stable, from concentration, or displacement.
      #  Also, does not account for problems arising from multiple stable events.
      t1.base <- stmp[which(stmp$GROUP == i & is.na(stmp$ID1) == FALSE),]
      c1 <- coordinates(gCentroid(t1.base))   #gCentroid is an "area-weighted" centroid function
      #create ndir cones.
      for (j in 1:ndir){
        #create column for each directional cone
        #cone angles
        theta1 <- origins[j] + 0.5*cwidth
        theta2 <- origins[j] - 0.5*cwidth
        #cone outer coords
        c2 <- c1 + c(sin(theta1),cos(theta1))*dexpand
        c3 <- c1 + c(sin(theta2),cos(theta2))*dexpand
        #cone polygon
        cone <- SpatialPolygons(list(Polygons(list(Polygon(rbind(c1,c3,c2,c1))),1)),proj4string=stmp@proj4string)
        #loop through polys in each group and compute area in each cone
        for (k in 1:length(ind)){
          into <- gIntersection(cone,stmp[ind[k],])
          if (is.null(into) == FALSE) {
            stmp@data[ind[k],cols[j]] <- gArea(into)
            }
          else {
            stmp@data[ind[k],cols[j]] <- 0
            }
          }
        }
      } #end if group > 1
    } #end group loop
  return(stmp)
  } #end function
#------------ End of ConeModel function --------------------------------------

#------- MBRModel function ---------------------------------------------------
MBRModel <- function(stmp){
  dexpand <- sqrt( (bbox(stmp)[1,2]-bbox(stmp)[1,1])^2 + (bbox(stmp)[2,2]-bbox(stmp)[2,1])^2 )
  c1 <- c(1,2,3,5,6,7,9,10,11)
  #add directional Cols
  cols <- c("SW","S","SE","W","SAME","E","NW","N","NE")
  stmp@data[,cols] <- NA
  #go through groups
  grps <- unique(stmp$GROUP)
  for (i in grps){
    ind <- which(stmp$GROUP == i)
    if (length(ind) > 1){   #no movement for individual events
      t1.base <- stmp[which(stmp$GROUP == i & is.na(stmp$ID1) == FALSE),]
      t1.bbox <- bbox(t1.base)
      x.tmp <- c(t1.bbox[1,1]-dexpand,t1.bbox[1,1],t1.bbox[1,2],t1.bbox[1,2]+dexpand)
      y.tmp <- c(t1.bbox[2,1]-dexpand,t1.bbox[2,1],t1.bbox[2,2],t1.bbox[2,2]+dexpand)
      crds <- expand.grid(list(x=x.tmp,y=y.tmp))
      #loop through MBR boxes
      for (j in 1:9){
        #get MBR box
        ind.crds <- c(c1[j],c1[j]+4,c1[j]+5,c1[j]+1,c1[j])
        box.poly <- SpatialPolygons(list(Polygons(list(Polygon(crds[ind.crds,])),1)),proj4string=stmp@proj4string)
        #loop through polys in each group and compute area in each box
        for (k in 1:length(ind)){
          into <- gIntersection(box.poly,stmp[ind[k],])
          if (is.null(into) == FALSE) {
            #stmp@data[ind[k],(ncols + j)] <- gArea(into)
            stmp@data[ind[k],cols[j]] <- gArea(into)
            }
          else {
            #stmp@data[ind[k],(ncols + j)] <- 0
            stmp@data[ind[k],cols[j]] <- 0
            }
          }
        }
      }
    }
  return(stmp)
  }
#--------- end of MBRModel function ------------------------------------------

#--------- ModConeModel function ---------------------------------------------
ModConeModel <- function(stmp,ndir=4){

#Some pre-defined functions, potentially move externally to ModConeModel
#------------------------------
allcoords = function(x) {
  vert = NULL
  polys = slot(x,'polygons')
  for(i in 1:length(polys)) {
    pp = slot(polys[[i]],'Polygons')
    for (j in 1:length(pp)) {vert = rbind(vert, coordinates(pp[[j]]))}
    }
  return(data.frame(x=vert[,1],y=vert[,2]))
  }

voronoipolygons <- function(x,poly) {
  if (.hasSlot(x, 'coords')) {
    crds <- slot(x,'coords')
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw,supressMsge=TRUE)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],y=crds[,2], row.names=sapply(slot(SP, 'polygons'),function(x) slot(x, 'ID'))))
  return(voronoi)
  }

ModVoronoi <- function(multi.pol,bbox.sp){
  verts <- unique(allcoords(multi.pol))
  vor <- voronoipolygons(verts,gBuffer(bbox.sp,width=1))
  data <- vor@data
  slot(vor,'proj4string') <- slot(bbox.sp,'proj4string')
  vor <- gIntersection(vor,bbox.sp,byid=T,id=as.character(1:length(vor)))
  vor <- SpatialPolygonsDataFrame(vor,data=data)
  vor$own <- NA
  for (p in 1:length(multi.pol)){vor$own[gIntersects(vor,multi.pol[p,],byid=T)] <- multi.pol@polygons[[p]]@ID}
  vor <- unionSpatialPolygons(vor,vor$own)
  return(vor)
  }
 #--------------------
 
 
  #---------- ndir = 4 or 8 set-up ---------------------------------        4 works, 8 has some problems can't figure out...
  if (ndir == 4) {
    cols <- c("N","E","S","W")
    slot(stmp,'data')[,cols] <- 0
    c2.ind <- c(3,4,2,1)
    c4.ind <- c(4,2,1,3)
    }
  if (ndir == 8) {
    cols <- c("N","NE","E","SE","S","SW","W","NW")
    slot(stmp,'data')[,cols] <- 0
    c2.ind <- c(22,24,20,10,4,2,6,16)
    c3.ind <- c(23,25,15,5,3,1,11,21)
    c4.ind <- c(24,20,10,4,2,6,16,22)
    }
  #-----------------------------------------------------------------------------
  #go through groups
  grps <- unique(stmp$GROUP)
  for (i in grps){
    ind <- which(stmp$GROUP == i)                    #poly ids in group i in stamp
    stb <- which(stmp$LEV3[ind] == "STBL")            #stable ids in group i in stamp

    #only perform movement analysis on groups with > 1 events.
    if (length(ind) > 1){
      bbox.sp <- gEnvelope(stmp[ind,])
      if (ndir == 4){
        #get bounding box cordinates
        x.tmp <- bbox(stmp[ind,])[1,]
        y.tmp <- bbox(stmp[ind,])[2,]
        crds <- expand.grid(x=x.tmp,y=y.tmp)
        }
      if (ndir == 8){
        #get bounding box coordinates at 25, 50 and 75 percent along box edge.
        dx <- bbox(stmp[ind,])[1,2] - bbox(stmp[ind,])[1,1]
        x.tmp <- c(bbox(stmp[ind,])[1,1],bbox(stmp[ind,])[1,1] + 0.25*dx,bbox(stmp[ind,])[1,1] + 0.5*dx, bbox(stmp[ind,])[1,1] + 0.75*dx, bbox(stmp[ind,])[1,2])
        dy <- bbox(stmp[ind,])[2,2] - bbox(stmp[ind,])[2,1]
        y.tmp <- c(bbox(stmp[ind,])[2,1],bbox(stmp[ind,])[2,1] + 0.25*dy,bbox(stmp[ind,])[2,1] + 0.5*dy, bbox(stmp[ind,])[2,1] + 0.75*dy, bbox(stmp[ind,])[2,2])
        crds <- expand.grid(x=x.tmp,y=y.tmp)
        }
    #If, else if, else statements for number of stable events.
    if (length(stb) < 1){  #no stable event groups
      #Assumes that all T1 events in a group form the basis of the centroid.
      t1.base <- stmp[which(stmp$GROUP == i & is.na(stmp$ID1) == FALSE),]
      c1 <- coordinates(gCentroid(t1.base))   #gCentroid is an "area-weighted" centroid function
      #create ndir cones.
      for (j in 1:ndir){
        #cone outer coords
        c2 <- crds[c2.ind[j],]
        c4 <- crds[c4.ind[j],]
        if (ndir==8){c3 <- crds[c3.ind[j],]}
        else{c3 <- (c2+c4)/2}
        #cone polygon
        cone <- SpatialPolygons(list(Polygons(list(Polygon(rbind(c1,c2,c3,c4,c1))),1)),proj4string=stmp@proj4string)
        #loop through polys in each group and compute area in each cone
        for (k in 1:length(ind)){
          into <- gIntersection(cone,stmp[ind[k],])
          if (is.null(into) == FALSE) {
            #stmp@data[ind[k],(ncols + j)] <- gArea(into)
            stmp@data[ind[k],cols[j]] <- gArea(into)
            }
          } #end k loop
        } #end j loop
      #-----------------------

      } else if (length(stb) == 1){ #single stable event groups
        #get stable events
        ID1.stb <- unique(stmp$ID1[ind[stb]]) #stable ids from T1
        T1.stb <- stmp[which(stmp$ID1 %in% ID1.stb),]   #T1 polys that form stable events)
        c1 <- coordinates(gCentroid(T1.stb))
        #create ndir cones.
        for (j in 1:ndir){
          #cone outer coords
          c2 <- crds[c2.ind[j],]
          c4 <- crds[c4.ind[j],]
          if (ndir==8){c3 <- crds[c3.ind[j],]}
          else{c3 <- (c2+c4)/2}
          #cone polygon
          cone <- SpatialPolygons(list(Polygons(list(Polygon(rbind(c1,c2,c3,c4,c1))),1)),proj4string=stmp@proj4string)
          #loop through polys in each group and compute area in each cone
          for (k in 1:length(ind)){
            #if (gIsValid(cone) == FALSE){           #could do some checking
            #cone <- gBuffer(cone,width=0)
            #print(c(i,j,k,ind))
            #}
            into <- gIntersection(cone,stmp[ind[k],])
            if (!is.null(into)) {
              #stmp@data[ind[k],(ncols + j)] <- gArea(into)
              stmp@data[ind[k],cols[j]] <- gArea(into)
              }
            } #end k loop
          } #end j loop
        #----------------------------

      } else {  #multi-stable polygon event groups
        print('c3c')
        #get stable events
        ID1.stb <- unique(stmp$ID1[ind[stb]])                   #stable ids from T1
        T1.stb <- stmp[which(stmp$ID1 %in% ID1.stb),]   #T1 polys that form stable events

        #Get multi-polygons voronoi dependent on "UNION","DIVISION","BOTH"
        if (stmp$LEV4[ind[1]] == "UNION"){
          vor.p <- ModVoronoi(T1.stb,bbox.sp)
          }
        else if (stmp$LEV4[ind[1]] == "DIVISION"){
          ID2.stb <- unique(stmp$ID2[ind[stb]])                   #stable ids from T2
          T2.stb <- stmp[which(row.names(stmp) %in% ID2.stb),]   #T2 polys that form stable events
          vor.p <- ModVoronoi(T2.stb,bbox.sp)
          }
        else{  #(stmp$LEV4[ind[1]] == "BOTH")
          vor.p1 <-ModVoronoi(T1.stb,bbox.sp)
          ID2.stb <- unique(stmp$ID2[ind[stb]])                   #stable ids from T2
          T2.stb <- stmp[which(row.names(stmp) %in% ID2.stb),]   #T2 polys that form stable events
          vor.p2 <- ModVoronoi(T2.stb,bbox.sp)
          vor.p <- gIntersection(vor.p1,vor.p2,byid=T)    #combine two voronoi diagrams
          }

        #perform ModCone directional analysis
        for (vor in 1:length(vor.p)){
          print('c4')
          c1 <- coordinates(gCentroid(gIntersection(T1.stb,vor.p[vor,])))
          for (j in 1:ndir){
            #cone outer coords
            c2 <- crds[c2.ind[j],]
            c4 <- crds[c4.ind[j],]
            if (ndir==8){c3 <- crds[c3.ind[j],]}
            else{c3 <- (c2+c4)/2}
            #cone polygon
            cone <- SpatialPolygons(list(Polygons(list(Polygon(rbind(c1,c2,c3,c4,c1))),1)),proj4string=stmp@proj4string)
            modCone <- gIntersection(cone,vor.p[vor,])
            #loop through polys in each group and compute area in each cone
            for (k in 1:length(ind)){
              into <- gIntersection(modCone,stmp[ind[k],])
              if (is.null(into) == FALSE) {
                stmp@data[ind[k],cols[j]] <- stmp@data[ind[k],cols[j]] + gArea(into)
                }
              } #end k loop
            }  #end j loop
          } #end vor loop
        }  #end multi-stable group analysis
      #------------------------------------------------


      } else {stmp@data[ind,cols] <- NA} #end single event check
    } #end i = grps loop
  return(stmp)
  } #end function
#--------- end of ModConeModel function --------------------------------------

##----- VoronoiModel function --------------------------------------------------
#VoronoiModel <- function(stmp){
#  #Function for extraction polygon vertex coordinates
#  allcoords = function(x) {
#    vert = NULL
#    polys = x@polygons
#    for(i in 1:length(polys)) {
#    #sapply(x@polygons, function(y) sapply(y@Polygons, function(z) rbind(vert,coordinates(z))))       #Can't get this to work yet...
#      pp = polys[[i]]@Polygons
#      for (j in 1:length(pp)) {vert = rbind(vert, coordinates(pp[[j]]))}
#      }
#    return(data.frame(x=vert[,1],y=vert[,2]))
#    }
#  #Get three dataframes with polygon vertices
#  T1.vert <- allcoords(T1)
#  T2.vert <- allcoords(T2)
#  Int.vert <- data.frame(coordinates(gIntersection(gBoundary(T1,byid=T),gBoundary(T2,byid=T))))
#  verts <- rbind(T1.vert,T2.vert,Int.vert)
#  
#  Del <- tri.mesh(verts,duplicate="remove")
#  #append vertex source to triangulation object
#  tab <- data.frame(node=1:Del$n,x=Del$x,y=Del$y,orig=0)
#  tab$orig[which(tab$x %in% T1.vert$x & tab$y %in% T1.vert$y)] <- "p"
#  tab$orig[which(tab$x %in% T2.vert$x & tab$y %in% T2.vert$y)] <- "q"
#  tab$orig[which(tab$x %in% Int.vert$x & tab$y %in% Int.vert$y)] <- "k"
#  tris <- data.frame(triangles(Del),orig1=0,orig2=0,orig3=0)
#  tris$orig1 <- tab$orig[match(tris$node1,tab$node,nomatch=NA)]
#  tris$orig2 <- tab$orig[match(tris$node2,tab$node,nomatch=NA)]
#  tris$orig3 <- tab$orig[match(tris$node3,tab$node,nomatch=NA)]
#  tris$keep <- FALSE
#  for (i in 1:dim(tris)[1]){
#    if (length(unique(unlist(tris[i,10:12]))) > 1) {tris$keep[i] <- TRUE}
#    }
#  tris <- tris[which(tris$keep==TRUE),]
#
#  #### NOT FINISHED YET ####
#  }
  
#----- end of VoronoiModel ----------------------------------------------------

#============= End of Directional Analysis Functions============================
  
  #Compute function results based on input method.
  stmp <- switch(dir.mode,
      CentroidAngle = CentroidAngle(stmp,group),
      ConeModel = ConeModel(stmp,ndir),
      MBRModel = MBRModel(stmp),
      ModConeModel = ModConeModel(stmp,ndir),
      stop(paste("The direction method is does not exist: ",dir.mode)))
  return(stmp)    
  }
