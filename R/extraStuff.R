# R helper bits
#tkplot(g, layout = layout.k_partite(g), vertex.color="grey")

#plot(g, layout = layout.k_partite(g), vertex.size=2, edge.curved=TRUE, vertex.size=5, edge.arrow.mode='-', edge.width=E(g)$weight, vertex.label.cex = 0.5)

#tkplot(g, layout = layout.k_partite(g), vertex.size=2, edge.curved=TRUE, vertex.size=5, edge.arrow.mode='-', edge.width=E(g)$weight, vertex.label.cex = 0.5)

# library(RJSONIO) 
# temp<-cbind(V(g)$name,V(g)$layer)
# colnames(temp)<-c("name","group")
# js1<-toJSON(temp)
# write.graph(g,"edgelist.csv",format="edgelist")
# edges<-read.csv("edgelist.csv",sep=" ",header=F)
# colnames(edges)<-c("source","target")
# edges<-as.matrix(edges)
# js2<-toJSON(edges)
# asn<-paste('{"nodes":',js1,',"links":',js2,'}',sep="")
# write(asn,file="asn.json")
# 
# 
# 
# fired <- readOGR("C:/Users/crobertson/Dropbox/R/stamp/temp", "okanogan_complex_sp")
# fired$ID <- 0:(length(fired)-1)
# chng <- multiChange(fired, changeByRow = FALSE, changeByField=TRUE, changeField = "TIMEPERIOD", stampArgs = list(500, TRUE, TRUE))
# outSTGroup <- stamp.stgroup.summary(chng)


# v_layers_df <- unique( rbind(
#   expand.grid( ID = df$from[df$tg==1], Layer = 1),
#   expand.grid( ID = df$to[df$tg==1], Layer = 2),
#   expand.grid( ID = df$from[df$tg==2], Layer = 2),
#   expand.grid( ID = df$to[df$tg==2], Layer = 3),
#   expand.grid( ID = df$from[df$tg==3], Layer = 3),
#   expand.grid( ID = df$to[df$tg==3], Layer = 4),
#   expand.grid( ID = df$from[df$tg==4], Layer = 4),
#   expand.grid( ID = df$to[df$tg==4], Layer = 5),
#   expand.grid( ID = df$from[df$tg==5], Layer = 5),
#   expand.grid( ID = df$to[df$tg==5], Layer = 6),
#   expand.grid( ID = df$from[df$tg==6], Layer = 6),
#   expand.grid( ID = df$to[df$tg==6], Layer = 7),
#   expand.grid( ID = df$from[df$tg==7], Layer = 7),
#   expand.grid( ID = df$to[df$tg==7], Layer = 8),
#   expand.grid( ID = df$from[df$tg==8], Layer = 8),
#   expand.grid( ID = df$to[df$tg==8], Layer = 9),
#   expand.grid( ID = df$from[df$tg==9], Layer = 9),
#   expand.grid( ID = df$to[df$tg==9], Layer = 10),
#   expand.grid( ID = df$from[df$tg==10], Layer = 10),
#   expand.grid( ID = df$to[df$tg==10], Layer = 11),
#   expand.grid( ID = df$from[df$tg==11], Layer = 11),
#   expand.grid( ID = df$to[df$tg==11], Layer = 12),
#   expand.grid( ID = df$from[df$tg==12], Layer = 12),
#   expand.grid( ID = df$to[df$tg==12], Layer = 13),
#   expand.grid( ID = df$from[df$tg==13], Layer = 13),
#   expand.grid( ID = df$to[df$tg==13], Layer = 14),
#   expand.grid( ID = df$from[df$tg==14], Layer = 14),
#   expand.grid( ID = df$to[df$tg==14], Layer = 15),
#   expand.grid( ID = df$from[df$tg==15], Layer = 15),
#   expand.grid( ID = df$to[df$tg==15], Layer = 16),
#   expand.grid( ID = df$from[df$tg==16], Layer = 16),
#   expand.grid( ID = df$to[df$tg==16], Layer = 17),
#   expand.grid( ID = df$from[df$tg==17], Layer = 17),
#   expand.grid( ID = df$to[df$tg==17], Layer = 18),
#   expand.grid( ID = df$from[df$tg==18], Layer = 18),
#   expand.grid( ID = df$to[df$tg==18], Layer = 19),
#   expand.grid( ID = df$from[df$tg==19], Layer = 19),
#   expand.grid( ID = df$to[df$tg==19], Layer = 20),
#   expand.grid( ID = df$from[df$tg==20], Layer = 20),
#   expand.grid( ID = df$to[df$tg==20], Layer = 21),
#   expand.grid( ID = df$from[df$tg==21], Layer = 21),
#   expand.grid( ID = df$to[df$tg==21], Layer = 22),
#   expand.grid( ID = df$from[df$tg==22], Layer = 22),
#   expand.grid( ID = df$to[df$tg==22], Layer = 23),
#   expand.grid( ID = df$from[df$tg==23], Layer = 23),
#   expand.grid( ID = df$to[df$tg==23], Layer = 24),
#   expand.grid( ID = df$from[df$tg==24], Layer = 24),
#   expand.grid( ID = df$to[df$tg==24], Layer = 25),
#   expand.grid( ID = df$from[df$tg==25], Layer = 25),
#   expand.grid( ID = df$to[df$tg==25], Layer = 26),
#   expand.grid( ID = df$from[df$tg==26], Layer = 26),
#   expand.grid( ID = df$to[df$tg==26], Layer = 27),
#   expand.grid( ID = df$from[df$tg==27], Layer = 27),
#   expand.grid( ID = df$to[df$tg==27], Layer = 28),
#   expand.grid( ID = df$from[df$tg==28], Layer = 28),
#   expand.grid( ID = df$to[df$tg==28], Layer = 29)
#   ))


#Figure x1
#mpb <- readOGR('../../../../../stampr/data/', 'mpbShape')
#proj4string(mpb) <- "+proj=utm +zone=9 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#chng <- stamp.multichange(mpb, changeByRow = FALSE, changeByField=TRUE, changeField = "TGROUP", stampArgs = list(2000, TRUE, "ConeModel", 8, TRUE))
#mpb$TGROUP <- as.numeric(mpb$TGROUP)

#png('MPB_Poly146.png',height=5,width=5,units='in',res=300)
#  plot(mpb[which(mpb$ID==146),], col=NA, border='green')
#  plot(mpb[which(mpb$TGROUP == 3),], col=NA, border='red', add=TRUE)
#  dev.off()

#outSTGroup <- stamp.stgroup.summary(chng)

#Figure x2
#plot(outSTGroup$aEXPN/outSTGroup$AREA, outSTGroup$aCONT/outSTGroup$AREA, xlab="% Expansion", ylab="% Contraction", pch=20, ylim=c(0,1), xlim=c(0,1), cex=2)
#Figure x3

