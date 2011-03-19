RC.igraph.union <-
function(r1, r2, dovert=FALSE) {
  #join
  res.edges <- rbind(r1$edges,r2$edges)
  #count
  myvertices <- unique(c(as.character(res.edges$V1),as.character(res.edges$V2)))
  numvert <- length(myvertices)
  numedges <- length(res.edges$V1)
  res.vertices <- array(0,dim=c(numvert,2))
  if (dovert==TRUE) {
    for (i in 1:numvert) {
      res.vertices[i,1] = myvertices[i]
      for (j in 1:numedges) {
        if(res.vertices[i,1] == res.edges$V1[j]) res.vertices[i,2] = as.numeric(res.vertices[i,2]) + as.numeric(res.edges$V3[j])
        if(res.vertices[i,1] == res.edges$V2[j]) res.vertices[i,2] = as.numeric(res.vertices[i,2]) - as.numeric(res.edges$V3[j])
      }
    }
  } else {
    for (i in 1:numvert) {
      res.vertices[i,1] = myvertices[i]
      res.vertices[i,2] = 0
    }
  }
  list(edges=res.edges,vertices=res.vertices)
}

