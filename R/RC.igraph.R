RC.igraph <-
function(network=NULL) {
  if (is.null(network)) stop("A network must be specified. Use RC.network.list or RC.network to create one.")
  g <- graph.data.frame(network$edges,directed=T,vertices=network$vertices)
  for (i in 3:length(network$vertices[1,])) {
    g <- set.vertex.attribute(g, colnames(network$vertices)[i], index=V(g), as.numeric(network$vertices[,i]))
  }
  return(g) 
}

