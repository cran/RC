RC.plot.network <-
function(network="", colors="", weights="") {
  require(igraph)
  if (length(network)==1) stop("No network was specified.")
  g <- graph.data.frame(network$edges, directed=T, vertices=network$vertices)
  if (length(colors)>1) V(g)$color <- colors
  if (length(weights)>1) E(g)$weight <- weights
  tkplot(g, layout=layout.circle, vertex.label=V(g)$name, vertex.shape="rectangle", vertex.size=50, asp=FALSE)
  return(g)
}

