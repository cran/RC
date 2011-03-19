RC.network.list <-
function(bloggers=NULL, elaborate=TRUE, computations=1, quote=TRUE, startdate="1970-01-01", enddate="4000-12-31", vertex.attributes=NULL) {
  if (is.null(bloggers)) stop("Vector of bloggers is required.")
  if (typeof(bloggers) != "character") stop("Bloggers must be contained in a vector of type 'character'.")
  if (!is.null(vertex.attributes)) {
    if (length(vertex.attributes[,1]) != length(bloggers)) stop("Attributes array must have the same number of rows as the vector of bloggers.")
  }
  n <- length(bloggers)
  for (i in 1:n) {
    message("Fetching network for ",paste(bloggers[i],sep=""))
    if (nchar(as.character(bloggers[i])) > 0) {
      r1 <- NULL
      try(r1 <- RC.network(keyword=bloggers[i], elaborate=elaborate, computations=computations, quote=quote, startdate=startdate, enddate=enddate),silent=T)		
      if (!is.null(r1)) {
        if (i==1) {
          mykey <- list(bloggers[i])
          myname <- list(r1$vertices[1,1])
        } else {
          mykey <- c(mykey, bloggers[i])
          myname <- c(myname, r1$vertices[1,1])
        }
        if (i==n) {
          message("Computing vertex counts. This may take a while...")
          r <- RC.igraph.union(r,r1,TRUE)
          if (!is.null(vertex.attributes)) {
            message("Adding attributes...")
            newv <- array(NA,dim=c(length(r$vertices[,1]),2+length(vertex.attributes[1,])))
            for (iii in 1:length(r$vertices[,1])) {
              mysearchkey <- 0
              for (jjj in 1:length(myname)) {
                if (r$vertices[iii,1] == myname[[jjj]]) mysearchkey = jjj
              }
              newv[iii,1:2] = r$vertices[iii,]
              if (mysearchkey != 0) {
                newv[iii,3:length(newv[1,])] = vertex.attributes[mysearchkey,]
              }
            }
            #colnames(newv) <- colnames(vertex.attributes)
            r$vertices = newv
            colnames(r$vertices) <- c("name","count",colnames(vertex.attributes))
          }
        } else {
          if (i==1) {
            r <- r1
          } else {
            r <- RC.igraph.union(r,r1)
          }
        }
      }
    }
  }
  return(r)
}

