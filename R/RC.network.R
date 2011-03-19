RC.network <-
function(keyword="workshop", elaborate=TRUE, computations=1, quote=TRUE, startdate="1970-01-01",enddate="4000-12-31") {
  if (nchar(keyword) < 3) stop("Keyword must be of length 3 or more.")
  #require(date)
  message("Fetching data...")
  mnoc <- computations - 1
  r <- RC.ls(keyword=keyword, quote=quote, echo=FALSE)
  message("Filtering users and dates...")
  startdate <- as.Date(startdate,order="ymd")
  enddate <- as.Date(enddate,order="ymd")
  mydate <- as.Date(substr(as.character(r$date),1,10),order="ymd")
  for (i in 1:length(r[,1])) {
    if ((mydate[i] >= startdate) & (mydate[i] <= enddate)) {
    } else {
      r[i,"user"] <- NA
    }
  }
  r <- r[r$user!="",]
  r <- r[!is.na(r$user),]
  if (length(r[,1]) < 1) return()

  message("Creating key-user index...")
  index <- list()
  for (i in 1:length(r[,1])) {
    index[as.character(r$key[i])] <- list(as.character(r$user[i]))
  }
  r <- cbind(as.character(r$user),as.character(r$parent))

  r <- r[r[,2]!="parent",]
  r <- r[r[,2]!="",]
  if (length(r[,1]) < 1) return()

  message("Searching user names of each parent. This may take a while...")
  ni <- length(r[,1])
  for (i in 1:ni) {
    if (is.null(index[[r[i,2]]])==FALSE) {
      r[i,2] <- as.character(index[[r[i,2]]])
    } else {
      if (elaborate == TRUE) {
        dum <- RC.ls(keyword=r[i,2],echo=FALSE)
        if (dum$parent[1] == r[i,2]) {
          index[as.character(r[i,2])] <- list(as.character(dum$user[1]))
          r[i,2] <- as.character(dum$user[1])
        }
      }
    }
  }
  r <- r[r[,1]!=r[,2],]

  message("Computing sociomatrix. This may take a while...")
  mytabparent <- table(paste("_",r[,2],sep=""))
  mytabchild <- table(paste("_",r[,1],sep=""))
  nparents <- length(mytabparent)
  nchildren <- length(mytabchild)
  x <- array(NA,dim=c(nparents,nchildren))
  x <- as.array(table(paste("_",r[,2],sep=""), paste("_",r[,1],sep="")))
  df <- array(NA,dim=c(nparents*nchildren,3))
  rowc <- 0
  for (j in 1:nchildren) {
    vcount <- 0
    for (i in 1:nparents) {
      plab <- rownames(mytabparent)[i]
      clab <- rownames(mytabchild)[j]
      dum <- 0		
      try(dum <- x[clab,plab],T)
      if (x[i,j]-dum > 0) {
        rowc <- rowc + 1
        df[rowc,1] <- rownames(mytabparent)[i]
        df[rowc,2] <- rownames(mytabchild)[j]
        df[rowc,3] <- x[i,j] - dum
      }
      vcount <- vcount + x[i,j] - dum
    }
  }
  truelengthdf <- length(df[!is.na(df[,1]),1])
  df <- df[1:truelengthdf,]
  message("Computing ordered vertices...")
  vrn <- unique(c(rownames(mytabparent), rownames(mytabchild)))
  v <- array(0,dim=c(length(vrn),2))
  for (h in 1:length(vrn)) {
    dump <- 0
    dumm <- 0
    try(dump <- as.numeric(mytabparent[vrn[h]]),T)
    try(dumm <- as.numeric(mytabchild[vrn[h]]),T)
    if (is.na(dump)) dump <- 0
    if (is.na(dumm)) dumm <- 0
    v[h,] <- c(vrn[h], dump-dumm)
  }
  message("Applying user-defined filter...")
  edges <- df[df[,3]>mnoc,]
  edges <- as.data.frame(array(as.factor(edges),dim=c(length(edges[,1]),3)))
  vertices <- v[order(as.numeric(v[,2])),]
  message("Done.")
  return(list(edges=edges, vertices=vertices))
}

