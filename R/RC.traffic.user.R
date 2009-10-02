RC.traffic.user <-
function(id="", echo=TRUE) {
  if (id=="") stop("No user id was specified.")
  r <- RC.ls.user(id=id, echo=echo)
  nc <- length(r[,1])
  for (i in 1:nc) {
    dum <- as.list(RC.traffic(r$pk[i],echo=echo))
    dum.ip <- as.vector(dum$ip)
    dum.count <- as.vector(dum$count)
    dum.pk <- as.character(r$pk[i])
    if (i>1) { 
      traffic.ip <- append(traffic.ip, dum.ip)
      traffic.count <- append(traffic.count, dum.count) 
      traffic.pk <- append(traffic.pk, rep(dum.pk, length(dum.ip)))
    } else {
      traffic.ip <- dum.ip
      traffic.count <- dum.count
      traffic.pk <- rep(dum.pk, length(dum.ip))
    }
  }
  traffic <- as.data.frame(cbind(traffic.ip, traffic.count, traffic.pk))
  colnames(traffic) = c("ip","count","pk")
  tab <- table(traffic$ip, traffic$count)
  tablab <- labels(tab)[[1]]
  l <- length(tablab)
  stat <- array(NA,dim=c(l,2))
  rownames(stat) <- tablab
  for (i in 1:l) {
    stat[i,1] <- sum(tab[i,])
    stat[i,2] <- mean(tab[i,])
  }
  colnames(stat) <- c("sum","mean")
  stat <- as.data.frame(stat)
  ret <- list(traffic=traffic, statistics=stat)
  return(ret)
}

