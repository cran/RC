RC.traffic <-
function(pk="", echo=TRUE) {
  if (pk=="") stop("No primary key was specified.")
  mydata <- list(id=pk)
  r <- strsplit(.RC.post(.RCrepurl,"/RC.traffic.php?action=907", data.to.send=mydata, referer=.RCversion, port=80),"\n")
  r2 <- r[[1]]

  myarr <- array(NA,dim=c(length(r2),2))
  myindex <- 0
  for (iii in 1:length(r2)) {
    if (substr(r2[iii],1,3) == " ; ") {
      myindex = myindex + 1
      myarr[myindex,] = strsplit(r2[iii]," ; ")[[1]][2:3]
    }
  }
  if (echo==TRUE) message(paste("Number of valid cases found: ",myindex,".",sep=""))
  if (myindex > 0) {
    myarr <- data.frame(myarr)
    colnames(myarr) <- c("ip","count")
    myarr <- myarr[!is.na(myarr$ip),]
    return(myarr)
  }
}

