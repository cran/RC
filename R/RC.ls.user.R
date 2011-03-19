RC.ls.user <-
function(id="", echo=TRUE) {
  if (id=="") stop("No user id was specified.")
  mydata <- list(id=id)
  r <- strsplit(.RC.post(.RCrepurl,"/RC.user.php?action=907", data.to.send=mydata, referer=.RCversion, port=80),"\n")
  r2 <- r[[1]]

  myarr <- array(NA,dim=c(length(r2),11))
  myindex <- 0
  for (iii in 1:length(r2)) {
    if (substr(r2[iii],1,3) == " ; ") {
      myindex = myindex + 1
      myarr[myindex,] = strsplit(r2[iii]," ; ")[[1]][2:12]
    }
  }
  if (echo==TRUE) message(paste("Number of valid cases found: ",myindex,".",sep=""))
  if (myindex == 1000) message("Note: the query engine only returned the first 1000 records!")
  if (myindex > 0) {
    myarr <- data.frame(myarr)
    colnames(myarr) <- c("url","key","folder","date","module","title","keywords","parent","message", "isprivate", "pk")
    myarr <- myarr[!is.na(myarr$key),]
    return(myarr)
  }
}

