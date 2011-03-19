RC.feedback.computation <-
function(pk="", echo=TRUE) {
  if(pk=="") stop("No User Primary Key was specified.")
  mydata <- list(pk=pk, submit="Search")
  r <- strsplit(.RC.post(.RCrepurl,"/index.php?action=908", data.to.send=mydata, referer=.RCversion, port=80),"\n",fixed=T)
  if (is.na(r)) stop("Error in query engine on freestatistics.org.")
  r2 <- r[[1]]
  if(length(r2)<2) {
    if(is.na(r2)) {
      stop("No records found.", call. = FALSE)
    }
  }
  myarr <- array(NA,dim=c(length(r2),16))
  myindex <- 0
  for (iii in 1:length(r2)) {
    if (substr(r2[iii],1,5) == "pk ; ") {
      myindex = myindex + 1
      myarr[myindex,] = strsplit(r2[iii]," ; ",fixed=T)[[1]]
    }
  }
  if (echo==TRUE) {
    message(paste("Number of valid cases found: ",myindex,".",sep=""))
    if (myindex == 1000) message("Note: the query engine never returns more than 1000 records!")
  }
  if (myindex > 0) {
    myarr <- cbind(myarr[,2], myarr[,4], myarr[,6], myarr[,8], myarr[,10], myarr[,12], myarr[,14], myarr[,16])
    colnames(myarr) <- c("pk","pk_frcomp","username","userid","text","date","pk_parent","usernamenotvisible")
    myarr <- myarr[!is.na(myarr[,"pk"]),]
    for (i in 1:length(myarr[,1])) myarr[i,"text"] <- .RC.decode(as.character(myarr[i,"text"]))
    myarr <- data.frame(myarr)
    return(myarr)
  }
}

