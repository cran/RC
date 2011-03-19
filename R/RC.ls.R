RC.ls <-
function(uid="",pwd="",keyword="AS2009",quote=TRUE, echo=TRUE, course="", module="") {
  if (quote==TRUE) {
    if (length(grep(" ",keyword))>0) {
      if (echo==TRUE) message(paste("Changing keyword into '",keyword,"'...",sep=""))
      keyword <- paste("'",keyword,"'",sep="")
    }
  }
  if (echo==TRUE) message("Fetching list from FreeStatistics.org archive...")
  if (uid != "") {
    mydata <- list(frmloginname=uid,frmloginpwd=pwd,login="rlogin")
    r <- .RC.post(.RCrepurl,"/index.php?action=8",data.to.send=mydata,referer=.RCversion,port=80)
    if (is.na(r)) stop("Error in query engine on freestatistics.org.")
    nr <- nchar(r)
    rrep <- substr(r,nr-1,nr)
    #extract cookie
    myrarr <- strsplit(r,"\r\n")
    for (iii in 1:length(myrarr[[1]])) {
      if (substr(myrarr[[1]][iii],1,21) == "Set-Cookie: FREESTAT=") {
        mycookie <- substr(myrarr[[1]][iii],22,nchar(myrarr[[1]][iii])-8)
      }
    }
    if (rrep == "ok") {
      #to do: fix spelling!
      mydata <- list(frmloginname=uid, frmloginpwd=pwd, query=keyword, datefrom="", dateuntill="", fldUsertime="", FREESTAT=mycookie, submit="Search")
      r2 <- strsplit(.RC.post(.RCrepurl,"/index.php?action=912", data.to.send=mydata, referer=.RCversion, port=80, cookie=mycookie),"\n",fixed=T)
      if (is.na(r2)) stop("Error in query engine on freestatistics.org.")
      r2 <- r2[[1]]
      myarr <- array(NA,dim=c(length(r2),11))
      myindex <- -1
      for (iii in 1:length(r2)) {
        if (substr(r2[iii],1,3) == " ; ") {
          myindex = myindex + 1
          myarr[myindex,] = strsplit(r2[iii]," ; ",fixed=T)[[1]]
        }
      }
      myarr <- data.frame(myarr)
      colnames(myarr) <- c("url","key","folder","date","module","title","keywords","course","user","parent","message")
      myarr <- myarr[!is.na(myarr$key),]
      myarr$url <- paste("http://",.RCrepurl,myarr$folder,myarr$key,".htm",sep="")
      myarr
    } else {
      if (echo==TRUE) message(paste("Cannot fetch list from archive. The following error message was received: ",r,".",sep=""))
    }
  } else {
    #do ordinary search
    mydata <- list(query=keyword, course=course, module=module, submit="Search")
    r <- strsplit(.RC.post(.RCrepurl,"/index.php?action=907", data.to.send=mydata, referer=.RCversion, port=80),"\n",fixed=T)
    if (is.na(r)) stop("Error in query engine on freestatistics.org.")
    r2 <- r[[1]]
    if(length(r2)<2) {
      if(is.na(r2)) {
        stop("No records found.", call. = FALSE)
      }
    }
    myarr <- array(NA,dim=c(length(r2),11))
    myindex <- 0
    for (iii in 1:length(r2)) {
      if (substr(r2[iii],1,3) == " ; ") {
        myindex = myindex + 1
	      #print(myindex)
	      if(myindex==418) {
		      #print(r2[iii])
		      print(keyword)
	      }
        myarr[myindex,] = strsplit(r2[iii]," ; ",fixed=T)[[1]]
      }
    }
    if (echo==TRUE) message(paste("Number of valid cases found: ",myindex,".",sep=""))
    if (myindex == 1000) message("Note: the query engine only returned the first 1000 records!")
    if (myindex > 0) {
      myarr <- data.frame(myarr)
      colnames(myarr) <- c("url","key","folder","date","module","title","keywords","course","user","parent","message")
      myarr <- myarr[!is.na(myarr$key),]
      myarr$url <- paste("http://",.RCrepurl,myarr$folder,myarr$key,".htm",sep="")
      if (length(course)>1) myarr <- myarr[myarr$course==course,]
      if (length(module)>1) myarr <- myarr[myarr$module==module,]
      return(myarr)
    }
  }
}

