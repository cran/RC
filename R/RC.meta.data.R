RC.meta.data <-
function(url="") {
  myurl <- .RC.shorten.url(url)
  RC.parent <<- .RC.parent(myurl)
  if (myurl == "") {
    stop("Please, specify the URL of the computation.");
  } else {
    myurl <- sub("blog","blog/rc_reproduce.php?v=",myurl)
    myurl <- sub("/date","date",myurl)

    mydata <- list(submit="Search")
    r <- strsplit(.RC.post(.RCrepurl,myurl, data.to.send=mydata, referer=.RCversion, port=80),"\n")
    #check if private
    mypar <- array(NA,dim=20)
    re <- r[[1]]
    nrows <- length(re)
    for (i in 1:nrows) {
      if (substr(re[i],1,38) == "Cannot reproduce private computations.") {
        stop("This computation is private.")
      }
    }
    mytitle <- ""
    mytype <- ""
    mytarget <- ""
    myparent <- ""
    myformula <- ""
    mydataarr <- NA
    mychartwidth <- ""
    mychartheight <- ""
    mychartylab <- ""
    mychartxlab <- ""
    myylimmin <- ""
    myylimmax <- ""
    mydate <- ""
    myuid <- ""
    myoutput <- ""
    myrawoutput <- ""
    myrmodulecode <- ""
    myrawinput <- ""
    for (i in 1:nrows) {
      if (substr(re[i],1,6) == "output") {
	outputnchar <- nchar(re[i])
	myoutput <- substr(re[i],9,outputnchar)
      }
      if (substr(re[i],1,4) == "date") {
        dum <- strsplit(re[i]," ; ")
        mydate <- dum[[1]][2]
      }
      if (substr(re[i],1,4) == "title") {
        dum <- strsplit(re[i]," ; ")
        mytitle <- dum[[1]][2]
      }
      if (substr(re[i],1,4) == "type") {
        dum <- strsplit(re[i]," ; ")
        mytype <- dum[[1]][2]
      }
      if (substr(re[i],1,8) == "rawinput") {
        dum <- strsplit(re[i]," ; ")
        myrawinput <- dum[[1]][2]
        if (mytype=='R module') myrawinput <- .RC.post(.RCrepurl,myrawinput, data.to.send=list(submit="R console"), referer=.RCversion, port=80)
        if (mytype=='Rscript') myrawinput <- gsub("#n#","\n",myrawinput)
      }
      if (substr(re[i],1,11) == "rmodulecode") {
        dum <- strsplit(re[i]," ; ")
        myrmodulecode <- dum[[1]][2]
        if (mytype=='Rscript') myrmodulecode <- gsub("#n#","\n",myrmodulecode)
      }
      if (substr(re[i],1,9) == "rawoutput") {
        dum <- strsplit(re[i]," ; ")
        myrawoutput <- dum[[1]][2]
        if (mytype=='R module') myrawoutput <- .RC.post(.RCrepurl,myrawoutput, data.to.send=list(submit="R console"), referer=.RCversion, port=80)
        if (mytype=='Rscript') myrawoutput <- gsub("#n#","\n",myrawoutput)
      }
      if (substr(re[i],1,3) == "uid") {
        dum <- strsplit(re[i]," ; ")
        myuid <- dum[[1]][2]
      }
      if (substr(re[i],1,6) == "target") {
        dum <- strsplit(re[i]," ; ")
        mytarget <- dum[[1]][2]
      }
      if (substr(re[i],1,6) == "target") {
        dum <- strsplit(re[i]," ; ")
        mytarget <- dum[[1]][2]
      }
      if (substr(re[i],1,6) == "parent") {
        dum <- strsplit(re[i]," ; ")
        myparent <- dum[[1]][2]
      }
      if (substr(re[i],1,10) == "newformula") {
        dum <- strsplit(re[i]," ; ")
        dumnchar <- nchar(dum[[1]][2])
        myformula = paste(myformula,substr(dum[[1]][2],1,dumnchar-1),"\n",sep="")
        di <- FALSE
        for (j in (i+1):nrows) {
          dum <- nchar(re[j])
          if (dum == 0) {
            j = nrows
            di = TRUE
          } else {
            if (di == FALSE) myformula = paste(myformula,substr(re[j],1,dum-1),"\n",sep="")
          }
        }
        i = j
      }
      if (substr(re[i],1,4) == "data") {
        #count number of observations
        nobs <- 0
        j <- i
        dum <- nchar(re[j])
        while(substr(re[j],dum,dum)=="\r") {
          nobs = nobs + 1
          j = j + 1
          dum <- nchar(re[j])
        }

        #define array and fill it up
        mydataarr <- array(NA,dim=nobs+1)
        dum <- strsplit(re[i]," ; ")
        dumnchar <- nchar(dum[[1]][2])
        mydataarr[1] <- as.numeric(sub(",",".",substr(dum[[1]][2],1,dumnchar-1)))
        for (j in (i+1):(i+nobs)) {
          dumnchar <- nchar(re[j])
          if (j==(i+nobs)) {
            mydataarr[j-i+1] <- as.numeric(sub(",",".",substr(re[j],1,dumnchar)))
          } else {
            mydataarr[j-i+1] <- as.numeric(sub(",",".",substr(re[j],1,dumnchar-1)))
          }
        }
        i = j
      }
      for (j in 1:9) {
        if (substr(re[i],1,7) == paste("par",j," ; ",sep="")) {
          dum <- strsplit(re[i]," ; ")
          mypar[j] <- dum[[1]][2]
        }
      }
      for (j in 10:20) {
        if (substr(re[i],1,8) == paste("par",j," ; ",sep="")) {
          dum <- strsplit(re[i]," ; ")
          mypar[j] <- dum[[1]][2]
        }
      }
      if (substr(re[i],1,10) == "chartwidth") {
        dum <- strsplit(re[i]," ; ")
        mychartwidth <- dum[[1]][2]
      }
      if (substr(re[i],1,11) == "chartheight") {
        dum <- strsplit(re[i]," ; ")
        mychartheight <- dum[[1]][2]
      }
      if (substr(re[i],1,9) == "chartylab") {
        dum <- strsplit(re[i]," ; ")
        mychartylab <- sub("\"","",sub("\"","",dum[[1]][2]))
      }
      if (substr(re[i],1,9) == "chartxlab") {
        dum <- strsplit(re[i]," ; ")
        mychartxlab <- sub("\"","",sub("\"","",dum[[1]][2]))
      }
      if (substr(re[i],1,7) == "ylimmin") {
        dum <- strsplit(re[i]," ; ")
        myylimmin <- sub("\"","",sub("\"","",dum[[1]][2]))
      }
      if (substr(re[i],1,7) == "ylimmax") {
        dum <- strsplit(re[i]," ; ")
        myylimmax <- sub("\"","",sub("\"","",dum[[1]][2]))
      }

    }
    if (mytype=='R module') mydata = list(type=mytype, date=mydate, uid=myuid, title=mytitle, target=mytarget, rawinput=myrawinput, rawoutput=myrawoutput, output=myoutput, ylimmax=myylimmax, ylimmin=myylimmin, chartxlab=mychartxlab, chartylab=mychartylab, chartheight=mychartheight, chartwidth=mychartwidth, par1=mypar[1], par2=mypar[2], par3=mypar[3], par4=mypar[4], par5=mypar[5], par6=mypar[6], par7=mypar[7], par8=mypar[8], par9=mypar[9], par10=mypar[10], par11=mypar[11], par12=mypar[12], par13=mypar[13], par14=mypar[14], par15=mypar[15], par16=mypar[16], par17=mypar[17], par18=mypar[18], par19=mypar[19], par20=mypar[20], parent=myparent, data=mydataarr, newformula=myformula)
    if (mytype=='Rscript') mydata = list(type=mytype, date=mydate, rmodulecode=myrmodulecode, rawinput=myrawinput, rawoutput=myrawoutput)

    return(mydata)
  }
}

