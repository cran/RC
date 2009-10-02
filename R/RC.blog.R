RC.blog <-
function(title="",keywords="",comments="",uid="",pwd="",typeofaccess="public",moratoriumdate="",rcode) {
  #there are 20 hard-coded parameters (can be automatically converted to fields in a web form)
  RCpar <- array("",dim=c(5,20))
  colnames(RCpar) <- paste("par",1:20,sep="")
  rownames(RCpar) <- list("value","default","label","tooltip","type")

  #plain text of rcode
  #strcode is used to create the datainput and init code
  #strcode1 is executed: the output text is returned and blogged
  #strcode2 is executed (silently) with the sole purpose to create the postscript code that can be blogged
  strcode2 <- strcode1 <- strcode <- .RC.fun2string(body(rcode))

  #are other people allowed to access your computation at freestatistics.org?
  if (typeofaccess == "public")     typeofaccesscode = 1 #anybody can access my computation
  if (typeofaccess == "moratorium") typeofaccesscode = 2 #computation is private until a certain date (a valid account is required!)
  if (typeofaccess == "private")    typeofaccesscode = 3 #nobody else can access my computation (a valid account is required!)
  #Note: 'dateuntil' or 'moratorium' date (YYYY-MM-DD) - only needed if 'Moratorium' is selected in 'Type of Access'

  typeofaccesscode <- .RC.typeofaccess(typeofaccess)
  mydat <- ""
  postnameslist <- ""
  if (is.array(RCx) | is.data.frame(RCx)) {
    if (length(t(RCx)) > 1) {
      #first check is RCz or RCy is used (not allowed when RCx is array or dataframe)
      gRCy <- grep("RCy",strcode)
      if (length(gRCy) > 0) {
	if (gRCy == 1) stop("RCy is not allowed when RCx is array or data frame.")
      }
      gRCz <- grep("RCz",strcode)
      if (length(gRCz) > 0) {
	if (gRCz == 1) stop("RCz is not allowed when RCx is array or data frame.")
      }
      RCy <- vector()
      RCz <- vector()
      if (is.data.frame(RCx)) RCxnames <- colnames(RCx)
      cx <- length(RCx[1,])
      rx <- length(RCx[,1])
      if (is.na(RCxnames[1])) RCxnames <- paste("V",1:cx,sep="")
      mynameslist <- list(RCxnames,1:rx)
      snlist <- deparse(mynameslist)
      postnameslist <- RCxnames[1]
      for (i in 2:length(RCxnames)) postnameslist <- paste(postnameslist,RCxnames[i],sep=" ")
      mydat <- paste(mydat,"x <- array(",.RC.data2string(as.vector(t(RCx))),",dim=c(",cx,",",rx,"),dimnames=",snlist,")\n",sep="")
      mydat <- paste(mydat,"y <- array(NA,dim=c(",cx,",",rx,"),dimnames=",snlist,")\n",sep="")
      mydat <- paste(mydat,"for (i in 1:dim(x)[1]) {\n","	for (j in 1:dim(x)[2]) {\n","		y[i,j] <- as.numeric(x[i,j])\n","	}\n","}\n",sep="")
      mydat <- paste(mydat,"x <- as.data.frame(t(y))\n",sep="")
    }
  }
  if (is.vector(RCx)) {
    if (length(RCx) > 1) mydat <- paste(mydat,"x <- ",.RC.data2string(RCx),"\n",sep="")
    if (length(RCy) > 1) mydat <- paste(mydat,"y <- ",.RC.data2string(RCy),"\n",sep="")
    if (length(RCz) > 1) mydat <- paste(mydat,"z <- ",.RC.data2string(RCz),"\n",sep="")
  }
  myinp <- ""
  myinit <- ""
  myout <- ""
  funstr <- deparse(rcode)[1]
  funstr <- substr(funstr,11,nchar(funstr)-2)
  funstrarray <- strsplit(funstr,split=",")[[1]]
      strcode <- gsub("RCx","x",strcode)
      strcode <- gsub("RCy","y",strcode)
      strcode <- gsub("RCz","z",strcode)
  RCargs <- length(funstrarray)
  myrealtype <- "character" #this default is necessary to prevent an error when there are no parameters
  if (RCargs>0) {
    for (i in 1:RCargs) {
      myout <- paste(myout,RC.texteval(funstrarray[i],collapse="\n"),sep="")
      mypar <- .RC.trim(strsplit(funstrarray[i],split="=")[[1]][1])
      mynewpar <- paste("par",i,sep="")
      strcode <- gsub(mypar,mynewpar,strcode) #replace mypar by mynewpar in strcode
      myval <- .RC.trim(strsplit(funstrarray[i],split="=")[[1]][2])
      myval <- gsub("\"","'",myval,fixed=T)
      mytype <- ""
      RC.texteval(paste("mytype <- class(",mypar," = ",myval,")",sep=""), collapse="", echo=F)
      if (mytype == "numeric") {
        myinp <- paste(myinp,mynewpar," <- '",myval,"'\n",sep="") 
        myinit <- paste(myinit,mynewpar," <- as.numeric(",mynewpar,")\n",sep="")
        myrealtype <- "numeric"
      }
      if (mytype == "character") {
        myinp <- paste(myinp,mynewpar," <- ",myval,"\n",sep="") 
        myinit <- paste(myinit,mynewpar," <- as.character(",mynewpar,")\n",sep="")
        myrealtype <- "character"
      }
      #check if there's a comment: use it as tooltip
      mytip <- "" #try(RC.texteval(paste("attr(",mypar,",\"comment\")",sep="")))
      RCpar[,i] <- c(myval,myval,mypar,mytip,myrealtype)
    }
  }
  #remove all RC.*.plot from strcode
  strcode <- gsub("RC.start.plot","",strcode) 
  strcode <- gsub("RC.end.plot","",strcode)
  rawinput <- paste(mydat,myinp,myinit,strcode,sep="") #init of pars + rcode
  #before we execute the R code we need to find all RC.*.plot tags and insert additional code for the postscript device
  gr  <- gregexpr("RC.start.plot",strcode2,fixed=T)
  numpics <- length(gr[[1]])
  if (gr[[1]][1] != -1) {
    if (numpics > 0) {
      for (i in 1:numpics) {
        gr  <- gregexpr("RC.start.plot",strcode2,fixed=T)
        if (gr[[1]][1]==-1) stop("No 'RC.start.plot' found")
        grn <- gregexpr("RC.end.plot",strcode2)
        if (grn[[1]][1]==-1) stop("No 'RC.end.plot' found")
        if (grn[[1]][1] == -1) {
          tn <- Inf #obsolete
        } else {
          term <- grn[[1]][1]
        }
        myplot <- substr(strcode2,gr[[1]][1]+8+7-1,term-5)
        mycode <- paste("RCpicnr <- RCpicnr + 1\npostscript(file = 'Rplot.ps',horizontal=F,pagecentre=F,paper='special',width=8.3333333333333,height=5.5555555555556)\n",myplot,"\ndev.off()\npscon <- file('Rplot.ps','r')\npsarr <- readLines(pscon)\npsstr <- ''\nfor (i in 1:length(psarr)) psstr <- paste(psstr,psarr[i],'","\\","n',sep='')\nclose(pscon)\nRCpicarr[RCpicnr] <- psstr\n",sep="")
        #change strcode2 to create postscript
        strcode2 <- paste(substr(strcode2,1,gr[[1]][1]-5),mycode,substr(strcode2,term+12,nchar(strcode2)),sep="")
      }
    }
  }
  strcode2 <- paste("RCpicarr <- array(NA,dim=",numpics,")\nRCpicnr <- 0\n",strcode2,sep="")
  #now execute strcode2 (silently): this creates RCpicnr pictures in postscript which are contained in RCpicarr
  dum <- paste(myout, RC.texteval(strcode2,collapse="\n"), sep="")
  mypicdata <- list()
  for (i in 1:length(RCpicarr)) {
    mypicdata <- c(mypicdata,RCpicarr[i])
    names(mypicdata)[i] <- paste("pic",i,sep="")
  }
  #remove all RC.*.plot from strcode1
  strcode1 <- gsub("RC.start.plot","",strcode1)
  strcode1 <- gsub("RC.end.plot","",strcode1)
  #execute strcode1 and capture output to be blogged
  output <- paste(myout, RC.texteval(strcode1,collapse="\n"), sep="")
  output <- gsub("\"","'",output)
  if (nchar(uid) > 0) RC.utag <<- uid else uid <- RC.utag
  mydata <- list(title=title, tags1=keywords, tags2=.RCprotag, tags3=RC.utag, tags4=RC.parent, comments=comments, mu=uid, bp=pwd, typeofaccess=typeofaccesscode, dateuntil=moratoriumdate, rmodule="Rscript", rmodulecode=strcode, rawinput=rawinput, rawinputurl="", rawoutput=output, rawoutputurl="", output="see: raw output", par1=RCpar[1,1], par2=RCpar[1,2], par3=RCpar[1,3], par4=RCpar[1,4], par5=RCpar[1,5], par6=RCpar[1,6], par7=RCpar[1,7], par8=RCpar[1,8], par9=RCpar[1,9], par10=RCpar[1,10], par11=RCpar[1,11], par12=RCpar[1,12], par13=RCpar[1,13], par14=RCpar[1,14], par15=RCpar[1,15], par16=RCpar[1,16], par17=RCpar[1,17], par18=RCpar[1,18], par19=RCpar[1,19], par20=RCpar[1,20], datax=.RC.data2string1(RCx), datay=.RC.data2string1(RCy), dataz=.RC.data2string1(RCz), namesx=postnameslist)
  #append postscript pictures
  if (length(mypicdata) > 0) mydata <- c(mydata,mypicdata)
  #blog it!
  r <- .RC.post(.RChomeurl,"/blogrc.wasp",data.to.send=mydata,referer=.RC.myref,port=80)

  #remove certain html tags from the result page
  r <- gsub("<br />","\n",r)
  r <- gsub("&amp;","&",r)
  r <- gsub("<a href=\"","",r)
  r <- gsub("\" target=\"_blank\"><u><b>this unique URL</b></u></a>","",r)
  cat(paste(r,"\n",sep=""))
  r <- strsplit(r,"submission at ")
  r <- r[[1]][2]
  r <- substr(r,1,nchar(r)-1)
  return(r)
}

