RC.save.image <-
function(title="",keywords="",comments="",uid="",pwd="",typeofaccess="public",moratoriumdate="") {
  if (title=="") warning("No title was specified.")
  mytmpfn <- tempfile()
  save.image(file=mytmpfn,ascii=T)
  myc <- file(description=mytmpfn,open="rt")
  myimagearr <- readLines(myc)
  close(myc)
  unlink(mytmpfn)
  myimagestr <- ""
  for (i in 1:length(myimagearr)) {
    myimagestr <- paste(myimagestr,myimagearr[i],"\n",sep="")
  }
  mydata <- list(title=title,tags1=keywords,tags2=.RCprotag,tags3=RC.utag,tags4=RC.parent,comments=comments,mu=uid,bp=pwd,typeofaccess=.RC.typeofaccess(typeofaccess),dateuntil=moratoriumdate,rmodule="Rimage",rmodulecode="[No code available, this is an image file.]",rawinput=myimagestr,rawinputurl="",rawoutput="",rawoutputurl="",output="")
  r <- .RC.post(.RChomeurl,"/blogrc.wasp",data.to.send=mydata,referer=.RC.myref,port=80)
  r <- gsub("<br />","\n",r)
  r <- gsub("&amp;","&",r)
  r <- gsub("<a href=\"","",r)
  r <- gsub("\" target=\"_blank\"><u><b>this unique URL</b></u></a>","",r)
  cat(paste(r,"\n",sep=""))
}

