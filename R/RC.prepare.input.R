RC.prepare.input <-
function(rawinput) {
  myrawinput = gsub("dev.off()","#dev.off()",rawinput)
  myrawinput = gsub("postscript","#postscript",myrawinput)
  myrawinput = gsub("table.save","#table.save",myrawinput)
  myrawinput = gsub("system","#system",myrawinput)
  myrawinput = gsub("'","\"",myrawinput)
  #myrawinput = gsub("a<-","RC.a",myrawinput)
  #myrawinput = gsub("(a)","(RC.a)",myrawinput)

  myrawinput = sub("HTTP","#HTTP",myrawinput)
  myrawinput = sub("Date:","#Date:",myrawinput)
  myrawinput = sub("Server:","#Server:",myrawinput)
  myrawinput = sub("Last-Modified:","#Last-Modified:",myrawinput)
  myrawinput = sub("ETag:","#ETag:",myrawinput)
  myrawinput = sub("Accept-Ranges:","#Accept-Ranges:",myrawinput)
  myrawinput = sub("Content-Length:","#Content-Length:",myrawinput)
  myrawinput = sub("Connection:","#Connection:",myrawinput)
  myrawinput = sub("Content-Type:","#Content-Type:",myrawinput)
  myrawinput = sub("X-Pad:","#X-Pad:",myrawinput)
  myrawinput = sub("file=\"/var/www/html/rcomp/createtable\"", paste("'http://",.RChomeurl,"/cretabc'",sep=""), myrawinput)
  myrawinput = sub("file=\"/var/www/html/freestat/rcomp/createtable\"", paste("'http://",.RChomeurl,"/cretabc'",sep=""), myrawinput)
  myrawinput = gsub("table.start()","table.start(a)#",myrawinput)
  myrawinput = paste("a <- ''\n",myrawinput,sep="")
  myrawinput = gsub("load","source",myrawinput)
  #to do: now delete all lines that start with #

  return(myrawinput)
}

