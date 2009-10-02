RC.pk <-
function(url="", echo=TRUE) {
  if (url=="") stop("No url was specified.")
  url <- .RC.shorten.url(url)
  url <- strsplit(url,"/")[[1]]
  folder <- paste("/blog/date/",url[6],"/",url[7],"/",url[8],"/",sep="")
  url <- url[9]
  nc <- nchar(url)
  url = substr(url,1,nc-4)
  mydata <- list(id=url,folder=folder)
  r <- strsplit(.RC.post(.RCrepurl,"/RC.pk.php?action=907", data.to.send=mydata, referer=.RCversion, port=80),"\n")
  r <- as.numeric(r[[1]][length(r[[1]])])
  return(r)
}

