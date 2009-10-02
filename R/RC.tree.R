RC.tree <-
function(url="") {
  myurl <- .RC.shorten.url(url)
  if (myurl == "") {
    stop("Please, specify the URL of the computation.");
  } else {
    myurl <- sub("blog","blog/rc_tree.php?v=",myurl)
    myurl <- sub("/date","date",myurl)
    mydata <- list(submit="Tree")
    r <- strsplit(.RC.post(.RCrepurl,myurl, data.to.send=mydata, referer=.RCversion, port=80),"\n")
    re <- r[[1]]
    nrows <- length(re)
    #count number of objects in tree
    numob <- 0
    options(warn = -1)
    for (i in 1:nrows) {
      if(length(grep("freestatistics.org",re[i]))>0) {
        numob = numob+1
      }
    }
    options(warn = 0)
    mytree <- array(NA,dim=c(numob,12))
    colnames(mytree) <- c("forum", "rcode", "module", "parameters", "data", "level", "url", "otitle", "utitle", "date", "user", "current")
    myindex <- 0
    options(warn = -1)
    for (i in 1:nrows) {
      if(length(grep("freestatistics.org",re[i]))>0) {
        myindex = myindex + 1
        mytree[myindex,] <- strsplit(re[i],";")[[1]]
      }
    }
    options(warn = 0)
    mytree <- as.data.frame(mytree)
    mytree$url <- as.character(mytree$url)
    mytree$user <- as.character(mytree$user)
    mytree$otitle <- as.character(mytree$otitle)
    mytree$utitle <- as.character(mytree$utitle)
    mytree$forum <- as.character(mytree$forum)
    mytree$rcode <- as.character(mytree$rcode)
    mytree$module <- as.character(mytree$module)
    mytree$parameters <- as.character(mytree$parameters)
    mytree$data <- as.character(mytree$data)
    mytree$date <- as.character(mytree$date)

    return(mytree)
  }
}

