RC.hello <-
function() {
system.time({
  cat("Calling R Framework server network. This may take a while...\n")
  mydata <- list(dum="")
  cat(.RC.post(.RChomeurl,"/RC/hellorc.wasp",data.to.send=mydata,referer=.RCversion,port=80))
})
}

