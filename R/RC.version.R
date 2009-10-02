RC.version <-
function(title=FALSE) {
  if (title==TRUE) cat(paste(.RC.title(),"version ",.RCversion,"\n",sep=""))
  return(.RCversion)
  #print(paste("Reproducible Computing package - version", .RCversion))
}

