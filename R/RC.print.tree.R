RC.print.tree <-
function(mytree="") {
  myout <- ""
  for (i in 1:length(mytree[,1])) {
    if (i>1) {
      for (j in 2:mytree$level[i]) myout <- paste(myout,"|    ",sep="")
    }
    mycurrent <- ""
    if(mytree$current[i]==1) mycurrent <- "***"
    myout <- paste(myout, "", paste(mytree$otitle[i], "[", mytree$utitle[i], "][", mytree$date[i], "][", mytree$user[i], "]", mycurrent,sep=""),"\n",sep="")
  }
  myout <- gsub(":#","-",myout)
  cat(myout)
}

