RC.wrapper.function <-
function(metadata = "") {

  if (metadata$type == "Rscript") {
    myraw <- metadata$rawinput
    stop("Cannot 'reuse' computations that have been generated in the R console. You have to fetch the meta data and make changes manually.")
  }
  if (metadata$type == "R module") {
    myraw <- metadata$rawinput
    mycode <- RC.prepare.input(myraw)
    RC.texteval(mycode,echo=FALSE)
    #extract original (untransformed) data
    mysnippet <- paste(strsplit(mycode,")\n")[[1]][1],")\n",sep="")
    mysnippet <- strsplit(mysnippet,"x <- ")[[1]][2]
    #store original (untransformed) data in RCx
    RC.texteval( paste("RCx <- ",mysnippet,sep=""), echo=FALSE)
    #note: the number of parameters is limited to 20 (hard coded)
    mypars <- ""
    if (is.na(metadata$par1) != TRUE) mypars <- paste(mypars,"par1='",metadata$par1,"'",sep="")
    if (is.na(metadata$par2) != TRUE) mypars <- paste(mypars,",par2='",metadata$par2,"'",sep="")
    if (is.na(metadata$par3) != TRUE) mypars <- paste(mypars,",par3='",metadata$par3,"'",sep="")
    if (is.na(metadata$par4) != TRUE) mypars <- paste(mypars,",par4='",metadata$par4,"'",sep="")
    if (is.na(metadata$par5) != TRUE) mypars <- paste(mypars,",par5='",metadata$par5,"'",sep="")
    if (is.na(metadata$par6) != TRUE) mypars <- paste(mypars,",par6='",metadata$par6,"'",sep="")
    if (is.na(metadata$par7) != TRUE) mypars <- paste(mypars,",par7='",metadata$par7,"'",sep="")
    if (is.na(metadata$par8) != TRUE) mypars <- paste(mypars,",par8='",metadata$par8,"'",sep="")
    if (is.na(metadata$par9) != TRUE) mypars <- paste(mypars,",par9='",metadata$par9,"'",sep="")
    if (is.na(metadata$par10) != TRUE) mypars <- paste(mypars,",par10='",metadata$par10,"'",sep="")
    if (is.na(metadata$par11) != TRUE) mypars <- paste(mypars,",par11='",metadata$par11,"'",sep="")
    if (is.na(metadata$par12) != TRUE) mypars <- paste(mypars,",par12='",metadata$par12,"'",sep="")
    if (is.na(metadata$par13) != TRUE) mypars <- paste(mypars,",par13='",metadata$par13,"'",sep="")
    if (is.na(metadata$par14) != TRUE) mypars <- paste(mypars,",par14='",metadata$par14,"'",sep="")
    if (is.na(metadata$par15) != TRUE) mypars <- paste(mypars,",par15='",metadata$par15,"'",sep="")
    if (is.na(metadata$par16) != TRUE) mypars <- paste(mypars,",par16='",metadata$par16,"'",sep="")
    if (is.na(metadata$par17) != TRUE) mypars <- paste(mypars,",par17='",metadata$par17,"'",sep="")
    if (is.na(metadata$par18) != TRUE) mypars <- paste(mypars,",par18='",metadata$par18,"'",sep="")
    if (is.na(metadata$par19) != TRUE) mypars <- paste(mypars,",par19='",metadata$par19,"'",sep="")
    if (is.na(metadata$par20) != TRUE) mypars <- paste(mypars,",par20='",metadata$par20,"'",sep="")
    mybody <- metadata$newformula
    mybody <- sub("load\\(file='createtable')","source\\('http://www\\.wessa\\.net/cretabc')",mybody)
    mybody <- gsub("^a<-table\\.start\\()\n$","a<-table\\.start\\(a)",mybody)
    mybody <- gsub("table\\.start","table\\.start(a)#",mybody)
    mybody <- gsub("\\bx\\b","RCx",mybody)
    mybody <- gsub("bitmap","RC.start.plot\n#bitmap",mybody)
    mybody <- gsub("dev.off()","RC.end.plot\n#dev.off",mybody)
    mybody <- gsub("table.save","#table.save",mybody)
    mybody.arr <- strsplit(mybody,"\n")[[1]]
    mybody.new <- ""
    for (i in 1:length(mybody.arr)) {
      if (substr(mybody.arr[i],1,1) != '#') {
        mybody.new <- paste(mybody.new,mybody.arr[i],"\n",sep="")
      }
    }
    RC.texteval(paste("RC.fun <- function(",mypars,") { \n",mybody.new,"\n}\n",sep=""),echo=FALSE)
  }
}

