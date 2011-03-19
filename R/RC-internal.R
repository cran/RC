.onAttach <-
function(libname, pkgName) {
  .RCtitle <- paste(.RC.title(),"version ",.RCversion,sep="")
  Sys.setlocale('LC_ALL','C')
  library(igraph)
  library(bitops)
  #library(date)
  message("Connecting to remote server...\n")
  if (readLines("http://www.wessa.net/RC/RCbaseversion") != .RCbaseversion)
    stop("The currently installed package is out of date. You must update the package before you are able to use the RC package again. You can update your packages with the following command: update.packages(repos='http://www.freestatistics.org/cran'). As an alternative you can also download the latest source with the following command: source('http://www.wessa.net/RC/rc.r')")
  if (readLines("http://www.wessa.net/RC/RCversion") != .RCversion) {
      warning(paste(.RCtitle,"\n",.RCterms,"\n\n","*** WARNING ***\n\nA new version of the RC package is available. It is strongly advised that you update the package in the near future. Some features may have been disabled in the currently installed package.",sep="")) 
  }
  else {
      message(paste(.RCtitle,"\n",.RCterms,sep=""))
  }
  RC.parent <<- ""
  RC.utag <<- ""
  RCx <<- vector()
  RCxnames <<- array()
  RCy <<- vector()
  RCz <<- vector()
  RCpicnr <<- 0
  RCpicarr <<- list()
  RC.resultText <<- ""
  RC.demo.res <<- ""
  RC.demo.r <<- ""
  RC.demo.mycode <<- ""
  RC.demo.mytree.1 <<- ""
  RC.demo.mytree.2 <<- ""
  RC.demo.traffic <<- ""
  RC.mystep <<- 0
  RC.fun <<- ""
}

.RCprotag <-
"R console"
.RC.myref <-
"Linux 2.6.35-28-generic"
.RC.trim <-
function(x) sub(" *([^ ]+) *", "\\1", x)

.RC.printed <-
function (sourceText, collapse = NULL) {
    return(RC.texteval(sourceText, collapse, FALSE))
}

.RC.fun2string <-
function(expression) {
  dum <- deparse(expression)
  myret <- ""
  for (i in 1:length(dum)) {
    myret <- paste(myret,dum[i],sep="\n")
  }
  myret
}

.RC.data2string <-
function(data) {
  dum <- .RC.fun2string(data)
  dum <- gsub("\n","",dum)
  dum <- gsub("\"","",dum)
  dum <- gsub(", ,",",",dum)
  dum <- gsub(", )","",dum)
  dum
}

.RC.data2string1 <-
function(data) {
  myret <- ""
  if (is.array(data) | is.data.frame(data)) {
    myret <- ""
    for (myrow in 1:length(data[,1])) {
      myret <- paste(myret,data[myrow,1],sep="")
      for (mycol in 2:length(data[1,])) {
	myret <- paste(myret,data[myrow,mycol],sep="\t")
      }
      myret <- paste(myret,"\n",sep="")
    }
  }
  if (is.vector(data)) {
    myret <- data[1]
    for (i in 2:length(data)) {
      myret <- paste(myret,data[i],sep="\n")
    }
  }
  myret
}

#simplified decoder based on base64decode from caTools package
.RC.decode <- function (z) {
    require(bitops)
    z = strsplit(z, NULL)[[1]]
    if (length(z)%%4 != 0) warning("In .RC.decode: length of data is not a multiple of 4.")
    alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
    alpha = strsplit(alpha, NULL)[[1]]
    y = match(z, alpha, nomatch = -1) - 1
    if (any(y == -1)) stop(".RC.decode: Input string is not in Base64 format")
    if (any(y == 64)) y = y[y != 64]
    neByte = length(y)
    nBlock = ceiling(neByte/4)
    ndByte = 3 * nBlock
    if (neByte < 4 * nBlock) y[(neByte + 1):(4 * nBlock)] = 0
    dim(y) = c(4, nBlock)
    x = matrix(as.integer(0), 3, nBlock)
    x[1, ] = bitOr(bitShiftL(y[1, ], 2), bitShiftR(y[2, ], 4))
    x[2, ] = bitOr(bitShiftL(y[2, ], 4), bitShiftR(y[3, ], 2))
    x[3, ] = bitOr(bitShiftL(y[3, ], 6), y[4, ])
    x = bitAnd(x, 255)
    if (neByte%%4 == 2) x = x[1:(ndByte - 2)]
    if (neByte%%4 == 3) x = x[1:(ndByte - 1)]
    r = as.raw(x)
    n = length(r)
    size = n
    if (n%%size) {
      print(n)
      stop(".RC.decode: number of elements in 'r' is not multiple of 'size'")
    }
    x = readBin(r, "character", n = n%/%4, size = NA, signed = T, endian = .Platform$endian)
    x = paste(x, collapse = "")
    return(x)
}

.RC.typeofaccess <-
function(typeofaccess="public") {
  #are other people allowed to access your computation at freestatistics.org?
  if (typeofaccess == "public")     typeofaccesscode = 1 #anybody can access my computation
  if (typeofaccess == "moratorium") typeofaccesscode = 2 #computation is private until a certain date (a valid account is required!)
  if (typeofaccess == "private")    typeofaccesscode = 3 #nobody else can access my computation (a valid account is required!)
  #Note: 'dateuntil' or 'moratorium' date (YYYY-MM-DD) - only needed if 'Moratorium' is selected in 'Type of Access'
  return(typeofaccesscode)
}

.RC.post <-
function (host, path, data.to.send, referer = "", port = 80, cookie="") {
    if (missing(path)) path <- "/"
    ua <- "User-Agent:" 
    ua <- paste(ua,R.Version()[["version.string"]],sep=" ")
    ua <- paste(ua,R.Version()[["platform"]],sep=" ")
    ua <- paste(ua,Sys.info()[["sysname"]],sep=" ")
    ua <- paste(ua,Sys.info()[["release"]],sep="/")
    ua <- paste(ua,"\n",sep="")
    if (missing(data.to.send)) stop("No data to send provided")
    if (!inherits(data.to.send, "list")) stop("Data to send have to be a list")
    dc <- 0
    xx <- as.integer(runif(10, min = 1, max = 10))
    bo <- paste(xx, collapse = "")
    bo <- paste("xxx", bo, sep = "")
    bol <- "--"
    header <- NULL
    header <- c(header, paste("POST ", path, " HTTP/1.1\n", sep = ""))
    header <- c(header, paste("Host: ", host, "\n", sep = ""))
    if (cookie != "") header <- c(header, paste("Cookie: name=", cookie, "\n", sep = ""))
    header <- c(header, "Connection: close\n")
    header <- c(header, paste("Referer: ", referer, "\n", sep = ""))
    header <- c(header, ua)
    header <- c(header, "Accept: */*\n")
    header <- c(header, paste("Content-type: multipart/form-data; boundary=", bo, "\n", sep = ""))
    mcontent <- NULL
    for (x in 1:length(data.to.send)) {
        val <- data.to.send[[x]]
        key <- names(data.to.send)[x]
        if (typeof(val) == "list") {
            ds <- c(charToRaw(sprintf("%s%s\nContent-Disposition: form-data; name=\"%s\"; filename=\"%s\"\nContent-type: application/octet-stream\n\n", 
                bol, bo, key, val$filename)), val$object, as.raw(10))
        }
        else {
            spcv <- paste(bol,bo,"\nContent-Disposition: form-data; name=\"",key,"\"\n\n",val,"\n",sep="")
            ds <- charToRaw(spcv)
        }
        dc <- dc + length(ds)
        mcontent <- c(mcontent, ds)
    }
    dc <- dc + length(strsplit(bo, "")[[1]]) + length(strsplit(bol,"")[[1]]) + 4
    header <- c(header, paste("Content-length: ", dc, "\n\n",sep = ""))
    mypost <- c(charToRaw(paste(header, collapse = "")), mcontent,charToRaw(paste(bol, bo, "--\n\n", sep = "")))
    rm(header, mcontent)
    #this is where we do the actual http connection
    scon <- socketConnection(host = host, port = port, open = "a+b",blocking = TRUE)
    writeBin(mypost, scon, size = 1)
    output <- character(0)
    options(warn = -1)
    repeat {
        ss <- rawToChar(readBin(scon, "raw", 2048))
        output <- paste(output, ss, sep = "")
        if (regexpr("\r\n0\r\n\r\n", ss) > -1) 
            break()
        if (ss == "") 
            break()
    }
    options(warn = 0)
    close(scon)
    #cat(print(output))
    #RC.debug.output <<- output
    return(output)
}

.RC.is.secure <-
function(code) {
  s <- array(c('.Fortran', '.External', '.External.graphics', '.Call.graphics', '.Call', 'edit', 'edit.data.frame', 'vi', 'emacs', 'pico', 'xemacs', 'xedit', 'de', 'data.entry', 'dataentry', 'postscript', 'pdf', 'pictex', 'xfig', 'x11', 'X11', 'png', 'jpeg', 'quartz', 'graphics.off', 'Gnome', '.Tcl', '.Tcl.args', '.Tcl.callback', '.Tk.ID', '.Tk.newwin', '.Tk.subwin', '.Tkwin', '.Tkroot', 'tkpager', 'tkStartGUI', 'gtk', 'GTK', 'GTK.GNOME', 'screen', 'split.screen', 'erase.screen', 'cloae.screen', 'dev2bitmap', 'dev.cur', 'dev.list', 'dev.next', 'dev.prev', 'dev.print', 'dev.set', 'dev.control', 'dev.copy2eps', 'fifo', 'pipe', 'system', 'system.file', 'Sys.info', 'Sys.getenv', 'Sys.putenv', 'Sys.getlocale', 'Sys.putlocale', 'Sys.source', 'sys.source', 'sys.parent', 'Sys.sleep', 'Sys.time', 'dyn.load', 'library.dynam', 'lookup.xport', 'cat', 'read.dta', 'write.dta', 'data.restore', 'read.epiinfo', 'read.mtp', 'read.spss', 'read.ssd', 'read.xport', 'readLines', 'readline', 'readBin', 'writeBin', '.saveRDS', '.readRDS', 'read.00Index', 'read.ftable', 'write.ftable', 'read.fwf', 'unz', 'zip.file.extract', 'gzcon', 'dput', 'dget', 'capture.output', 'dump', 'setwd', 'getwd', 'prompt', 'promptData', 'save', 'tempfile', 'seek', 'source', 'scan', 'dir', 'dir.create', 'url', 'url.show', 'do.call', 'file', 'file.access', 'file.append', 'file.choose', 'file.copy', 'file.create', 'file.exists', 'file.info', 'file.path', 'file.remove', 'file.rename', 'file.symlink', 'file.show', 'list.files', 'unlink', 'basename', 'dirname', 'write', 'writeLines', 'write.table', 'read', 'path.expand', 'sink', 'sink.number', 'open', 'close', 'download.file', 'gzfile', 'bzfile', 'stdin', 'stdout', 'stderr', 'textConnection', 'showConnection', 'getConnection', 'closeAllConnection', 'socketConnection', 'make.socket', 'read.socket', 'print.socket', 'write.socket', 'savehistory', 'loadhistory', '.Script', 'browseURL', 'locator', 'Rprof', 'remove.packages', 'make.packages.html', 'R.home', 'R.version', 'fix', 'menu', 'example'))
  code <- gsub("^.*source\\('http://www\\.wessa\\.net/cretabc')","",code)
  code <- gsub("^.*source\\(\"http://www\\.wessa\\.net/cretabc\")","",code)
  #code <- gsub("#dev.off()","",code)
  code <- gsub("#postscript","",code)
  code <- gsub("#table.save","",code)
  code <- gsub("#system","",code)

  for (i in 1:length(s)) {
    if (length(grep(glob2rx(paste("*",s[i],"(*)*",sep="")),code))>0) {
      return(paste("Are you sure you want to execute the command '",s[i],"'? If you are certain that it is ok to execute the R code you should use RC.reproduce(...,secure=FALSE).",sep=""))
    }
  }
  return(TRUE)
}

.RC.nstrstr <-
function(haystack,needle) {
  lenHaystack <- nchar(haystack)
  lenNeedle   <- nchar(needle)
  if (lenHaystack < lenNeedle)
    return (0)
  if (lenHaystack == lenNeedle)
    return(haystack==needle)
  lenDiff <- lenHaystack-lenNeedle
  for (i in (1:lenDiff))
    if (needle==substr(haystack,i,i+lenNeedle-1))
      return(i)

  return (0)
}

.RC.strstr <-
function(haystack,needle) {
  strIndex <- .RC.nstrstr(haystack,needle)
  if (strIndex==0)
    return ("")
  return (substr(haystack,strIndex,nchar(haystack)))
}

.RC.title <-
function() {
x <- "
.---.                           .-.             _ .-.   .-.        
: .; :                          : :            :_;: :   : :        
:   .' .--. .---. .--.  .--.  .-' :.-..-. .--. .-.: `-. : :   .--. 
: :.`.' '_.': .; `: ..'' .; :' .; :: :; :'  ..': :' .; :: :_ ' '_.'
:_;:_;`.__.': ._.':_;  `.__.'`.__.'`.__.'`.__.':_;`.__.'`.__;`.__.'
            : :                                                    
            :_;                                                    
 .--.                             .-.  _             
: .--'                           .' `.:_;            
: :    .--. ,-.,-.,-..---. .-..-.`. .'.-.,-.,-. .--. 
: :__ ' .; :: ,. ,. :: .; `: :; : : : : :: ,. :' .; :
`.__.'`.__.':_;:_;:_;: ._.'`.__.' :_; :_;:_;:_;`._. ;
                     : :                        .-. :
                     :_;                        `._.'
" #fuzzy #created by http://www.network-science.de/ascii/
  return(x)
}

.RC.print.demo.text <-
function(text="") {
    pets <- "\n\nPress Enter to continue\n"
    myline <- ""
    RC.mystep <<- RC.mystep + 1
    mytitle <- paste("Step ",RC.mystep,sep="")
    nc <- nchar(mytitle)
    for (i in 1:nc) myline <- paste(myline,"=",sep="")
    text <- paste("\n        ",myline,"\n        ",mytitle,"\n        ",myline,"\n","\n        ",text,sep="")
    cat(text)
    readline(pets)
}

.RC.print.demo.command <-
function(command="",exec=TRUE) {
    pcommand <- paste("> ",command,"
",sep="")
    cat(pcommand)
    if (exec==TRUE) RC.texteval(command)
}

.RC.shorten.url <-
function(url="") {
  if (nchar(url)>0) {
    url <- sub("index\\.php.v=","",url)
    return(url)
  }
}

.RCterms <-
"\n-> Your use of the software is AT YOUR OWN RISK. More info: RC.disclaimer().\n-> This version is a release candidate.\n-> Use RC.demo() to start the demonstration.\n"
.RCdisclaimer <-
"\n###########################################################################\n# GENERAL DISCLAIMER\n# The code provided in this package is provided AS IS without warranty of \n# any kind, either express or implied, including, without limitation, \n# warranties of merchantability, fitness for a particular purpose, and \n# noninfringement. The author uses reasonable efforts to include accurate \n# algorithms and periodically updates the software without notice. However, \n# the author makes no warranties or representations as to the accuracy or \n# completeness of such software, and assumes no liability or responsibility\n# for errors, or software bugs. Your use of this software is AT YOUR OWN \n# RISK. Under no circumstances and under no legal theory shall the author \n# be liable to you or any other person for any direct, indirect, special, \n# incidental, exemplary, or consequential damages arising from your access \n# to, or use of, this software.   \n#\n# SPECIAL WARNING\n# WARNING: This software uses your internet connection to retrieve and store \n# data about statistical computations. Some types of personal information \n# may be transmitted and archived in our online repository. In addition, it \n# is possible that the R code which is reproduced contains code that affects, \n# changes, or replaces objects in your R session or on your hard drive. \n# USE AT YOUR OWN RISK.\n#\n# GPL NOTICE\n# The GPL license only applies to that part of the RC package which is \n# written in R and installed on the local machine of the user. The server-\n# side software is NOT included under the terms of the GPL. Please, contact\n# the author for more information.\n#\n###########################################################################\n"
.RCversion <-
"1.0.2-13-beta"
.RCbaseversion <-
"1.0.2"
.RChomeurl <-
"www.wessa.net"
.RCrepurl <-
"www.freestatistics.org"
.RC.parent <-
function(myurl="") {
  mydum <- strsplit(myurl,"/")
  nc <- nchar(mydum[[1]][9])
  return(substr(mydum[[1]][9],1,nc-4))
}

