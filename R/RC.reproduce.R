RC.reproduce <-
function(url="",echo=FALSE,secure=TRUE) {
  url <- .RC.shorten.url(url)
  RCmymd <- RC.meta.data(url)
  RC.parent <<- .RC.parent(url)
  myprepinput <- RC.prepare.input(RCmymd$rawinput)
  mysec <- .RC.is.secure(myprepinput)
  if ((mysec==TRUE) | (secure==FALSE)) {
    RC.texteval(sourceText=myprepinput,echo=echo)
  } else {
    stop(mysec)
  }
}

