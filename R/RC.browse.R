RC.browse <-
function(url="http://www.freestatistics.org/") {
  url <- .RC.shorten.url(url)
  #we use a secure callback mechanism because IE 8 contains a bug in the cross-site scripting filter
  #IE 8 thinks that R is a web scripting language :-)
  browseURL(paste("https://www.wessa.net/RC/rc_callback.wasp?url=",url,sep=""))
}

