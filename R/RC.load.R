RC.load <-
function(url="",uid="",pwd="") {
  if (url == "") stop("URL is required.")
  load(url(url))
}

