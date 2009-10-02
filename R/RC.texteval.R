RC.texteval <-
function (sourceText, collapse = NULL, echo = TRUE) {
    sourceConn <- textConnection(sourceText, open = "r")
    on.exit(close(sourceConn))
    result <- RC.capture(source(file = sourceConn, local = FALSE, echo = echo, print.eval = TRUE), collapse = collapse)
    on.exit(NULL)
    close(sourceConn)
    res <- ""
    for(i in 1:length(result)) {
      if (result[i]!="") res <- paste(res,result[i],"\n",sep="")
    }
    return(res)
}

