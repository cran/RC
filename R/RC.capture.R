RC.capture <-
function (expression, collapse = NULL) {
    resultConn <- textConnection("RC.resultText", open = "w", local=TRUE)
    sink(resultConn)
    on.exit(function() {
        sink()
        close(resultConn)
    })
    expression
    on.exit(NULL)
    sink()
    close(resultConn)
    return(paste(c(RC.resultText, ""), collapse = collapse, sep = ""))
}

