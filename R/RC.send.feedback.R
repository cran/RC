RC.send.feedback <-
function(email="",subject="",message="") {
    mydata <- list(email=email,subject=subject,message=message)
    reply <- .RC.post(.RChomeurl, "/RC/postmessagerc.wasp", data.to.send=mydata, referer=.RCversion, port=80)
    cat(reply)
}

