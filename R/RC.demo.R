RC.demo <-
function() {
demo.r <- ""
spaces <- "        "
while (demo.r != "q") {

  cat("

=-=-=-=-=-=-=-=-=
: Quick RC demo :
=-=-=-=-=-=-=-=-=

sh. say 'hello' to the network
fc. find a computation

rc. 'reproduce' a computation
sg. security guidelines
bc. blog a computation
ec. 'reuse' a computation

wt. query web traffic
ur. user report

ff. fetch feedback about a computation
fu. fetch feedback submitted by user

tr. tree of p-c relationships
sn. social network of p-c relationships
si. sociogram for individual
sg. sociogram for group

pp. pitfalls/problems

q. quit demo

")
  RC.mystep <<- 0
  demo.r <- readline("Which feature should be demonstrated? ")
  if (demo.r == "sh") {
    .RC.print.demo.text("The RC package cannot function without internet connection. The package communicates with several web servers and therefore it is useful to be able to test the connectivity and the computing performance of the network of servers. The RC.hello() function sends a test request to the main web server which initiates a connectivity and performance test on the network. The result is sent back to your computer. If the reported statistics are all 'Excellent' and if all the response times are low (below one second) then any latency that you might have experienced are due to 'local' problems (e.g. connection to your ISP). Let us check the network now...")
    cat("> RC.hello()\n")
    RC.hello()
  }
  if (demo.r == "ff") {
    .RC.print.demo.text("The RC.feedback.computation() function allows you to fetch all feedback messages about a particular computation. The computation must be identified by its primary key (pk).")
    cat("> (RC.demo.r <- RC.feedback.computation(pk=22149))\n")
    print(RC.demo.r <- RC.feedback.computation(pk=22149))
    .RC.print.demo.text("Observe how several people have posted feedback about this particular computation. Note that the reviewer has the option to submit the feedback anonymously.")
  }
  if (demo.r == "fu") {
    .RC.print.demo.text("The RC.feedback.submitted() function allows you to fetch all feedback messages that were submitted by a particular user. The user must be identified by the User ID that belongs to the account at www.freestatistics.org.")
    cat("> (RC.demo.r <- RC.feedback.submitted(id='b-r0262549'))\n")
    print(RC.demo.r <- RC.feedback.submitted(id='b-r0262549'))
    .RC.print.demo.text("Observe how this particular user opted to submit his/her feedback messages anonymously.")
  }
  if (demo.r == "bc") {
    .RC.print.demo.text("Suppose we want to blog the following code snippet: \n\n\t\tx <- data.frame(array(rnorm(100),dim=c(50,2)))\n\t\tcolnames(x) <- c('X1','X2')\n\n\t\tplot(x$X1,x$X2,main='my title')\n\t\tres <- cor.test(x$X1,x$X2)\n\t\tres\n\n\tHow do we prepare this code snippet before we can submit it to the FreeStatistics.org repository?")
    .RC.print.demo.text("First we need to define the data as a data frame which is called RCx:\n\n\t\tRCx <- data.frame(array(rnorm(100),dim=c(50,2)))\n\t\tRCxnames <- c('X1','X2')\n\n\t\tplot(RCx$X1,x$X2,main='my title')\n\t\tres <- cor.test(RCx$X1,RCx$X2)\n\t\tres\n\n")
    .RC.print.demo.text("Now we create a wrapper function which contains the remaining code of the snippet:\n\n\t\tRCx <- data.frame(array(rnorm(100),dim=c(50,2)))\n\t\tRCxnames <- c('X1','X2')\n\n\t\tRC.sample.fun <- function() {\n\t\t\tplot(RCx$X1,RCx$X2,main='my title')\n\t\t\tres <- cor.test(RCx$X1,RCx$X2)\n\t\t\tprint(res)\n\t\t}\n\n\tNote that we need to output the res object with the function print(res) because it is now contained in the wrapper function.")
    .RC.print.demo.text("It is always a good idea to define parameters in the wrapper function. The maximum number of parameters is 20. Note: a default value (even if it is 0 or '') is required for each parameter.\n\n\t\tRCx <- data.frame(array(rnorm(100),dim=c(50,2)))\n\t\tRCxnames <- c('X1','X2')\n\n\t\tRC.sample.fun <- function(title='my title') {\n\t\t\tplot(RCx$X1,RCx$X2,main=title)\n\t\t\tres <- cor.test(RCx$X1,RCx$X2)\n\t\t\tprint(res)\n\t\t}\n\n\tObserve how main=title in the plot() function. The title is now user-specified (input of the wrapper function) and becomes an input field of the web interface whenever the blogged computation is reproduced inside a web browser.")
    .RC.print.demo.text("Finally, we need to identify a start and end for each plot in the body of the wrapper function. The code now becomes:\n\n\t\tRCx <- data.frame(array(rnorm(100),dim=c(50,2)))\n\t\tRCxnames <- c('X1','X2')\n\n\t\tRC.sample.fun <- function(title='my title') {\n\t\t\tRC.start.plot\n\t\t\tplot(RCx$X1,RCx$X2,main=title)\n\t\t\tRC.end.plot\n\t\t\tres <- cor.test(RCx$X1,RCx$X2)\n\t\t\tprint(res)\n\t\t}\n\n\tIn order to check if everything is ok, we execute the wrapper function (it should run without any errors)...")
    .RC.print.demo.command("RCx <- data.frame(array(rnorm(100),dim=c(50,2)))")
    .RC.print.demo.command("RCxnames <- c('X1','X2')")
    .RC.print.demo.command("RC.sample.fun <- function(title='my title') { RC.start.plot; plot(RCx$X1,RCx$X2,main=title); RC.end.plot; res <- cor.test(RCx$X1,RCx$X2); print(res); }")
    .RC.print.demo.command("RC.sample.fun()")
    .RC.print.demo.text("Now that we are satisfied that everything works, we are ready to submit the R code (and data) to the FreeStatistics.org repository...")
    .RC.print.demo.command("RC.demo.r <- RC.blog(title='correlation test', keywords='blogtest', comments='This example is used in the manual files of the RC package.', uid='UseR', pwd='UseR', typeofaccess='public', rcode=RC.sample.fun)")
    .RC.print.demo.text("If all went well, the computation has been blogged in the repository. The RC.demo.r object contains the URL that is associated with the computation. The RC.browse() function allows us to have a look at the web page.")
    .RC.print.demo.command("RC.browse(RC.demo.r)")
    .RC.print.demo.text("We can also find the computation through the RC.ls() function.")
    .RC.print.demo.command("RC.demo.res <- RC.ls(keyword='blogtest')")
    .RC.print.demo.command("RC.demo.res[length(RC.demo.res[,1]),]",F)
    print(RC.demo.res[length(RC.demo.res[,1]),])
  }
  if (demo.r == "fc") {
    .RC.print.demo.text("First we list all computations which have the 'AS2009' keyword.")
    .RC.print.demo.command("RC.demo.r <- RC.ls(keyword='AS2009')")
    .RC.print.demo.text("Now we observe the contents of the returned object.")
    .RC.print.demo.command("RC.demo.r",F)
    print(RC.demo.r)
    .RC.print.demo.command("labels(RC.demo.r)",F)
    print(labels(RC.demo.r))
  }
  if (demo.r == "rc") {
    .RC.print.demo.text("First we need to identify the unique URL of the computation that we want to reproduce. Suppose that we 'know' that the computation of interest contains the keyword 'AS2009'. We can use the RC.ls(keyword='AS2009') command to obtain a list of all relevant computations.")
    .RC.print.demo.command("r <- RC.ls(keyword='AS2009')")
    .RC.print.demo.text("Now we select the second computation from the list and use it to reproduce the computation on our local machine.")
    .RC.print.demo.command("RC.demo.res <- RC.reproduce(r$url[2])")
    .RC.print.demo.text("Warning: observe how the reproduced computation has changed the r object (this object is used in the archived R code). It now contains the contents of a spectral analysis. This can be demonstrated by executing plot(r).")
    .RC.print.demo.command("plot(r)")
    .RC.print.demo.text("To understand why this happens, we have a look at the archived R code that is executed. We do this through the 'rawinput' object which is returned by the RC.meta.data() function.")
    .RC.print.demo.command("RC.demo.mycode <- RC.meta.data(RC.ls(keyword='AS2009')$url[2])$rawinput")
    .RC.print.demo.command("cat(RC.demo.mycode)",F)
    cat(RC.demo.mycode)
  }
  if (demo.r == "ec") {
    .RC.print.demo.text("The difference between RC.reproduce() and RC.reuse() is that the latter allows you to change the computation and blog it easily. The RC.reuse() function creates a wrapper function RC.fun() and stores the data in the object RCx.")
    .RC.print.demo.text("First we fetch the URL of the computation that we wish to reuse. Suppose that we 'know' that the computation of interest contains the keyword 'AS2009'. We can use the RC.ls(keyword='AS2009') command to obtain a list of all relevant computations.")
    .RC.print.demo.command("RC.demo.r <- RC.ls(keyword='AS2009')")
    .RC.print.demo.text("Now we select the second computation from the list and use it to invoke the RC.reuse() function.")
    .RC.print.demo.command("RC.demo.res <- RC.reuse(RC.demo.r$url[2])")
    .RC.print.demo.text("Observe how the computation has been reproduced (you should see a picture of the cumulative periodogram of a time series. More importantly however, a so-called wrapper function (RC.fun) was created and contains the R code of the computation. Let us have a look at the upper part of the function...")
    .RC.print.demo.command("head(RC.fun)",F)
    print(head(RC.fun))
    .RC.print.demo.text("There are 4 parameters (par1, par2, par3, and par4) which are all numeric and define how the time series is transformed/differenced before the analysis is executed. Let us reuse the R code by comparing four different combinations of parameters.")
    .RC.print.demo.command("op <- par(mfrow=c(4,2)); RC.fun('1','0','0'); RC.fun('1','1','0'); RC.fun('1','0','1'); RC.fun('0','1','1'); par(op)")
    .RC.print.demo.text("Another way to 'reuse' (rather than 'reproduce') the computation is to change the data series which is contained in RCx. Suppose we want to apply the computation to four chronological periods of the time series. The following computation calls the RC.fun() wrapper function inside a loop which defines the periods of the time series.")
    .RC.print.demo.command("RCx.back <- RCx; op <- par(mfrow=c(4,2)); n <- length(RCx)/4; for(i in 1:4) { RCx <- RCx.back[((i-1)*n+1):((i-1)*n+n)]; RC.fun();}; par(op); RCx <-RCx.back")
    .RC.print.demo.text("Of course it is also possible to change the R code which is contained in the RC.fun() wrapper function. The obvious advantage is that the changed wrapper function can be readily used to blog the changed computation. Let us change RC.fun() and replace the 'cpgram' function with the 'acf' function (feel free to change the associated text also)...")
    .RC.print.demo.command("RC.fun <- edit(RC.fun)")
    .RC.print.demo.text("Now the function has been changed we can observe the result of RC.fun()...")
    .RC.print.demo.command("RC.fun()")
  }
  if (demo.r == "tr") {
    .RC.print.demo.text("First we need the URL of a compuation. Suppose that we are interested to see the parent-child relationships about a exercise which is related to the so-called 'babies' problem.")
    .RC.print.demo.command("RC.demo.r <- RC.ls(keyword='babies')")
    .RC.print.demo.text("We try the first computation to build a tree.")
    .RC.print.demo.command("RC.demo.mytree.1 <- RC.tree(RC.demo.r$url[1])")
    .RC.print.demo.text("Now we print the tree.")
    .RC.print.demo.command("RC.print.tree(RC.demo.mytree.1)",F)
    RC.print.tree(RC.demo.mytree.1)
    .RC.print.demo.text("A tree always contains ALL children of the 'current' computation (this is the computation with the ***). In addition, all single parents are displayed up to the root. The brothers/sisters (and other relatives) are not shown. Therefore, we take the root parent from the current tree and use the corresponding URL to create a second tree. This will generate a tree with all children of the root parent.")
    .RC.print.demo.command("RC.demo.mytree.2 <- RC.tree(RC.demo.mytree.1$url[1])")
    .RC.print.demo.text("Print the entire tree... (generates a lot of output).")
    .RC.print.demo.command("RC.print.tree(RC.demo.mytree.2)",F)
    RC.print.tree(RC.demo.mytree.2)
  }
  if (demo.r == "wt") {
    .RC.print.demo.text("First we determine the URL of a computation of interest.")
    .RC.print.demo.command("RC.demo.r <- RC.ls(keyword='AS2009')")
    .RC.print.demo.text("Now we fetch the web statistics about the first computation in the list. In order to do this we need the primary key (pk) of the compuation.")
    .RC.print.demo.command("RC.demo.pk <- RC.pk(RC.demo.r$url[2])")
    .RC.print.demo.text("The primary key can be used in the RC.traffic() function.")
    .RC.print.demo.command("RC.demo.traffic <- RC.traffic(pk=RC.demo.pk)")
    .RC.print.demo.text("The traffic statistics show all unique IP addresses that have visited the computational snapshot in the FreeStatistics.org repository.")
    .RC.print.demo.command("RC.demo.traffic",F)
    print(RC.demo.traffic)
  }
  if (demo.r == "sn") {
    .RC.print.demo.text("First we determine the URLs of all computations that contain a specific keyword (which is associated with an assignment).")
    .RC.print.demo.command("RC.demo.r <- RC.network(keyword='exercise')")
    .RC.print.demo.text("The r object contains all parent-child relationships. The 'edges' are defined by the UserIDs and the 'vertices' are sorted by impact which is measured as the total number of computations that are reproduced by peers. The RC.plot.network() function is used to display the sociogram that corresponds to the sociomatrix which is contained in the object RC.demo.r. NOTE: this will only work if the package 'igraph' is installed.")
    .RC.print.demo.command("RC.demo.g <- RC.plot.network(RC.demo.r,colors=c('red','blue'),weights=as.numeric(RC.demo.r$edges$V3))")
  }
  if (demo.r == "si") {
    .RC.print.demo.text("We can fetch the 'inbound' relationships for any user by simply defining the User ID as keyword.")
    .RC.print.demo.command("RC.demo.r <- RC.network(keyword='b-s0800184')")
    .RC.print.demo.text("The r object contains the network of all people from whom the user reproduced computations (= inbound relationships). NOTE: this will only work if the package 'igraph' is installed.")
    .RC.print.demo.command("RC.demo.g <- RC.plot.network(RC.demo.r)")
  }
  if (demo.r == "sg") {
    .RC.print.demo.text("First we create a vector (of type 'character') with all the User IDs of the users for which the sociogram is to be computed.")
    .RC.print.demo.command("RC.demo.users <- c('b-s0800252', 'b-s0800298', 'b-s0800944', 'b-s0801290', 'b-s0800032', 'b-s0510133', 'b-s0800417', 'b-s0801466', 'b-s0801156', 'b-s0800969', 'b-s0801215', 'b-s0700554', 'b-s0800458')")
    .RC.print.demo.text("Now we fetch the 'inbound' networks (for all users) and create the union.")
    .RC.print.demo.command("RC.demo.r <- RC.network.list(RC.demo.users)")
    .RC.print.demo.text("The sociogram is now the 'union' of all 'inbound' networks of the specified universe.")
    .RC.print.demo.command("RC.demo.g <- RC.plot.network(RC.demo.r)")
  }
  if (demo.r == "sg") {
    .RC.print.demo.text("Reproducing computations in your R console may be risky because the RC.reproduce() function will fetch the archived R code and execute it on your machine. This R code that is reproduced may create/change any objects in your R session, execute system commands, access your hard disk, and connect to the internet. Therefore the RC package has been designed with a built-in black list of so-called dangerous commands. The RC package checks the R code against the black list before it is executed (secure = TRUE). Let us look at a simple example to illustrate this feature.")
    .RC.print.demo.command("RC.demo.r <- RC.ls(keyword='bagplot')")
    .RC.print.demo.text("The previous command fetches the information about all computations with the 'bagplot' keyword. Let us see what happens when we want to reproduce the first computation in this list.")
    cat("> RC.demo.m <- RC.reproduce(RC.demo.r$url[1])")
    cat("\nError in RC.reproduce(RC.demo.r$url[1]) : 
  Are you sure you want to execute the command 'cat'? If you are certain that it is ok to execute the R code you should use RC.reproduce(...,secure=FALSE).
In addition: Warning messages:
1: NAs introduced by coercion 
2: NAs introduced by coercion 
3: NAs introduced by coercion\n")
    .RC.print.demo.text("An error message is diplayed and the potentially dangerous command is shown. Let us examine the source code of the computation in order to identify if there is any real danger.")
    .RC.print.demo.command("RC.demo.md <- RC.meta.data(RC.demo.r$url[1])")
    .RC.print.demo.command("RC.demo.mycode <- RC.demo.md$rawinput")
    .RC.print.demo.command("cat(RC.demo.mycode)",F)
    #cat(mycode)
    cat("\n[Output is not shown...]\n")
    .RC.print.demo.text("Suppose that we have established that the R source code is not harmful in any way. In this case we can proceed and execute the code with the secure=FALSE setting.")
    .RC.print.demo.command("RC.reproduce(RC.demo.r$url[1],secure=FALSE)")
  }
  if (demo.r == "ur") {
    .RC.print.demo.text("The RC.report.user() function fetches the statistics about the computations that have been blogged by a specific user (with id = 's0801036') and computes frequency tables.")
    .RC.print.demo.command("RC.demo.r <- RC.report.user(id='s0801036')")
    .RC.print.demo.command("RC.demo.r",F)
    print(RC.demo.r)
    .RC.print.demo.text("Now we can do some plots...")
    .RC.print.demo.command("op <- par(mfrow=c(2,1)); plot(RC.demo.r$year.month.day.table); plot(RC.demo.r$hour.table); par(op)")
    .RC.print.demo.text("Compare users...")
    .RC.print.demo.command("RC.demo.r1 <- RC.report.user(id='s0801234')")
    .RC.print.demo.command("op <- par(mfrow=c(2,2)); plot(RC.demo.r$year.month.day.table); plot(RC.demo.r1$year.month.day.table); plot(RC.demo.r$hour.table); plot(RC.demo.r1$hour.table); par(op)")
  }
  if (demo.r == "pp") {
    .RC.print.demo.text("This section illustrates several pitfalls and problems that may occur. The first example involves computations that are not powered by R (as shown below). Some web based software modules are not based on the R language: for instance the software at http://www.wessa.net/quart.wasp is written in PHP and C. This is part of a collection of 'old' modules which haven't been replaced by genuine R modules yet...")
    .RC.print.demo.command("RC.demo.r <- RC.reproduce('http://www.freestatistics.org/blog/date/2009/Aug/18/t1250581528nxc5br6ix0wvv6k.htm/')",F)
    cat("\nError in source(file = sourceConn, local = FALSE, echo = echo, print.eval = TRUE) : \n  5:22: unexpected numeric constant\n4: #Server: Apache/2.2.9 (Fedora)\n5: X-Powered-By: PHP/5.2.6\n                        ^\nIn addition: There were 42 warnings (use warnings() to see them)\n")

  }
  if (demo.r == "q") {
  }
}
}

