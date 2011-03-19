\name{RC.blog}
\alias{RC.blog}
\title{
Blog an R function in the FreeStatistics.org Repository
}
\description{
Use this function to blog any user-defined R function in the Repository (at www.FreeStatistics.org).
}
\usage{
RC.blog(title = "", keywords = "", comments = "", uid = "", pwd = "", 
typeofaccess = "public", moratoriumdate = "", rcode)
}
\arguments{
  \item{title}{
Any user-specified title which briefly describes the computation to be blogged.
}
  \item{keywords}{
A comma separated list of keywords (or key phrases) which can be used in future queries. If you want to be able to retrieve your computation at a later stage in time, you should specify one (or several) unique keywords. For example, the keyword 'AS2009' could be used for computations that are associated with the Reproducible Computing workshop at the Applied Statistics 2009 conference.
}
  \item{comments}{
This is used to specify any ASCII text (of any length) which describes the computation.
}
  \item{uid}{
The User ID of the owner of the computation is required in any one of the following cases:
1. to make sure that the identity of the computation is 'verified' (this is proof that the computation was created by you - or anyone who knows your UserID and Password)
2. to hide the computation from third parties (if typeofaccess='private')
3. to hide the computation until a certain date in the future (if typeofaccess='moratorium')
}
  \item{pwd}{
The password of your account - only required in combination with uid.
}
  \item{typeofaccess}{
A string which identifies how the computation can be accessed by third parties:
1. 'public' (default value which specifies that the computation can be accessed by anyone)
2. 'private' (specifies that the computation can only be viewed by you - or anyone who knows your uid and pwd)
3. 'moratorium' (the computation is 'private' until the 'moratoriumdate')
}
  \item{moratoriumdate}{
Only required if typeofaccess='moratorium'.
}
  \item{rcode}{
The name of the function to be blogged.
}
}
\details{
If you want to use the uid and pwd you must create an account first. To create an account you must do the following:
1. browse the http://www.freestatistics.org/index.php?action=8 web page
2. click the 'Create user' button
3. submit the account creation form (some fields are required)
4. within a period of 24 hours you will receive an e-mail which contains a hyperlink that you must use (in a web browser) to confirm your e-mail address
5. check that your account is working properly (login with your new uid and pwd on the http://www.freestatistics.org/index.php?action=8 page)
}
\value{
Returns the url that uniquely identifies the statistical computation.
}
\examples{
#define RCx data frame
RCx <- data.frame(array(rnorm(100),dim=c(50,2)))
RCxnames <- c("X1","X2")
#define wrapper function
RC.sample.fun <- function(title="my title") {
  RC.start.plot
  plot(RCx$X1,RCx$X2,main=title)
  RC.end.plot
  res <- cor.test(RCx$X1,RCx$X2)
  print(res) #print the result!
}
#blog it [not run]
#r <- RC.blog(title="correlation test", keywords="blogtest", 
#  comments="This example is used in the manual files of the 
#  RC package.", uid="UseR", pwd="UseR", typeofaccess="public", 
#  rcode=RC.sample.fun)
#now we can use the returned URL to fetch the meta data [not run]
#md <- RC.meta.data(r)
#md
}
\keyword{ IO }
\keyword{ connection }
\keyword{ database }
