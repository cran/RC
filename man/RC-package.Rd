\name{RC-package}
%%\Rdversion{1.1}
\alias{RC-package}
\alias{RC}
\docType{package}
\title{
Reproducible Computing package
}
\description{
The RC package allows the user to:
- submit code snippets to the online repository at FreeStatistics.org
- allow users to reproduce your computation through the internet (even if R is not installed on the client machine)
- retrieve the meta information about an archived computation
- reproduce a computation within the R console (on the local machine)
- reuse the code, data, and parameters of a computation
- query the computational repository for the purpose of educational quality control and research
Have a look at the examples in the following functions: RC.ls, RC.tree, RC.blog, RC.reproduce, RC.reuse, RC.traffic, RC.traffic.user, RC.report.user, ... The RC.demo() function demonstrates various functions in a step-wise manner.
}
\details{
\tabular{ll}{
Package: \tab RC\cr
Type: \tab Package\cr
Version: \tab see RC.version()\cr
Date: \tab 2011-03-20\cr
License: \tab GPL v2 (only applies to client software)\cr
LazyLoad: \tab yes\cr
}
To start a tutorial use RC.demo(). More information can be found at 
http://www.wessa.net/ReproducibleComputing/
}
\author{
Patrick Wessa

Maintainer: send questions/complaints to <patrick@wessa.net> or use the RC.send.feedback() function.

}
\references{
Wessa P., A framework for statistical software development, maintenance, and publishing within an open-access business model, Computational Statistics, 2009, (DOI 10.1007/s00180-008-0107-y)

Wessa, P., Reproducible Computing: a new Technology for Statistics Education and Educational Research, IAENG Transactions on Engineering Technologies, American Institute of Physics, Eds: Ao, Sio-Iong , 2009

Wessa P., Exploring Social Networks in Reproducible Computing and Collaborative Assignments, Proceedings of the International Conference on E-Learning 2009, Toronto, Canada

}
\keyword{ package }

