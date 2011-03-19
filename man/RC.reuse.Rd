\name{RC.reuse}
\alias{RC.reuse}
\title{
Reuse computation from the archive
}
\description{
This function allows you to recompute and reuse a computation from the repository. The data, parameters, and underlying R code can be changed.
}
\usage{
RC.reuse(url = "", secure = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
The unique URL of the computation to be reproduced
}
  \item{secure}{
Check R code against blacklist of potentially dangerous commands
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\examples{
#[not run]
#RC.demo.r <- RC.ls(keyword='AS2009')
#RC.demo.res <- RC.reuse(RC.demo.r$url[2])
#now the RC.fun() function is available and can be executed
#op <- par(mfrow=c(4,2))
#RC.fun('1','0','0')
#RC.fun('1','1','0')
#RC.fun('1','0','1')
#RC.fun('0','1','1')
#par(op)
}
\keyword{ IO }
\keyword{ connection }
\keyword{ database }
