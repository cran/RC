\name{RC.prepare.input}
\alias{RC.prepare.input}
\title{
Prepare Raw Input
}
\description{
This function converts the Raw Input (as archived in the repository) into a code snippet that can be reproduced in the R console. This function is called by RC.reproduce().
}
\usage{
RC.prepare.input(rawinput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawinput}{
rawinput from RC.meta.data()
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
returns pre-processed R code which can be recomputed
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
#[not run]
#r <- RC.ls.user(id='UseR')
#fetching meta data about a computation that was generated in the console
#md <- RC.meta.data(r$url[21])
#prepare input
#p <- RC.prepare.input(md$rawinput)
#now it can be reproduced
#RC.texteval(p)
}
\keyword{ manip }
\keyword{ utilities }
