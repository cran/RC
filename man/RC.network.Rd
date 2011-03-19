\name{RC.network}
\alias{RC.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create network (sociomatrix) based on keyword
}
\description{
This function creates the sociomatrix of parent-child relationships about all computations that contain a specific keyword. The object that is returned can be used to display a sociogram (requires the 'igraph' package).
}
\usage{
RC.network(keyword = "workshop", elaborate = TRUE, computations = 1, 
	quote = TRUE, startdate="1970-01-01",enddate="4000-12-31")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keyword}{
keyword to be used in search
}
  \item{elaborate}{
Boolean parameter which indicates if parents should have the same keyword as the children (elaborate=TRUE).
}
  \item{computations}{
Imposes a filter: only the relationships between pairs of users are displayed for which the number of reproduced computations of user A by user B is larger than (or equal to) <computations>.
}
  \item{quote}{
use a keyword search with quotes?
}
  \item{startdate}{do not use computations that are older than startdate}
  \item{enddate}{do not use computations that are newer than enddate}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Returns edges and verices that can be used in the igraph package to display and analyze social networks.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
\code{\link{RC.network.list}}
}
\examples{
#Note: the RC.plot.network function requires that the 'igraph' package is 
#installed on your computer
#r <- RC.network(keyword="exercise")
#g <- RC.plot.network(r,colors=c("red","blue"),weights=as.numeric(r$edges$V3))
}
\keyword{ graphs }

