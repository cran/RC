\name{RC.network}
\Rdversion{1.1}
\alias{RC.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create network (sociomatrix)
}
\description{
This function creates the sociomatrix of parent-child relationships about all computations that contain a specific keyword. The object that is returned can be used to display a sociogram (requires the 'igraph' package).
}
\usage{
RC.network(keyword = "workshop", elaborate = TRUE, computations = 1, 
	quote = TRUE)
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
Imposes a filter: only the relationships between pairs of users are displayed for which the number of reproduced computations of user A by user B is larger (or equal) than <computations>.
}
  \item{quote}{
use a keyword search with quotes?
}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%\code{\link{'igraph'}}
See also the 'igraph' package.
}
\examples{
#Note: the RC.plot.network function requires that the 'igraph' package is 
#installed on your computer
#r <- RC.network(keyword="exercise")
#g <- RC.plot.network(r,colors=c("red","blue"),weights=as.numeric(r$edges$V3))
}
\keyword{ graphs }

