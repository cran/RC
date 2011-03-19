\name{RC.network.list}
\alias{RC.network.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create network (sociomatrix) based on list of bloggers
}
\description{
This function creates the sociomatrix of parent-child relationships about all computations that belong to a user-specified list of bloggers (users who archive statistical computations). The object that is returned can be used to display a sociogram (requires the 'igraph' package).
}
\usage{
RC.network.list(bloggers = NULL, elaborate = TRUE, computations = 1, quote = TRUE, startdate = "1970-01-01", enddate = "4000-12-31", vertex.attributes = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bloggers}{
character list of user identifications of the bloggers
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
  \item{startdate}{
do not use computations that are older than startdate
}
  \item{enddate}{
do not use computations that are newer than enddate
}
  \item{vertex.attributes}{
apply attributes to vertices (for instance 'male' and 'female')
}
}
\value{
Returns edges and verices that can be used in the igraph package to display and analyze social networks.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
\code{\link{RC.network}}
}
\examples{
#[not run]
#RC.demo.users <- c('b-s0800252', 'b-s0800298', 'b-s0800944', 'b-s0801290', 'b-s0800032', 'b-s0510133', 'b-s0800417', 'b-s0801466', 'b-s0801156', 'b-s0800969', 'b-s0801215', 'b-s0700554', 'b-s0800458')
#RC.demo.r <- RC.network.list(RC.demo.users)
#RC.demo.g <- RC.plot.network(RC.demo.r)
#now you may analyze RC.demo.g with the igraph package
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }

