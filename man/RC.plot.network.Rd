\name{RC.plot.network}
\alias{RC.plot.network}
\title{
Plot the Sociogram of Impact
}
\description{
This function uses the feautures of the 'igraph' package to plot the Sociogram that is based on the Impact bewteen users. The Impact is computed based on the parent-child relationships that exist between computations.
}
\usage{
RC.plot.network(network = "", colors = "", weights = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{
network created by RC.network()
}
  \item{colors}{
list of color codes
}
  \item{weights}{
weights of edges
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
\references{
Wessa P., Exploring Social Networks in Reproducible Computing and Collaborative Assignments, Proceedings of the International Conference on E-Learning 2009, Toronto, Canada
}
%%\examples{
%%
%%}
\keyword{ graphs }

