\name{RC.ls.user}
\Rdversion{1.1}
\alias{RC.ls.user}
\title{
Find computation from a user
}
\description{
This function returns information about all the computations that were blogged by a user.
}
\usage{
RC.ls.user(id = "", echo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
user id
}
  \item{echo}{
should results be echoed to console?
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
#fetch the list
#r <- RC.ls.user(id="UseR")
#print some useful info
#table(r$module)
}
\keyword{ utilities }

