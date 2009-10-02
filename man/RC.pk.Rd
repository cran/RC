\name{RC.pk}
\Rdversion{1.1}
\alias{RC.pk}
\title{
Fetch primary key of a computation
}
\description{
This function fetches the primary key that corresponds to a record in the computational database. Some functions require the primary key as input.
}
\usage{
RC.pk(url = "", echo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
the unique URL of the computation
}
  \item{echo}{
Print result
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{RC.traffic}}, \code{\link{RC.traffic.user}}
}
\examples{
#r <- RC.ls(keyword="AS2009")
#pk <- RC.pk(r$url[2])
}
\keyword{ utilities }

