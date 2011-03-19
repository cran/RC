\name{RC.traffic}
\alias{RC.traffic}
\title{
Fetch internet traffic about a computation
}
\description{
This function fetches the internet traffic of any computation in the repository. In order to identify the computation, a primary key (pk) must be provided. The pk can be obtained through RC.pk() which translates the URL into the corresponding primary key.
}
\usage{
RC.traffic(pk = "", echo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pk}{
the primary key of the computation
}
  \item{echo}{
print intermediary results?
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{ip}{IP addresses}
  \item{count}{# of visits}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{RC.traffic.user}}
}
\examples{
#[not run]
#r <- RC.ls(keyword="AS2009")
#pk <- RC.pk(r$url[2])
#(traffic <- RC.traffic(pk=pk))
#result at the time of writing:
#Number of valid cases found: 5.
#              ip count
#1 84.192.102.135     9
#2  66.249.71.227     9
#3   72.30.81.186     9
#4  66.249.71.101     5
#5  84.194.80.219     5

#[not run]
#now an example with many IP addresses
#r <- RC.ls(keyword="exercise")
#tree <- RC.tree(r$url[50])
#pk <- RC.pk(tree$url[1])
#(traffic <- RC.traffic(pk=pk))
#output not shown
}
\keyword{ utilities }

