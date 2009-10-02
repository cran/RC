\name{RC.traffic.user}
\Rdversion{1.1}
\alias{RC.traffic.user}
\title{
Fetch internet traffic from all computations of a specific user
}
\description{
This function fetches statistics about the internet traffic of all computations that have been blogged by a user.
}
\usage{
RC.traffic.user(id = "", echo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
user id
}
  \item{echo}{
echo to console?
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A list of IP addresses. For each IP address the following sublists are available:
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{traffic}{IP address, count, and primary key}
  \item{statistics}{sum and mean}
%% ...
}
\examples{
#traffic <- RC.traffic.user(id="Philippe Versluys")
#what is the 'impact' of the user?
#sum(traffic$statistics$sum)
#length(traffic$statistics$sum)
#maybe we should discount the IP addresses from search engines?
#traffic$stat[traffic$stat$mean>1,]

}
\keyword{ utilities }

