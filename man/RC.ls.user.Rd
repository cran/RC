\name{RC.ls.user}
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
\value{
The function returns a list with the following items:
  \item{url}{url of the webpage that contains the meta data of the computation}
  \item{key}{the key of the computation (this is NOT the primary key but the URL key)}
  \item{folder}{folder of the computation}
  \item{date}{date of the computation (as registered on the server)}
  \item{module}{the name of the R module that generated the computation}
  \item{title}{user-specified title}
  \item{keyword}{user-specified keywords}
  \item{course}{id of the course or project (this is only available if the 'protag' label was used in the hyperlink that directs to the R module)}
  \item{user}{user id (owner of the computation)}
  \item{parent}{URL key of the parent compuation (if any)}
  \item{message}{boolean variable which indicates whether the computation contains forum messages}
}
\seealso{
\code{\link{RC.ls}}
}
\examples{
#fetch the list
#r <- RC.ls.user(id="UseR")
#print some useful info
#table(r$module)
}
\keyword{ utilities }

