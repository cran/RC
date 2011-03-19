\name{RC.ls}
\alias{RC.ls}
\title{
List computations
}
\description{
This function allows the user to search computations that are associated with a keyword or key phrase.
}
\usage{
RC.ls(uid = "", pwd = "", keyword = "AS2009", quote = TRUE, 
	echo = TRUE, course = "", module = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uid}{
User ID (only required to search for private computations)
}
  \item{pwd}{
Password (only required to search for private compuations)
}
  \item{keyword}{
The keyword or key phrase to search. Use * as a wildcard.
}
  \item{quote}{
Put key phrase between quotes
}
  \item{echo}{
Echo intermediary results to console
}
  \item{course}{
Not yet implemented.
}
  \item{module}{
Not yet implemented
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
\examples{
#[not run]
#r <- RC.ls(keyword="AS2009")
#r
}
\keyword{ utilities }

