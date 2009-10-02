\name{RC.ls}
\Rdversion{1.1}
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
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\examples{
#r <- RC.ls(keyword="AS2009")
#r
}
\keyword{ utilities }

