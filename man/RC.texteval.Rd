\name{RC.texteval}
\alias{RC.texteval}
\title{
Evaluate text in the R interpreter
}
\description{
With this function, any properly formatted text can be evaluated in the R interpreter. The result of the computation is returned.
}
\usage{
RC.texteval(sourceText, collapse = NULL, echo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sourceText}{
R code to be evaluated
}
  \item{collapse}{
separator for results
}
  \item{echo}{
echo in console?
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
The output of the computation is returned (as text). Any objects that are created/changed by the computation are available after the function has been executed.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\section{Warning }{There is no security check - use at your own risk.}
%%\examples{
%%}
\keyword{ internal }

