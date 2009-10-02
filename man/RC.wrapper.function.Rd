\name{RC.wrapper.function}
\Rdversion{1.1}
\alias{RC.wrapper.function}
\title{
Fetch wrapper function
}
\description{
Creates the wrapper function RC.fun() which contains the R code that was stored in the repository. The function RC.wrapper.function() is called by RC.reuse().
}
\usage{
RC.wrapper.function(metadata = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata}{
the meta data object which is obtained from RC.meta.data()
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function simply creates the function RC.fun() which contains the R code from the repository. If RC.fun() already exists, it is replaced without warning.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{RC.reuse}}
}
%%\examples{
%%
%%}
\keyword{ utilities }

