\name{RC.meta.data}
\Rdversion{1.1}
\alias{RC.meta.data}
\title{
Fetch meta data of a statistical computation
}
\description{
Use this function to download the meta data that is associated with a stored computation. You need to know the exact URL of the computation or use the URL that is returned by the RC.ls() function (see example).
}
\usage{
RC.meta.data(url = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
url of the computation
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
This function returns a list which contains several types of meta data about the computation. The actual elements in the list depend on several factors. In most cases the following elements will be available:
  \item{type }{Indicates how the computation has been archived (e.g. 'R module', 'R console', ...}
  \item{date }{Date when the computation was archived}
  \item{uid}{User id (owner of the computation)}
  \item{title}{User defined title}
  \item{target}{URL of the R module}
  \item{rawinput}{The R code (in raw form) which is sent to the R server}
  \item{rawoutout}{The reply from the R server (in raw form)}
  \item{output}{Output of the R module in HTML}
  \item{ylimmax}{maximum value on y-axis}
  \item{ylimmin}{minimum value of y-axis}
  \item{chartxlab}{text on x-axis of chart}
  \item{chartylab}{text on y-axis of chart}
  \item{chartheight}{the height of each chart in pixels}
  \item{chartwidth}{the width of each chart in pixels}
  \item{par<number>}{value of parameter <number>}
  \item{parent}{id of the parent computation}
  \item{data}{dataset}
  \item{newformula}{R code as specified by the author or user before it is submitted to the R server}
}
\examples{
#search for computations with the 'AS2009' keyword
#r <- RC.ls(keyword="AS2009")
#fetch the meta data for the second computation in the list
#md <- RC.meta.data(r$url[2])
#do something useful with the data that was used in the computation
#plot(md$data,type="l",main="Data from the fetched computation")
}
\keyword{ attribute }
\keyword{ IO }
\keyword{ database }
