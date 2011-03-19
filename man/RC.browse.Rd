\name{RC.browse}
\alias{RC.browse}
\title{
View computation in a web browser
}
\description{
Opens a web browser and fetches the web page that contains the snapshot of the statistical computation. The web page is openen through a secure callback mechanism which ensures that the content is shown within the www.wessa.net website. This effectively avoids any browser (such as IE 8/9) from (falsely) complaining about cross-site scripting when the statistical computation is reproduced.
}
\usage{
RC.browse(url)
}
\arguments{
  \item{url}{
The URL of the statistical computation.
}
}
\details{
To use this function you need to specify a valid URL that identifies a statistical computation. The easiest way to obtain such an URL is through the RC.ls() function (see example).
}
\examples{
#search for all computations with the 'AS2009' keyword
#r <- RC.ls(keyword="AS2009")
#view the second computation from the list
#RC.browse(r$url[2])
#a browser window will be opened which displays the snapshot of the 
#statistical computation
}
\keyword{ misc }

