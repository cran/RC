\name{RC.ls.course}
\alias{RC.ls.course}
\title{
Find computations in course
}
\description{
This function retrieve a list of all computations that were generated within the context of a course. The course identification is automatically stored in the repository if the user clicks on a hyperlink with a tag called 'protag' (e.g. http://..../rwasp_myrmodule.waps?utag=...&protag=mycourseid). In Moodle, BlackBoard, and many other Virtual Learning Environments it is possible to specify the course id with any label in the hyperlinks. The use of the 'protag' label is not limited to courses - one may uniquely identify any project, journal, type of publication, etc...
}
\usage{
RC.ls.course(course = "Applied Business Statistics 2008 (SHW)")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{course}{
course id
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
%%\examples{
%%
%%}
\keyword{ utilities }

