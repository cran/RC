\name{RC.save.image}
\alias{RC.save.image}
\title{
Save image
}
\description{
Saves an image in the repository. This function is extremely slow and not recommended unless the amount of data to be stored is relatively small.
}
\usage{
RC.save.image(title = "", keywords = "", comments = "", uid = "", 
	pwd = "", typeofaccess = "public", moratoriumdate = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{
Title of your image file
}
  \item{keywords}{
Comma separated list of keywords
}
  \item{comments}{
Any comment (ASCII text)
}
  \item{uid}{
User ID of your account at FreeStatistics.org
}
  \item{pwd}{
Password
}
  \item{typeofaccess}{
A string which identifies how the computation can be accessed by third parties:
1. 'public' (default value which specifies that the computation can be accessed by anyone)
2. 'private' (specifies that the computation can only be viewed by you - or anyone who knows your uid and pwd)
3. 'moratorium' (the computation is 'private' until the 'moratoriumdate')
}
  \item{moratoriumdate}{
Only required if typeofaccess='moratorium'.
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
%%\examples{
%%}
\keyword{ IO }
\keyword{ file }% __ONLY ONE__ keyword per line
