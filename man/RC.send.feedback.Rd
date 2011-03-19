\name{RC.send.feedback}
\alias{RC.send.feedback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Send a question/complaint to the author
}
\description{
Use this function if you want to send a complaint or if you want to make a suggestion.
}
\usage{
RC.send.feedback(email = "", subject = "", message = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{email}{
Your e-mail address (optional)
}
  \item{subject}{
A subject line
}
  \item{message}{
Your message in plain ASCII.
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
#RC.send.feedback("my.email@myisp.com","Help!!!", 
#  "My computer crashed and displays a blue screen...")
#Note: you will probably get a reply to install Linux :-)
}
\keyword{ debugging }

