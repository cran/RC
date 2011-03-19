\name{RC.report.user}
\alias{RC.report.user}
\title{
Report about a user
}
\description{
Generate a report about the user activities. Various types of frequency statistics are reported.
}
\usage{
RC.report.user(id = "UseR user ", recursive = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
user id
}
  \item{recursive}{
should the search be done recursively (including children's children)?
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{numcomp}{Number of computations}
  \item{module.table}{Frequency of computations by R module}
  \item{keywords.table}{Frequency table of keywords}
  \item{year.table}{Frequency of compuations by year}
  \item{month.table}{Frequency of computations by month of the year}
  \item{day.table}{Frequency of computations by day of the week}
  \item{year.month.table}{Frequency of computations by month and year}
  \item{year.month.day.table}{Frequency of computations by day, month, and year}
  \item{hour.table}{Frequency of computations by hour of the day}
  \item{computations}{list of all computations from the user (this list has the same items as the list that is produced by RC.ls()}
}
\examples{
#[not run]
#r <- RC.report.user(id="s0801036")
#r
}
\keyword{ utilities }

