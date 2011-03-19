\name{RC.tree}
\alias{RC.tree}
\title{
Fetch tree of parent & child relationships
}
\description{
Creates a tree of parent-child relationships of all computations that correspond to a specific computation. The tree contains all parents and children of the 'current' computation. The sisters/brothers however are not displayed.
}
\usage{
RC.tree(url = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
The unique URL of the 'current' computation
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
#search computations with the 'babies' keyword [not run]
#r <- RC.ls(keyword="babies")
#fetch and print the tree about the first computation from the list
#mytree.1 <- RC.tree(r$url[1])
#RC.print.tree(mytree.1)
#fetch the tree based on the root parent
#mytree.2 <- RC.tree(mytree.1$url[1])
#print the entire tree (generates a lot of output)
#RC.print.tree(mytree.2)
}
\keyword{ graphs }

