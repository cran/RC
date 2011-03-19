\name{RC.print.tree}
\alias{RC.print.tree}
\title{
Print tree of parent-child relationships
}
\description{
This function is typically used after a tree (of parent-child relationships) has been generated with the RC.tree() function. RC.print.tree() outputs a hierarchical tree in text format. The 'current' computation is indicated with ***. The tree contains all children and parents of the 'current' computation. However, the brothers/sisters are not shown.
}
\usage{
RC.print.tree(mytree = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mytree}{
tree produced by RC.tree()
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
#[not run]
#search computations with the 'babies' keyword
#r <- RC.ls(keyword="babies")
#fetch and print the tree about the first computation from the list
#mytree.1 <- RC.tree(r$url[1])
#RC.print.tree(mytree.1)
#fetch the tree based on the root parent
#mytree.2 <- RC.tree(mytree.1$url[1])
#print the entire tree (generates a lot of output)
#RC.print.tree(mytree.2)
}
\keyword{ print }

