\name{mpi.apply}
\alias{mpi.apply}
\title{Scatter an array to slaves and then apply a fun}
\description{
An array (length <= total number of slaves) is scattered to slaves so that the first 
slave calls \code{fun} with arguments \code{x[[1]]} and \code{...}, the second one 
calls with arguments \code{x[[2]]} and \code{...}, and so on.} 
\usage{
mpi.apply(x, fun, ..., comm=1)  
}
\arguments{
  \item{x}{an array}
  \item{fun}{a function}
  \item{...}{optional arguments to \code{fun}}
  \item{comm}{a communicator number}
}
\value{
A list of the results is returned. Its length is the same as that of \code{x}. In 
case the call \code{fun} with arguments \code{x[[i]]} and \code{...} fails on ith 
slave, corresponding error message will be returned in the returning list.  }

%\seealso{
%  \code{\link{mpi.spawn.Rslaves}},
%  \code{\link{mpi.bcast.cmd}}
%  \code{\link{mpi.bcast.Robj2slave}}
%}
\author{
Hao Yu
}
\keyword{utilities}
