\name{mpi.send}
\title{MPI\_Send and MPI\_Recv APIs}
\alias{mpi.send}
\alias{mpi.recv}
\description{
  The pair \code{mpi.send} and \code{mpi.recv} are two most used blocking 
  calls for point-to-point communications. An int, double or char vector 
  can be transmitted from any source to any destination.  
}
\usage{
mpi.send(x, type, dest, tag,  comm = 1)
mpi.recv(x, type, source, tag,  comm = 1, status = 0)
}
\arguments{
  \item{x}{data to be sent or received. Must be the same 
  type for source and destination. The receive buffer must be as large as 
  the send buffer.}
  \item{type}{1 for integer, 2 for double, and 3 for 
  character. Others are not supported.}
  \item{dest}{the destination rank. Use \code{mpi.proc.null} for a 
  fake destination.}
  \item{source}{the source rank. Use \code{mpi.any.source} for any source 
  flag. Use \code{mpi.proc.null} for a fake source. }
  \item{tag}{non-negative integer. Use \code{mpi.any.tag} for any tag flag.}
  \item{comm}{a communicator number.}
  \item{status}{a status number.}
}
\value{
  \code{mpi.send} returns no value. \code{mpi.recv} returns the int, 
  double or char vector sent from \code{source}. 
} 
\details{
  The pair \code{mpi.send} and \code{mpi.recv} must be used together, 
  i.e., if there is a sender, then there must be a receiver. Any mismatch 
  will result a deadlock situation, i.e., programs stop responding. The 
  receive buffer must be large enough to contain an incoming message 
  otherwise programs will be crashed. One can use 
  \code{\link{mpi.get.count}} to find the length of an incoming message 
  before calling \code{mpi.recv}. If \code{\link{mpi.any.source}} or 
  \code{\link{mpi.any.tag}} is used in \code{mpi.recv}, one can use 
  \code{\link{mpi.get.sourcetag}} to find out the source or tag of the 
  received message. To send/receive an R object rather than an int, double 
  or char vector, please use the pair \code{\link{mpi.send.Robj}} and  
  \code{\link{mpi.recv.Robj}}.
 }
\author{
Hao Yu
}
\references{
  \url{http://www.lam-mpi.org/}, 
  \url{http://www-unix.mcs.anl.gov/mpi/www/www3/}
}
\seealso{
  \code{\link{mpi.send.Robj}},
  \code{\link{mpi.recv.Robj}},
  \code{\link{mpi.probe}}.  
  \code{\link{mpi.get.count}}, 
  \code{\link{mpi.get.sourcetag}}. 
} 
%\examples{ 
%} 
\keyword{interface}

