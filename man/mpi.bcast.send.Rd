\name{mpi.bcast.send}
\title{Various Extensions of MPI\_Bcast API}
\alias{mpi.bcast.send}
\alias{mpi.bcast.recv}
\description{
  \code{mpi.bcast.send} and \code{mpi.bcast.recv} are two extensions of 
  \code{\link{mpi.bcast}}. They simplify the message (int, double or char) 
  to be broadcasted. 
}
\usage{
mpi.bcast.send(x, rank = 0, comm = 1)
mpi.bcast.recv(rank = 0, comm = 1)
}
\arguments{
  \item{x}{data to be sent. Only int, double or char vector is supported.}
  \item{rank}{the sender}
  \item{comm}{a communicator number}
}
\value{
  \code{mpi.bcast.send} returns no value. \code{mpi.bcast.recv} returns  
  the message broadcasted by the sender (specified by the rank).
} 
\details{
  \code{mpi.bcast.send} broadcasts two messages to receivers who use 
  \code{mpi.bcast.recv}. The first message is used to inform the 
  receivers the type and length of a vector to be sent. The second message 
  is the real data (integer, double, or character vector). To broadcast a 
  general R object, please use the pair \code{\link{mpi.bcast.send.Robj}} 
  and \code{\link{mpi.bcast.recv.Robj}} or 
  \code{\link{mpi.bcast.Robj2slave}}. 
 }
\author{
Hao Yu
}
\seealso{
  \code{\link{mpi.bcast.send.Robj}},
  \code{\link{mpi.bcast.recv.Robj}},
  \code{\link{mpi.bcast.Robj2slave}}.
}
%\examples{
%}
\keyword{utilities}
