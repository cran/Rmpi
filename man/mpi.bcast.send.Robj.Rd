\name{mpi.bcast.send.Robj}
\title{Various Extensions of MPI\_Bcast API}
\alias{mpi.bcast.send.Robj}
\alias{mpi.bcast.recv.Robj}
\alias{mpi.bcast.Robj2slave}
\description{
  \code{mpi.bcast.send.Robj}, \code{mpi.bcast.recv.Robj}, and
  \code{mpi.bcast.Robj2slave} are used to move a general R object around among 
  master and all slaves. 
}
\usage{
mpi.bcast.send.Robj(obj, rank = 0, comm = 1, width.cutoff = 500)
mpi.bcast.recv.Robj(rank = 0, comm = 1, envir = sys.parent())

mpi.bcast.Robj2slave(obj, comm = 1, width.cutoff = 500)
}
\arguments{
  \item{obj}{an R object. Can be any R object as long as it does not 
  contain any reference to functions.}
  \item{rank}{the sender.}
  \item{comm}{a communicator number.}
  \item{width.cutoff}{an option used by \code{deparse}.}
  \item{envir}{an option used by \code{eval} to decide if the transmitted R 
  object is local or global.}
}
\value{
  \code{mpi.bcast.send.Robj} returns no value. \code{mpi.bcast.recv.Robj} 
  and \code{mpi.bcast.Robj2slave} return the transmitted R object. 
} 
\details{
  \code{mpi.bcast.send.Robj} and \code{mpi.bcast.recv.Robj} are two general 
  extensions of \code{\link{mpi.bcast}} for moving a general R object around 
  from a sender to everyone. Most R object can be moved around except such an 
  object as a \code{lm} object which contains a formula. This is rather a 
  restriction of the pair \code{deparse} and \code{parse} than MPI itself. 
  \code{mpi.bcast.Robj2slave} is a specific extension to R slave environment.
  It consolidates the two functions \code{mpi.bcast.send.Robj} and 
  \code{mpi.bcast.recv.Robj} into one and does an R object transmission from 
  master to all slaves only. Please note that this is not an efficient way to 
  move an R object around since the pair \code{deparse} and \code{parse} will 
  double the original object size by using char vectors.
}
\author{
Hao Yu
}
\seealso{
  \code{\link{mpi.send.Robj}},
  \code{\link{mpi.recv.Robj}},
}
%\examples{
%}
\keyword{utilities}
