\name{mpi.send.Robj}
\title{Extensions of MPI\_Send and MPI\_Recv APIs}
\alias{mpi.send.Robj}
\alias{mpi.recv.Robj}
\description{
  The pair \code{mpi.send.Robj} and \code{mpi.recv.Robj} are two 
  extensions of \code{mpi.send} and \code{mpi.recv}. They are used to 
  transmit a general R object from any source to any destination.  
}
\usage{
mpi.send.Robj(obj, dest, tag, comm = 1, width.cutoff = 500)
mpi.recv.Robj(source, tag, comm = 1, status = 0, envir = sys.parent())
}
\arguments{
  \item{obj}{an R object. Can be any R object as long as it does not 
          contain any reference to functions. 
}
  \item{dest}{the destination rank.}
  \item{source}{the source rank.} 
  \item{tag}{non-negative integer.}
  \item{comm}{a communicator number.}
  \item{status}{a status number.}
  \item{width.cutoff}{an option used by \code{deparse}.}
  \item{envir}{an option used by \code{eval} to decide if the transmitted 
  R object is local or global.}
}
\value{
  \code{mpi.send.Robj} returns no value. \code{mpi.recv.Robj} returns the 
  the transmitted R object. 
} 
\details{
  The pair \code{deparse} and \code{parse} are used to encode (decode) an 
  R object to (from) char vectors. \code{mpi.send.Robj} uses 
  \code{deparse} to encode an R object into a char vector (collapsed 
  into a length 1 char vector). It sends two messages to the destination. The 
  first message is used to inform the receiver the number of char of the 
  char vector. The second message is the char vector itself. The receiver uses 
  the first message to prepare the receive buffer for the second message. 
  Finally \code{parse} and \code{eval} are used to decode the char vector. 
  Since two passes are used, one cannot use  \code{mpi.get.count}, 
  \code{mpi.any.source}, \code{mpi.any.tag}, or \code{mpi.find.sourcetag}. 
  Notice that this is not an efficient way to  move an R object around
  since the pair `deparse' and `parse' will  double the original
  object size by using char vectors.
 }
\author{
Hao Yu
}
\references{
  \url{http://www.lam-mpi.org/}, 
  \url{http://www-unix.mcs.anl.gov/mpi/www/www3/}
}
\seealso{
  \code{\link{mpi.send}},
  \code{\link{mpi.recv}},
}
%\examples{
%}
\keyword{utilities}

