\name{mpi.comm}
\title{MPI\_Comm\_dup, MPI\_Comm\_rank. and MPI\_Comm\_size APIs}
\usage{
  mpi.comm.dup(comm, newcomm)
  mpi.comm.rank(comm = 1)
  mpi.comm.size(comm = 1)
}
\alias{mpi.comm.dup}
\alias{mpi.comm.rank}
\alias{mpi.comm.size}
\arguments{
  \item{comm}{a communicator number}
  \item{newcomm}{a new communicator number}
}
\description{
  \code{mpi.comm.dup} duplicates (copies) a comm to a 
new comm. \code{mpi.comm.rank} 
returns its rank in a comm. \code{mpi.comm.size} returns 
the total number of members in a comm. 
}
\author{
Hao Yu
}
\references{
  \url{http://www.lam-mpi.org/}, 
  \url{http://www-unix.mcs.anl.gov/mpi/www/www3/}
}
%\examples{
%}
\keyword{interface}
 
