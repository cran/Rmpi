\name{mpi.comm.size}
\title{MPI_Comm_c2f, MPI_Comm_dup, MPI_Comm_rank, and MPI_Comm_size APIs}
\usage{
  mpi.comm.c2f(comm=1)
  mpi.comm.dup(comm, newcomm)
  mpi.comm.rank(comm = 1)
  mpi.comm.size(comm = 1)
}
\alias{mpi.comm.c2f}
\alias{mpi.comm.dup}
\alias{mpi.comm.rank}
\alias{mpi.comm.size}
\arguments{
  \item{comm}{a communicator number}
  \item{newcomm}{a new communicator number}
}
\description{
\code{mpi.comm.c2f} converts the comm (a C communicator) and returns an integer that can be
used as the communicator in external FORTRAN code. \code{mpi.comm.dup} duplicates 
(copies) a comm to a new comm. \code{mpi.comm.rank} 
returns its rank in a comm. \code{mpi.comm.size} returns 
the total number of members in a comm. 
}
\author{
Hao Yu
}
\references{
  \url{https://www.open-mpi.org/}
}
\examples{
\donttest{
#Assume that there are some slaves running
#mpi.comm.size(comm=1)
#mpi.comm.size(comm=0)

#mpi.remote.exec(mpi.comm.rank(comm=1))
#mpi.remote.exec(mpi.comm.rank(comm=0))

#mpi.remote.exec(mpi.comm.size(comm=1))
#mpi.remote.exec(mpi.comm.size(comm=0))

#mpi.bcast.cmd(mpi.comm.dup(comm=1,newcomm=5))
#mpi.comm.dup(comm=1,newcomm=5)
}
}
\keyword{interface}
 
