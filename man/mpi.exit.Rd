\name{mpi.exit}
\alias{mpi.exit}
\title{Exit MPI Environment }
\description{
  Terminates MPI execution environment and detach the library Rmpi. 
}
\usage{
  mpi.exit()
}
\arguments{
  None
}
\value{
  Always return 1
}
\details{
  Normally, \code{\link{mpi.finalize}} is used to clean all MPI states. 
  However, it will not detach the library Rmpi. To be more safe leaving MPI,  
  \code{mpi.exit} not only calls \code{mpi.finalize} but also detaches the 
  library Rmpi. This will make reload the library Rmpi impossible. 
} 
\seealso{
  \code{\link{mpi.finalize}}
}
\author{
Hao Yu
}
\keyword{utilities}
