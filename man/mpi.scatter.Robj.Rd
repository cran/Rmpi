\name{mpi.scatter.Robj}
\title{Extensions of MPI\_ SCATTER and MPI\_SCATTERV }
\alias{mpi.scatter.Robj}
\description{
  \code{mpi.scatter.Robj} is used to scatter a list to all numbers.
}
\usage{
mpi.scatter.Robj(obj = NULL, root = 0, comm = 1)
}
\arguments{
  \item{obj}{a list object to be scattered from the root}
  \item{root}{rank of the scatter.}
  \item{comm}{a communicator number.}
}
\value{
  \code{mpi.scatter.Robj} for non-root members,  returns the
     scattered message . For the root  member, it returns the
     portion belonging to itself.
}
\details{
  \code{mpi.scatter.Robj} is an extension of \code{\link{mpi.scatter}} for
  scattering a list  object  from a sender to everyone.
}
\author{
Hao Yu  and   Wei Xia
}
\seealso{
  \code{\link{mpi.scatter}},
  \code{\link{mpi.gather.Robj}},
}
\examples{
\dontrun{
#assume that there are three slaves running
mpi.bcast.cmd(x<-mpi.scatter.Robj())

xx <- list("master",rnorm(3),letters[2],1:10)
mpi.scatter.Robj(obj=xx)

mpi.remote.exec(x)
}
}
\keyword{utilities}

