\name{mpi.get.processor.name}
\title{MPI_Get_processor_name API}
\usage{
  mpi.get.processor.name(short = TRUE)
}
\alias{mpi.get.processor.name}
\arguments{
 \item{short}{a logical.}
}
\description{
  \code{mpi.get.processor.name} returns the host name (a string) where 
it is executed. 
}
\value{
a base host name if \code{short = TRUE} and a full host name otherwise.
}
\author{
Hao Yu
}
\references{
  \url{https://www.open-mpi.org/}
}
%\examples{
%}
\keyword{interface}
 
