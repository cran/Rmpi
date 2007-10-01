\name{bin.nchar}
\alias{bin.nchar}
\alias{string}
\alias{mpi.comm.is.null}
\title{Internal functions}
\description{ Internal functions used by other MPI functions.

\code{bin.nchar} is used to find the number of char in a binary 
character vector (length 1) produced by 
\code{serialize}.

\code{mpi.comm.is.null} is used to test if a comm is MPI\_COMM\_NULL (empty 
members).

\code{string} create a string (empty space character) buffer.
}

\usage{
bin.nchar(x)
mpi.comm.is.null(comm)
string(length)
}
\arguments{
  \item{comm}{a communicator number.}
  \item{x}{an R object.}
  \item{length}{length of a string.}
}
\value{
\code{bin.nchar} returns the length of a binary char vector.

\code{string} returns an empty character string.
}
\seealso{
  \code{\link{mpi.spawn.Rslaves}}
}
\author{
Hao Yu
}
\keyword{utilities}

