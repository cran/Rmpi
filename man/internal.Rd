\name{bin.nchar}
\alias{bin.nchar}
\alias{mpi.remote.fun}
\alias{getpid}
\alias{mpi.remote.slave}
\alias{string}
\alias{typeindex}
\alias{mpi.comm.is.null}
\title{Internal functions}
\description{
Internal functions used by other MPI functions.

\code{bin.nchar} is used to find the number of char in a binary 
character vector (length 1) produced by 
\code{\link[serialize]{serialize}}.

\code{getpid} is used to get R process id which then is used for setting R slave 
log files.

\code{mpi.comm.is.null} is used to test if a comm is MPI\_COMM\_NULL (empty 
members).

\code{mpi.remote.fun} is used by \code{\link{mpi.remote.exec}} for preparing a 
remote function with master variables.

\code{mpi.remote.slave} is executed on R slaves by 
\code{\link{mpi.remote.exec}} (to carry out a remote command). 

\code{string}  create a string (empty space character) buffer.

\code{typeindex} checks an R object mode (integer, double, char, or others).
}
\usage{
bin.nchar(x)
getpid()
mpi.comm.is.null(comm)
mpi.remote.fun(cmd, ..., needsub = TRUE, width.cutoff = 500)
mpi.remote.slave(cmd, tag, ret)
string(length)
typeindex(x)
}
\arguments{
  \item{comm}{a communicator number.}
  \item{cmd}{a function name.}
  \item{...}{argument lists to be passed to cmd.}
  \item{needsub}{logical.}
  \item{width.cutoff}{an option used by \code{deparse}.}
  \item{tag}{a tag number}
  \item{ret}{logical.}
  \item{x}{an R object.}
  \item{length}{length of a string.}
}
\value{
\code{bin.nchar} returns the length of a binary char vector.

\code{getpid} returns an integer (process id).

\code{mpi.remote.fun} returns a string (a function with string quotation).

\code{string} returns an empty character string.

\code{typeindex} returns \code{c(1, length(x))} if \code{x} is an integer 
vector; \code{c(2,length(x))} if \code{x} is a double vector; and -1 for other 
objects. } 
\seealso{
  \code{\link{mpi.spawn.Rslaves}}, 
  \code{\link{mpi.remote.exec}}.
}
}
\author{
Hao Yu
}
\keyword{utilities}
