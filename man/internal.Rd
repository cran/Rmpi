\name{bin.nchar}
\alias{bin.nchar}
\alias{mpi.wrap.fun}
\alias{getpid}
\alias{mpi.remote.slave}
\alias{mpi.parallel.slave}
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

\code{mpi.wrap.fun} is used by \code{\link{mpi.parallel.sim}} for preparing a 
wrap function.

\code{mpi.remote.slave} is executed on R slaves by 
\code{\link{mpi.remote.exec}} (to carry out a remote command). 

\code{mpi.parallel.slave} is executed on R slaves by 
\code{\link{mpi.parallel.sim}} (to carry out parallel Monte Carlo 
simulation). 

\code{string}  create a string (empty space character) buffer.

\code{typeindex} checks an R object mode (integer, double, char, or others).
}
\usage{
bin.nchar(x)
getpid()
mpi.comm.is.null(comm)
mpi.wrap.fun(cmd,arg,cmd.name=as.character(substitute(cmd)))
mpi.remote.slave()
mpi.parallel.slave()
string(length)
typeindex(x)
}
\arguments{
  \item{comm}{a communicator number.}
  \item{cmd}{a function name.}
  \item{arg}{optional argument list to \code{cmd}.}
  \item{cmd.name}{a function name as character.}
  \item{x}{an R object.}
  \item{length}{length of a string.}
}
\value{
\code{bin.nchar} returns the length of a binary char vector.

\code{getpid} returns an integer (process id).

\code{mpi.wrap.fun} returns a function with argument values replaced from 
\code{arg}. If there is no ... argument in \code{cmd}, the original 
function is returned. Otherwise a wrapper function is returned (it is a 
wrapper to the function \code{cmd}). 

\code{string} returns an empty character string.

\code{typeindex} returns \code{c(1, length(x))} if \code{x} is an integer 
vector; \code{c(2,length(x))} if \code{x} is a double vector; and -1 for other 
objects. } 
\seealso{
  \code{\link{mpi.spawn.Rslaves}}, 
  \code{\link{mpi.remote.exec}}.
  \code{\link{mpi.parallel.sim}}.
}
}
\author{
Hao Yu
}
\keyword{utilities}
