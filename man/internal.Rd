\name{string}
\alias{string}
\alias{mpi.comm.is.null}
\alias{.docall}
\alias{.mpi.worker.apply}
\alias{.mpi.worker.applyLB}
\alias{.mpi.worker.exec}
\alias{.mpi.worker.sim}
\alias{.typeindex}
\alias{.simplify}
\alias{.splitIndices}
\alias{.onUnload}
\alias{.mpi.undefined}
\alias{.force.type}
\title{Internal functions}
\description{ Internal and hidden functions used by other MPI functions.

\code{mpi.comm.is.null} is used to test if a comm is MPI_COMM_NULL (empty
members).

\code{string} create a string (empty space character) buffer.

\code{.docall} a wrap to docall function.

\code{.mpi.worker.apply} apply like function used by workers.

\code{.mpi.worker.applyLB} apply like function used by workers (load balancing).

\code{.mpi.worker.exec} real execution by workers when using \link{mpi.remote.exec}.

\code{.mpi.worker.sim} real simulation by workers when using \link{mpi.parSim}.

\code{.type.index} identify input data type: integer, numeric, raw, or others.

\code{.simplify} simplify internal objects.

\code{.splitIndices} split parall apply jobs evenly.

\code{.onUnload} clean MPI when Rmpi is unloaded.

\code{.mpi.undefined} undefined mpi object.

\code{.force.type} force input data type object specified by type.

}

\usage{
mpi.comm.is.null(comm)
string(length)
.docall(fun, args)
}
\arguments{
  \item{comm}{a communicator number.}
  \item{length}{length of a string.}
  \item{fun}{ a function object.}
  \item{args}{arguments to function.}
}
\value{
\code{string} returns an empty character string.
}
\seealso{
  \code{\link{mpi.spawn.Rslaves}}
}
\author{
Hao Yu
}
\keyword{utilities}

