\name{Deprecated}
\title{Deprecated Functions}
\alias{mpi.bcast.send}
\alias{mpi.bcast.recv}
\alias{mpi.bcast.send.cmd}
\alias{mpi.bcast.recv.cmd}
\alias{mpi.bcast.send.Robj}
\alias{mpi.bcast.recv.Robj}
\alias{type2data}
\description{
These functions are provided for compatibility with older versions of Rmpi 
only, and may be defunct as soon as of the next release. 

\code{mpi.bcast.send} and \code{mpi.bcast.recv} are disabled. Use 
\code{\link{mpi.bcast}} or \code{\link{mpi.bcast.Robj}} instead.

\code{mpi.bcast.send.cmd} and \code{mpi.bcast.recv.cmd} are replaced by 
\code{\link{mpi.bcast.cmd}}.

\code{mpi.bcast.send.Robj} and \code{mpi.bcast.recv.Robj} are replaced by 
\code{\link{mpi.bcast.Robj}}.


}
\usage{
mpi.bcast.recv(rank=0, comm=1)
mpi.bcast.send(x, rank=0, comm=1)
mpi.bcast.recv.cmd(rank=0, comm=1)
mpi.bcast.send.cmd(cmd, rank=0, comm=1, width.cutoff=500)
mpi.bcast.recv.Robj(rank=0, comm=1, envir=sys.parent())
mpi.bcast.send.Robj(obj, rank=0, comm=1, width.cutoff=500)
}
\arguments{
\item{rank}{the sender}
\item{comm}{a communicator number}
\item{cmd}{a command to be sent}
\item{x}{data to be sent}
\item{obj}{an R object to be sent}
\item{width.cutoff}{used by deparse}
\item{envir}{used by eval}
}
\value{
} 
\author{
Hao Yu
}
%\examples{
%}
\keyword{utilities}
