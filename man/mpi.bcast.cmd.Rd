\name{mpi.bcast.cmd}
\title{Various Extensions of MPI\_Bcast API}
\alias{mpi.bcast.send.cmd}
\alias{mpi.bcast.recv.cmd}
\description{
  \code{mpi.bcast.send.cmd} and \code{mpi.bcast.recv.cmd} are two extensions 
  of \code{\link{mpi.bcast}}. They are mainly used to transmit a command from 
  master to all R slaves spawned by using slavedaemon.R script.
}
\usage{
mpi.bcast.send.cmd(cmd, rank = 0, comm = 1, width.cutoff = 500)
mpi.bcast.recv.cmd(rank = 0, comm = 1)
}
\arguments{
  \item{cmd}{a command to be sent. No string quotation.}
  \item{rank}{the sender}
  \item{comm}{a communicator number}
  \item{width.cutoff}{a option used by the function \code{deparse}}
}
\value{
  \code{mpi.bcast.send.cmd} returns no value. \code{mpi.bcast.recv.cmd} 
returns an expression of the transmitted command. 
} 
\details{
  \code{\link{deparse}} is used to convert a command into a char vector. 
  Some syntax errors of the command will be automatically detected by 
  \code{deparse}. Then the char vector is broadcasted to all members. After 
  receiving the char vector, the receivers use \code{\link{parse}} to convert 
  the char vector into an expression. Finally slavedaemon.R script will evaluate 
  this expression. Hence \code{mpi.bcast.recv.cmd} is only used by slavedaemon.R, 
  or by whatever a R script based on slavedaemon.R. Please use  
 \code{\link{mpi.remote.exec}} if you want the executed results returned from R 
  slaves.
 }
\author{
Hao Yu
}
\seealso{
  \code{\link{mpi.remote.exec}}
}
%\examples{
%}
\keyword{utilities}
