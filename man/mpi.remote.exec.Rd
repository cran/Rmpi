\name{mpi.remote.exec}
\alias{mpi.remote.exec}
\title{Remote Executions on R slaves}
\description{
 Remotely execute a command on R slaves spawned by using slavedaemon.R script 
 and return all executed results back to master. 
} 
\usage{
mpi.remote.exec(cmd, ..., comm = 1, ret = TRUE, width.cutoff = 200)  
}
\arguments{
  \item{cmd}{the command to be executed on R slaves}
  \item{...}{used as arguments to cmd (function command) for passing their 
  (master) values to R slaves, i.e., if `myfun(x)' will be executed on R slaves 
with `x' as master variable, use mpi.remote.exec(cmd=myfun, x).}
  \item{comm}{a communicator number.}
  \item{ret}{return executed results from R slaves if TRUE.}
  \item{width.cutoff}{an option used by \code{deparse}.}
}
\value{
  return executed results from R slaves if the argument \code{ret} is 
  set to be TRUE. If executed results are NULL on R slaves, the char "NULL" will 
  be returned.  
}
\details{
  Once R slaves are spawned by \code{\link{mpi.spawn.Rslaves}} with the 
  slavedaemon.R script, they are waiting for instructions from master. One can 
  use \code{\link{mpi.bcast.send.cmd}} to send a command to R slaves. However it 
  will not return executed results. Hence \code{mpi.remote.exec} can be 
  considered an extension to \code{\link{mpi.bcast.send.cmd}}.    
}

\section{Warning}{
  \code{mpi.remote.exec} may have difficult guessing invisible executed results 
  on R slaves. Use `ret = FALSE' instead. Please do not pass large amount of 
  data through \code{...} to a function command. The use of the argument 
  \code{...} is just for the sake of passing some simple variables from master 
  to R slave.
} 
\seealso{
  \code{\link{mpi.spawn.Rslaves}},
  \code{\link{mpi.bcast.send.cmd}}
}
\author{
Hao Yu
}
\keyword{utilities}
