\name{mpi.bcast.cmd}
\title{Extension of MPI\_Bcast API}
\alias{mpi.bcast.cmd}
\description{
  \code{mpi.bcast.cmd} is an extension of \code{\link{mpi.bcast}}. 
  It is mainly used to transmit a command from master to all R slaves 
  spawned by using slavedaemon.R script (block version) or islavedaemon.R 
  (nonblock version).
}
\usage{
mpi.bcast.cmd(cmd=NULL, rank = 0, comm = 1, nonblock=FALSE, sleep=0.1)
}
\arguments{
  \item{cmd}{a command to be sent for master.}
  \item{rank}{the sender}
  \item{comm}{a communicator number}
  \item{nonblock}{logical. If TRUE, a nonblock procedure is used on all receivers so that
they will consume none or little CPUs while waiting.}
  \item{sleep}{a sleep interval, used when nonblock=TRUE. Smaller sleep is, more response receivers are, 
  more CPUs consume}
}
\value{
  \code{mpi.bcast.cmd} returns no value for the sender and an expression 
  of the transmitted command for others.
} 
\details{
  \code{\link{deparse}} is used to convert a command into a char vector. 
  Some syntax errors of the command will be automatically detected by 
  \code{deparse}. Then the char vector is broadcasted to all members (using
  for loop with mpi.send and mpi.recv pair). After receiving the char vector, 
  the receivers use \code{\link{parse}} to convert the char vector into an expression. 
  Finally slavedaemon.R script will evaluate this expression. Hence \code{mpi.bcast.cmd} 
  is only used by slavedaemon.R, or by whatever a R script based on slavedaemon.R. 
  
  If nonblock=TRUE, then on receiving side, a nonblock procedure is used to check if 
  there is a message. If not, it will sleep for the specied amount and repeat itself.
  
  Please use \code{\link{mpi.remote.exec}} if you want the executed results returned from R 
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
