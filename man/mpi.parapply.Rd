\name{mpi.applyLB}
\alias{mpi.applyLB}
\alias{mpi.parApply}
\alias{mpi.parLapply}
\alias{mpi.parSapply}
\alias{mpi.parRapply}
\alias{mpi.parCapply}
\alias{mpi.parReplicate}
\title{(Load balancing) parallel apply}
\description{
(Load balancing) parallel\code{lapply} and related functions. 
}
\usage{
mpi.applyLB(x, fun, ..., apply.seq=NULL, comm=1)
mpi.parApply(x, MARGIN, fun, ..., job.num = mpi.comm.size(comm)-1,
                    apply.seq=NULL, comm=1)
mpi.parLapply(x, fun, ..., job.num=mpi.comm.size(comm)-1, apply.seq=NULL, comm=1)  
mpi.parSapply(x, fun, ..., job.num=mpi.comm.size(comm)-1, apply.seq=NULL, simplify=TRUE, 
		USE.NAMES = TRUE, comm=1)  
mpi.parRapply(x, fun, ..., job.num=mpi.comm.size(comm)-1, apply.seq=NULL, comm=1)  
mpi.parCapply(x, fun, ..., job.num=mpi.comm.size(comm)-1, apply.seq=NULL, comm=1)  
mpi.parReplicate(n, expr, job.num=mpi.comm.size(comm)-1, apply.seq=NULL, simplify = TRUE, comm=1)
}
\arguments{
  \item{x}{an array or matrix.}
  \item{MARGIN}{vector specifying the dimensions to use.}
  \item{fun}{a function.}
  \item{simplify}{logical; should the result be simplified to a vector or
          matrix if possible?}
  \item{USE.NAMES}{logical; if \code{TRUE} and if \code{x} is character, use \code{x} as
          \code{names} for the result unless it had names already.}
  \item{n}{number of replications.}
  \item{expr}{expression to evaluate repeatedly.}
  \item{job.num}{Total job numbers. If job numbers is bigger than total slave numbers (default 
value), a load balancing approach is used.}
  \item{apply.seq}{if reproducing the same computation (simulation) is desirable, set it 
to the integer vector .mpi.applyLB generated in previous computation (simulation).}
  \item{...}{optional arguments to \code{fun}}
  \item{comm}{a communicator number}
}
\details{
Unless length of \code{x} is no more than total slave numbers (slave.num) and in this case 
\code{mpi.applyLB} is the same as \code{mpi.apply}, \code{mpi.applyLB} sends a next job to a 
slave who just delivered a finished job. The sequence of slaves who deliver results to master are 
saved into \code{.mpi.applyLB}. It keeps track which part of results done by which slaves. 
\code{.mpi.applyLB} can be used to reproduce the same simulation result if the same seed is 
used and the argument \code{apply.seq} is equal to \code{.mpi.applyLB}.
 
With the default value of argument \code{job.num} which is slave.num, \code{mpi.parApply}, 
\code{mpi.parLapply}, \code{mpi.parSapply}, \code{mpi.parRapply}, \code{mpi.parCapply}, and 
\code{mpi.parSapply} are clones of \pkg{snow}'s parApply, parLappy, parSapply, parRapply, parCapply, 
and parSapply, respectively. When \code{job.num} is bigger than slave.num, a load balancing approach 
is used.
 }
\section{Warning}{
When using the argument \code{apply.seq} with \code{.mpi.applyLB}, be sure all settings are the same 
as before, i.e., the same data, job.num, slave.num, and seed. Otherwise a deadlock could occur. 
Notice that \code{apply.seq} is useful only if \code{job.num} is bigger than slave.num. 
}
\seealso{
\code{\link{mpi.apply}}
}
\keyword{utilities}
