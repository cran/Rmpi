\name{mpi.probe}
\title{MPI\_Probe API}
\usage{
mpi.probe(source, tag, comm = 1, status = 0)
}
\alias{mpi.probe}
\arguments{
  \item{source}{the source of incoming message or mpi.any.source() for any 
source.}
  \item{tag}{a tag number or mpi.any.tag() for any tag.}
  \item{comm}{a communicator number}
  \item{status}{a status number}
}
\description{
  \code{mpi.probe} uses the source and tag of incoming message to set a status.
}
\details{
  When \code{\link{mpi.send}} is used to send a message, the receiver may not 
  know the exact length before receiving it. \code{mpi.probe} is used to probe 
  the incoming message and put some information into a status. Then the exact 
  length can be found by using \code{\link{mpi.get.count}} to such a status.
If the wild card \code{mpi.any.source} or \code{mpi.any.tag} are used, then one 
can use \code{\link{mpi.get.sourcetag}} to find the exact source or tag of a 
sender. }
\value{
  1 if success; 0 otherwise.
}
\author{
Hao Yu
}
\references{
  \url{http://www.lam-mpi.org/}, 
  \url{http://www-unix.mcs.anl.gov/mpi/www/www3/}
}
\seealso{
 \code{\link{mpi.send}}, \code{\link{mpi.recv}},
 \code{\link{mpi.get.count}}
}
%\examples{
%}
\keyword{interface}
 
