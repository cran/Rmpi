\name{mpi.init.sprng}
\alias{mpi.init.sprng}
\title{Interface to rsprng for initializing SPRNG}
\description{
  \code{mpi.init.sprng} is used to initialize the parallel random 
   number generator (PRNG) states in MPI environment. It is a collective 
   call so all members in a comm must call it together.
}
\usage{
mpi.init.sprng(seed = floor(runif(1, 1, 2147483648)), 
		   kindprng = "default",
                   para = 0,
		   rank = 0,
		   comm = 1)
}
\arguments{
  \item{seed}{an integer of random number seed.} 
  \item{kindprng}{a character string of the desired kind of parallel
    random number generator.}
  \item{para}{additional parameters for the parallel random number
    generators.  If \code{para} is 0, default parameters for each PRNG
    are used.  When invalid parameter is given, a warning is issued and
    the default parameter is used.}
  \item{rank}{the sender who distributes seed, kindprng, para to other 
   members in a comm, i.e., other members' seed, kindprng, and para are 
   ignored.}
  \item{comm}{a comm number.}
}
\value{
  Return the a two-element character vector of the RNG and normal kinds
  in use before the call.
}
\section{Warning}{
  When \code{sprng} is not used any more, please run 
  \code{\link[rsprng]{free.sprng}} on all slaves and master to set back 
  to R default RNG. If not doing so, an error message will come out like 
  ".Random.seed[1] = 5 but no user-supplied generator" in the next 
  session. One quick remedy is \code{rm(.Random.seed)}. 
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link[rsprng]{init.sprng}}
}
\author{
  Hao Yu
}
\keyword{interface}
\keyword{distribution}
