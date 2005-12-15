\name{mpi.setup.sprng}
\alias{mpi.setup.sprng}
\alias{mpi.setup.rngstream}
\title{Setup parallel RNG on all slaves}
\description{
  \code{mpi.setup.sprng} setups SPRNG (\pkg{rsprng}) on all slaves.
  \code{mpi.setup.rngstream} setups RNGstream (\pkg{rlecuyer}) on all 
slaves. 
}
\usage{
mpi.setup.sprng(seed=runif(1, 1, 2^31-1), kindprng="default", para=0, comm=1)
mpi.setup.rngstream(seed=c(runif(3,0,2^32-210),runif(3,0,2^32-22854)), 
		comm = 1)
}
\arguments{
  \item{seed}{an integer or a vector of 6 integers of random number seed(s).} 
  \item{kindprng}{a character string of the desired kind of parallel
    random number generator.}
  \item{para}{additional parameters for the parallel random number
    generators.  If \code{para} is 0, default parameters for each PRNG
    are used.  When invalid parameter is given, a warning is issued and
    the default parameter is used.}
  \item{comm}{a comm number.}
}
\value{
  If setup fails, an error message will be returned. It is most likely that the 
\pkg{rsprng} or \pkg{rlecuyer} is not installed properly on all slaves.
\code{mpi.setup.sprng} and \code{mpi.setup.rngstream} save their seeds as 
.Sprng.seed and .Rngstream.seed respectively. 
}
\details{
  \code{mpi.setup.sprng} and \code{mpi.setup.rngstream} can be run only on master 
node. Please use one and only one of them on one session. According to SPRNG 
specification, one can only run \code{mpi.setup.sprng} once before using any RNG on 
slaves. Otherwise independence of streams on slaves are not guarantied. If several 
simulations with different seeds are desirable on one session,  
\code{mpi.setup.rngstream} is preferred. 
}
\author{
  Hao Yu
}
\keyword{interface}
\keyword{distribution}
