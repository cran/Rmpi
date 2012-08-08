\name{mpi.setup.rngstream}
\alias{mpi.setup.rngstream}
\title{Setup parallel RNG on all slaves}
\description{
  \code{mpi.setup.rngstream} setups RNGstream (\pkg{rlecuyer}) on all 
slaves. 
}
\usage{
mpi.setup.rngstream(seed=c(runif(3,0,2^32-210),runif(3,0,2^32-22854)), 
		comm = 1)
}
\arguments{
  \item{seed}{an integer or a vector of 6 integers of random number seed(s).} 
  \item{comm}{a comm number.}
}
\value{
  If setup fails, an error message will be returned. It is most likely that the 
\pkg{rlecuyer} is not installed properly on all slaves. \code{mpi.setup.rngstream} 
saves its seed as .Rngstream.seed. 
}
\details{
  \code{mpi.setup.rngstream} can be run only on master node. It can be run later 
on with different seed.
}
\author{
  Hao Yu
}
\keyword{interface}
\keyword{distribution}
