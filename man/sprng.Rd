\name{sprng}
\title{Scalable Parallel Random Number Generators}
\alias{make.sprng.seed}
\alias{get.sprng.seed}
\alias{init.sprng}
\alias{pack.sprng}
\alias{unpack.sprng}
\alias{printsprng}
\alias{free.sprng}
\alias{spawn.sprng}
\alias{unpack.spawned.stream}
\alias{startnew.stream}
\description{
  MPI version of SPRNG (Scalable Parallel Random Number Generators) is 
used to replace the original R uniform random number generator which is 
not suitable in parallel computation environment. It uses one seed 
but different stream numbers for different members in a comm. Since SPRNG 
chooses its own way to distribute a seed among members in comm 0 
(MPI\_COMM\_WORLD), it is strictly running either in slave 
environment or master environment. The argument \code{who = 
c("slave","master")} is used for that purpose.

\code{make.sprng.seed} generates a seed and saves it in .Sprng.seed. This 
seed should be identical among all members in comm 0.

\code{init.sprng} initializes SPRNG stream for a given seed and a type of 
generators. Then it replaces the original R uniform RNG by its own one.

\code{free.sprng} deallocates SPRNG stream, delete .Sprng.seed and set 
back R uniform RNG.

\code{printsprng} prints SPRNG seed, stream number, and type of RNG to 
stdio. One has to use `print(.Last.value)' to show the detail on R slaves 
(show up in R slaves log files).

\code{get.sprng.seed} just retrieve .Sprng.seed if it exists.

\code{pack.sprng} packs current SPRNG stream into an internal memory. 

\code{unpack.sprng} unpacks a saved SPRNG stream and replace current SPRNG 
stream (can be used to reproduce the same random sequences).

\code{spawn.sprng} spawns a new set of streams from the original one and 
saves them in an internal memory.

\code{unpack.spawned.stream} unpacks the first spawned stream.

\code{startnew.stream} starts a new stream and should be called by all 
members in comm 0.
}
\usage{
make.sprng.seed(who=c("slave","master"), pos=1)
init.sprng(who = c("slave", "master"), 
	seed = make.sprng.seed(who = match.arg(who)), 
    	sprngkind = c("LFG", "LCG", "LCG64", "CMRG", "MLFG", "PMLCG")) 
free.sprng(pos = 1)
get.sprng.seed(who = c("slave", "master"))
printsprng()
pack.sprng()
unpack.sprng(packold = FALSE)
spawn.sprng(nspawned)
unpack.spawned.stream(packold = TRUE)
startnew.stream(packold = TRUE)
}
\arguments{
  \item{who}{who executes the program.}
  \item{seed}{a seed for SPRNG.}
  \item{sprngkind}{type of SPRNG RNG. The default one is LFG.}
  \item{packold}{if TRUE, the old stream will be packed (saved).}
  \item{nspawned}{the number of streams to be spawned.}
  \item{pos}{used to make .Sprng.seed a global variable.}
}
\value{
  \code{make.sprng.seed} returns a seed and saves it in .Sprng.seed.

  \code{init.sprng}, \code{free.sprng}, \code{pack.sprng}, 
\code{unpack.sprng}, \code{unpack.spawned.sprng}, and
\code{startnew.stream} return 1 if success and 0 otherwise.

  \code{printsprng} returns no value except printing SPRNG information to stdio. 
} 
\section{Warning}{
SPRNG seed must be the same among all members in comm 0 otherwise the 
independence of each stream is not guarantied. Once SPRNG is 
initialized, one cannot change the seed. Instead one should use 
\code{startnew.stream} to start a new stream. Another possibility is to 
initialize a new type of SPRNG. 
 }
\author{
Hao Yu
}
\references{
  \url{http://sprng.cs.fsu.edu/sprng.html}. 
}
} 
%\examples{ 
%} 
\keyword{utilities}

