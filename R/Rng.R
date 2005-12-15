mpi.init.sprng <- function(seed = runif(1, 1, 2^31-1),
               kindprng = "default",
               para = 0, rank = 0, comm = 1){
    if (!require (rsprng)) {
        stop("SPRNG cannot be loaded.")
    }

    commsize <- mpi.comm.size(comm)
    if (commsize == 0)
    stop(paste("It seems no members running on comm", comm))

    if (commsize == 1){
        if (!is.na (pmatch (kindprng, "default"))) 
            kindprng <- "LFG"
        kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
        if (is.na (kind)) {
            stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
        }
    return (init.sprng(1, 0, seed, kindprng, para))
    }

    commrank <- mpi.comm.rank(comm)
    if(commrank == rank){
    if (!is.na (pmatch (kindprng, "default"))) {
            kindprng <- "LFG"
        }     
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
        if (is.na (kind)) {
            stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
        }

        assign(".Sprng.seed", as.integer(c(seed, kind, para)), 
        env=.GlobalEnv)
        mpi.bcast(.Sprng.seed, type=1, rank=rank, comm=comm)   
    }
    else {
    assign(".Sprng.seed",  
        mpi.bcast(integer(3), type=1, rank=rank, comm=comm),
        env = .GlobalEnv)
    }
    kindprng <- switch(.Sprng.seed[2]+1, "LFG","LCG", "LCG64",   
                    "CMRG", "MLFG", "PMLCG")
    init.sprng(commsize, commrank, .Sprng.seed[1], kindprng, 
    .Sprng.seed[3]) 
}

mpi.setup.rngstream <- function(seed=c(runif(3,0,2^32-210),runif(3,0,2^32-22854)), comm = 1){
    if (!require(rlecuyer)) 
        stop("rlecuyer cannot be loaded.")

    commsize <- mpi.comm.size(comm)
    if (commsize < 3)
        stop("There is no slave or only one slave")
    if (!mpi.is.master())
        stop("Can be run only on master")
    .lec.init()
    .lec.SetPackageSeed(seed)
    names <- as.character(1:(commsize-1))
    .lec.CreateStream(names)
    states <- lapply(names, .lec.GetStateList)
    
    if (sum(mpi.remote.exec(as.integer(require(rlecuyer)),comm=comm))
            < commsize-1)
        stop("It seems rlecuyer is not installed properly on slave machines.")
    .Rngstream.seed <<- seed
    #adapted from snow
    initRNGstreamNode <- function (stream) {
        if (length(.lec.Random.seed.table$name) > 0) {
            rm(".lec.Random.seed.table", envir=.GlobalEnv)
            assign(".lec.Random.seed.table", list(Cg=matrix(0,nrow=0,ncol=6),
                                              Bg=matrix(0,nrow=0,ncol=6),
                                              Ig=matrix(0,nrow=0,ncol=6),
                                              AIP=matrix(0,nrow=0,ncol=2),
                                              name=c()), envir=.GlobalEnv)
        }
        .lec.Random.seed.table$Cg <<- rbind(.lec.Random.seed.table$Cg,
                                        stream$Cg[1:6])
        .lec.Random.seed.table$Bg <<- rbind(.lec.Random.seed.table$Bg,stream$Bg)
        .lec.Random.seed.table$Ig <<- rbind(.lec.Random.seed.table$Ig,stream$Ig)
        .lec.Random.seed.table$AIP <<- rbind(.lec.Random.seed.table$AIP,
                                         c(stream$Anti, stream$IncPrec))
        .lec.Random.seed.table$name <<- c(.lec.Random.seed.table$name, stream$name)

        .lec.CurrentStream(stream$name)
    }
    invisible(mpi.apply(states, initRNGstreamNode,comm=comm))
}

mpi.setup.sprng <- function (seed = runif(1, 1, 2^31-1),
               kindprng = "default", para = 0, comm=1) {
    commsize <- mpi.comm.size(comm)
    if (commsize < 3)
        stop("There is no slave or only one slave")
    if (!mpi.is.master())
        stop("Can be run only on master")

    if (!is.na (pmatch (kindprng, "default"))) 
            kindprng <- "LFG"  
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (is.na (kind)) {
        stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
    }

    if (sum(mpi.remote.exec(as.integer(require(rsprng)),comm=comm))
            < commsize-1)
        stop("It seems rsprng is not installed properly on slave machines.")
    assign(".Sprng.seed", as.integer(c(seed, kind, para)), env=.GlobalEnv)
    #adapted from snow
    initSprngNode <- function (streamno, nstream, seed, kind, para) {
        .Call("r_init_sprng", as.integer(kind), as.integer(streamno), 
                as.integer(nstream), as.integer(seed), as.integer(para),
                PACKAGE = "rsprng")
        RNGkind("user")
    }

    invisible(mpi.apply(0:(commsize-2), initSprngNode, commsize-1, seed, kind, para))
}
