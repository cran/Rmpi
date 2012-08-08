mpi.setup.rngstream <- function(seed=c(runif(3,0,2^32-210),runif(3,0,2^32-22854)), comm = 1){
    if (!require(rlecuyer, quietly=TRUE)) 
        stop("rlecuyer cannot be loaded.")

    commsize <- mpi.comm.size(comm)
    if (commsize < 3)
        stop("There is no slave or only one slave")
    if (!mpi.is.master())
        stop("Can be run only on master")
    .lec.init()
    .lec.SetPackageSeed(seed)
    names <- as.character(0:(commsize-1))
    .lec.CreateStream(names)
    states <- lapply(names, .lec.GetStateList)
    
    if (sum(mpi.remote.exec(as.integer(require(rlecuyer, quietly=TRUE)),comm=comm))
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
	initRNGstreamNode(states[[1]])
    invisible(mpi.apply(states[-1], initRNGstreamNode,comm=comm))
}