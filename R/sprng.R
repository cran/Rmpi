mpi.init.sprng <- function(seed = floor(runif(1, 1, 2147483648)),
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
