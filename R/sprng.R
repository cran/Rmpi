### Copyright (C) 2002 Hao Yu 
make.sprng.seed <- function(who=c("slave","master"), pos=1){       
    who <- match.arg(who)
    if (who=="slave") {
	if (mpi.is.master())
	    stop("make.sprng.seed(slave) is running on a master")
    	else 
	     assign(".Sprng.seed",.Call("r_make_sprng_seed"), pos=pos)
    }
    else {
	if (!mpi.is.master())
	     stop("make.sprng.seed(master) is running on a slave")
	else 
       	    assign(".Sprng.seed",.Call("r_make_sprng_seed"), pos=pos)
    }
    .Sprng.seed
}

get.sprng.seed <- function(who=c("slave","master")){
    if (exists(".Sprng.seed"))
   	get(".Sprng.seed")
    else 
	stop("No .Sprng.seed available")
}

init.sprng <- function(who=c("slave","master"),
                      	seed = make.sprng.seed(who=match.arg(who)),
                        sprngkind = c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) {
    who <- match.arg(who)
    sprngkind <-match.arg(sprngkind)
    kind <- pmatch (sprngkind, c("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (who=="slave") {
	if (mpi.is.master())
	    stop("init.sprng(slave) is running on a master")
        else 
		.Call ("r_init_sprng", as.integer (kind),
           		as.integer (mpi.comm.rank(0)), 
  	   		as.integer (mpi.comm.size(0)),
           		as.integer (seed))
    }
    else {
	if (!mpi.is.master())
	     stop("init.sprng(master) is running on a slave")
	else {
                .Call ("r_init_sprng", as.integer (kind),
                        as.integer (0),
                        as.integer (1),
                        as.integer (seed))
	}	
    }
    invisible(RNGkind("user"))	
}

pack.sprng <- function(){
	.Call("r_pack_sprng")
}

unpack.sprng <- function (packold=FALSE){
	if (!is.logical(packold))
		stop("Choose TRUE or FALSE to save or not save old stream")
	.Call("r_unpack_sprng", as.integer(packold))
}

printsprng <- function(){
	invisible(.Call("r_print_sprng"))
}

free.sprng <- function(pos=1){
	RNGkind("default")
	if (exists(".Sprng.seed"))
		rm(.Sprng.seed, pos=1)
	.Call("r_free_sprng")
}

spawn.sprng <- function(nspawned){
	.Call("r_spawn_sprng", as.integer(nspawned))
}

unpack.spawned.stream <- function(packold=TRUE){
        if (!is.logical(packold))
                stop("Choose TRUE or FALSE to save or not save old stream")
	.Call("r_unpack_spawned_sprng", as.integer(packold))
}

startnew.stream <- function(packold=TRUE){
	out <- spawn.sprng(1)
	if (out != 1)
		stop("Cannot spawn a new stream")
	unpack.spawned.stream(packold=packold)
}
