### Copyright (C) 2002 Hao Yu
mpi.probe <- function(source, tag, comm=1, status=0){
	.Call("mpi_probe", as.integer(source), as.integer(tag), 
			as.integer(comm), as.integer(status))
}

mpi.get.count <- function(type, status = 0){
	.Call("mpi_get_count",as.integer(status), as.integer(type))
}

mpi.get.sourcetag <- function(status=0){
	.Call("mpi_get_sourcetag", as.integer(status))
}

mpi.gather <- function(x, type, rdata, root=0,  comm=1){
    .Call("mpi_gather", x, as.integer(type), rdata, 
		as.integer(root), as.integer(comm))
}

mpi.scatter <- function(x, type, rdata, root=0,  comm=1){
    .Call("mpi_scatter", x, as.integer(type), rdata, 
		as.integer(root), as.integer(comm))
}

mpi.gatherv <- function(x, type, rdata, rcounts, root=0,  comm=1){
    .Call("mpi_gatherv", x, as.integer(type), rdata, as.integer(rcounts), 
		as.integer(root), as.integer(comm))
}

mpi.scatterv <- function(x, scounts, type, rdata, root=0, comm=1){
    .Call("mpi_scatterv", x, as.integer(scounts), as.integer(type), rdata, 
		as.integer(root), as.integer(comm))
}

mpi.allgather <- function(x, type, rdata, comm=1){
    .Call("mpi_allgather", x, as.integer(type), rdata, as.integer(comm))
}

mpi.allgatherv <- function(x, type, rdata, rcounts, comm=1){
    .Call("mpi_allgatherv", x, as.integer(type), rdata, 
	as.integer(rcounts), as.integer(comm))
}

mpi.bcast <- function (x, type, rank = 0, comm = 1) {
    .Call("mpi_bcast", x, as.integer(type), as.integer(rank), 
        as.integer(comm))
}

bin.nchar <- function(x){
	if (!is.character(x))
		stop("Must be a (binary) character")
	.Call("bin_nchar", x[1])
}

mpi.bcast.cmd <- function (cmd=NULL, rank=0, comm=1){
    if(mpi.comm.rank(comm) == rank){
        cmd <- deparse(substitute(cmd), width.cutoff=500)
	cmd <- paste(cmd, collapse="\"\"/")
	mpi.bcast(x=nchar(cmd), type=1, rank=rank, comm=comm)
	invisible(mpi.bcast(x=cmd, type=3, rank=rank, comm=comm))
    } 
    else {
    	charlen <- mpi.bcast(x=integer(1), type=1, rank=rank, comm=comm)
    	if (is.character(charlen))   #error
            parse(text="break")
    	else {
	    out <- mpi.bcast(x=.Call("mkstr", as.integer(charlen)), 
			type=3, rank=rank, comm=comm)
	    parse(text=unlist(strsplit(out,"\"\"/"))) 
    	}
    }
}

mpi.bcast.Robj <- function(obj=NULL, rank=0, comm=1){
    if (mpi.comm.rank(comm) == rank){
	tmp <- serialize(obj, NULL)
	mpi.bcast(as.integer(bin.nchar(tmp)), 1, rank, comm)
  	invisible(mpi.bcast(tmp, 3, rank, comm))
    }
    else {
	charlen <- mpi.bcast(integer(1), 1, rank, comm)
	unserialize(mpi.bcast(.Call("mkstr", as.integer(charlen)), 3, 
		rank, comm))
    }
}

mpi.bcast.Robj2slave <- function(obj, comm=1){
        objname <- deparse(substitute(obj),width.cutoff=500)
        obj <- list(objname=objname,obj=obj)
	mpi.bcast.cmd(cmd=.tmpRobj <- mpi.bcast.Robj(comm=.comm),
					rank=0, comm=comm)
	mpi.bcast.Robj(obj, rank=0, comm=comm)
	mpi.bcast.cmd(cmd=assign(.tmpRobj$objname,.tmpRobj$obj, 
			env = .GlobalEnv), rank=0, comm=comm)
	mpi.bcast.cmd(rm(.tmpRobj), rank=0, comm=comm) 
}

mpi.send <- function (x, type,  dest, tag, comm=1){
	.Call("mpi_send", x, as.integer(type), as.integer(dest), 
	as.integer(tag), as.integer(comm))
}

mpi.recv <- function (x, type, source, tag, comm=1, status=0){
	.Call("mpi_recv", x, as.integer(type), as.integer(source), 
	as.integer(tag), as.integer(comm), as.integer(status))
}

mpi.send.Robj <- function(obj, dest, tag, comm=1){
    mpi.send(x=serialize(obj,NULL), type=3, dest=dest, tag=tag, comm=comm)
}

mpi.recv.Robj <- function(source, tag, comm=1, status=0){
    mpi.probe(source, tag, comm, status)
    srctag <- mpi.get.sourcetag(status)
    charlen <- mpi.get.count(type=3, status)
    unserialize(mpi.recv(x=.Call("mkstr", as.integer(charlen)), type=3,
	srctag[1],srctag[2], comm, status))
}

mpi.reduce <- function(x, type=2, 
	op=c("sum","prod","max","min","maxloc","minloc"), dest=0, comm=1){
#	op <- switch(match.arg(op),sum=1,prod=2,max=3,min=4)
 	op <- pmatch(match.arg(op), 
		c("sum","prod","max","min","maxloc","minloc"))
	if (is.integer(x)){
	   if(type!=1)
		stop("data (integer) and type are not matched.")
	}
	else if (is.double(x)){
	   if(type!=2)
		stop("data (double) and type are not matched.")
	}
	else 
		stop("Not implemented.")

#      if (op==5||op==6){
#	        n <- length(x)
#   		x <- rep(x,rep(2,n))
#		x[seq(2, 2*n, 2)] <- mpi.comm.rank(comm)
#	}
		
	.Call("mpi_reduce", x, as.integer(type), as.integer(op), 
		as.integer(dest), as.integer(comm))
}

mpi.allreduce <- function(x,type=2,
	op=c("sum","prod","max","min"), comm=1){
#	op <- switch(match.arg(op),sum=1,prod=2,max=3,min=4)
	op <- pmatch(match.arg(op), c("sum","prod","max","min","maxloc","minloc"))
	if (is.integer(x)){
	   if(type!=1)
		stop("data (integer) and type are not matched.")
	}
	else if (is.double(x)){
	   if(type!=2)
		stop("data (double) and type are not matched.")
	}
	else 
		stop("Not implemented.")
	.Call("mpi_allreduce", x, as.integer(type), as.integer(op), 
		as.integer(comm))
}
