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

typeindex <- function (x) {
	if (is.integer(x))
        	as.integer(c(1,length(x)))
	else if (is.double(x))
        	as.integer(c(2,length(x)))
	else if (is.character(x))
        	as.integer(c(3,length(x),max(nchar(x))))
	else
        	as.integer(-1)
}

mpi.bcast.send <- function (x, rank=0, comm=1){
        type <- typeindex(x)
            if(type[1]==-1)
                stop("Not implemented yet.")
        mpi.bcast(x=type, type=1, rank=rank, comm=comm)
	invisible(mpi.bcast(x=x, type=type[1], rank=rank, comm=comm))
}

mpi.bcast.send.cmd <- function (cmd, rank=0, comm=1, width.cutoff=500){
        cmd <- deparse(substitute(cmd), width.cutoff=width.cutoff)
	#cmd <- paste(cmd, collapse=" ")
	type <- typeindex(cmd) #must be characters.
	mpi.bcast(x=type, type=1, rank=rank, comm=comm)
	invisible(mpi.bcast(x=cmd, type=type[1], rank=rank, comm=comm))
}

mpi.bcast.recv.cmd <- function (rank=0,comm=1){
    type <- integer(3)
    out <- mpi.bcast(x=type, type=1, rank=rank, comm=comm)
    if (is.character(out))
	parse(text="break")
    else {
	cmd <- type2data(out)
	parse(text=mpi.bcast(x=cmd, type=out[1], rank=rank, comm=comm))
    }
}

type2data <- function(x){
	if (x[1]==1)
		out <-integer(x[2])
	else if (x[1]==2)
		out <- double(x[2])
	else if (x[1]==3)
	  #   out <- rep(paste(rep(" ", x[3]), collapse=""),x[2])
	 # out <- c(paste(rep(" ", x[3]), collapse=""),character(x[2]-1))
		out <- .Call("mkstr", as.integer(x[2:3]))
	out
}		
		
mpi.bcast.recv <- function (rank=0,comm=1){
	type <- integer(3)
	out <- mpi.bcast(x=type,type=1, rank=rank, comm=comm)
	x <- type2data(out)
	mpi.bcast(x=x, type=out[1], rank=rank, comm=comm)
}

mpi.bcast.send.Robj <- function(obj, rank=0, comm=1, width.cutoff=500){
	out <- deparse(obj, width.cutoff=width.cutoff)
	out <- paste(out, collapse="\"\"/")
	mpi.bcast.send(x=out, rank=rank, comm=comm)
}

mpi.bcast.recv.Robj <- function(rank=0, comm=1, envir=sys.parent()){
	out <- mpi.bcast.recv(rank=rank, comm=comm)
	out <- unlist(strsplit(out,"\"\"/")) 
	eval(parse(text=out), envir=envir)
}

mpi.bcast.Robj2slave <- function(obj, comm=1, width.cutoff=500){
        objname <- deparse(substitute(obj),width.cutoff=width.cutoff)
        obj <- list(objname=objname,obj=obj)
	mpi.bcast.send.cmd(cmd=tmpRobj <- mpi.bcast.recv.Robj(),
					rank=0, comm=comm)
	mpi.bcast.send.Robj(obj, rank=0, comm=comm, 
					width.cutoff=width.cutoff)
	mpi.bcast.send.cmd(cmd=assign(tmpRobj$objname,tmpRobj$obj, 
			env = .GlobalEnv), rank=0, comm=comm)
	mpi.bcast.send.cmd(rm(tmpRobj), rank=0, comm=comm) 
}

mpi.send <- function (x, type,  dest, tag, comm=1){
	.Call("mpi_send", x, as.integer(type), as.integer(dest), 
	as.integer(tag), as.integer(comm))
}

mpi.recv <- function (x, type, source, tag, comm=1, status=0){
	.Call("mpi_recv", x, as.integer(type), as.integer(source), 
	as.integer(tag), as.integer(comm), as.integer(status))
}

mpi.send.Robj <- function(obj, dest, tag, comm=1, width.cutoff=500){
	out <- deparse(obj, width.cutoff=width.cutoff)
	out <- paste(out, collapse="\"\"/")
        type <- typeindex(out) #must be characters
	mpi.send(x=type, type=1,  dest=dest, tag=tag, comm=comm)
	mpi.send(x=out, type=type[1], dest=dest, tag=tag, comm=comm)
}

mpi.recv.Robj <- function(source, tag, comm=1, 
			status=0, envir=sys.parent()){
	type <- integer(3)
	out <- mpi.recv(type, type=1, source=source, tag=tag, comm=comm, 
		status=status)
	obj <- mpi.recv(type2data(out), type=out[1], source=source, 
		tag=tag, comm=comm, status=status)
	obj <- unlist(strsplit(obj,"\"\"/"))
	eval(parse(text=obj), envir=envir)
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
