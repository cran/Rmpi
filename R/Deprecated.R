mpi.bcast.send <- function (x, rank=0, comm=1){
    print("use mpi.bcast or mpi.bcast.Robj")
}

mpi.bcast.send.cmd <- function (cmd, rank=0, comm=1, width.cutoff=500){
	invisible(mpi.bcast.cmd(cmd, rank=rank, comm=comm))
}

mpi.bcast.recv.cmd <- function (rank=0,comm=1){
	mpi.bcast.cmd(rank=rank,comm=comm)
}

type2data <- function(x){
	if (x[1]==1)
		out <-integer(x[2])
	else if (x[1]==2)
		out <- double(x[2])
	else if (x[1]==3)
		out <- .Call("mkstr", as.integer(x[2:3]))
	out
}		
		
mpi.bcast.recv <- function (rank=0,comm=1){
    print("use mpi.bcast or mpi.bcast.Robj")
}

mpi.bcast.send.Robj <- function(obj, rank=0, comm=1, width.cutoff=500){
	mpi.bcast.Robj(obj=obj, rank=rank, comm=comm)
}

mpi.bcast.recv.Robj <- function(rank=0, comm=1, envir=sys.parent()){
	mpi.bcast.Robj(rank=rank,comm=comm)
}
