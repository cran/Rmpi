### Copyright (C) 2002 Hao Yu 
mpi.barrier <- function(comm=1){
	.Call("mpi_barrier", as.integer(comm))
}

mpi.comm.set.errhandler <- function(comm=1){
	.Call("mpi_comm_set_errhandler", as.integer(comm))
}

mpi.comm.test.inter <- function(comm=2){
 	if(mpi.comm.is.null(comm))
		stop("NULL communicator")
	.Call("mpi_comm_test_inter",as.integer(comm))
}

mpi.comm.rank <- function(comm=1){
	.Call("mpi_comm_rank", as.integer(comm))
}

mpi.comm.size <- function(comm=1){
	if (.Call("mpi_comm_is_null", as.integer(comm))==1)
		0
	else 
		.Call("mpi_comm_size",as.integer(comm))
}

mpi.comm.dup <- function(comm, newcomm){
        .Call("mpi_comm_dup", as.integer(comm), as.integer(newcomm))
}

mpi.comm.remote.size <- function(comm=2){
	.Call("mpi_comm_remote_size", as.integer(comm))
}

mpi.comm.free <- function(comm=1){
    if (mpi.comm.size(comm)==0){
	tmp<-paste("It seems no members(slaves) associated with comm", comm)
	stop(tmp)
     }
     .Call("mpi_comm_free",as.integer(comm))
}

mpi.abort <- function(comm=1){
    if (mpi.comm.size(comm)==0){
	tmp<-paste("It seems no members(slaves) associated with comm", comm)
	stop(tmp)
     }
     .Call("mpi_abort",as.integer(comm))
}

mpi.comm.disconnect <- function(comm=1){
    if (mpi.comm.size(comm)==0){
	tmp<-paste("It seems no members(slaves) associated with comm", comm)
	stop(tmp)
     }
     .Call("mpi_comm_disconnect",as.integer(comm))
}

mpi.comm.spawn <- function(slave, 
			slavearg=character(0), 
			nslaves=mpi.universe.size(),
			info=0,
			root=0, 
			intercomm=2){
        if (!is.loaded("mpi_comm_spawn"))
            stop("You cannot use MPI_Comm_spawn API")

	if (!is.character(slave))
		stop("character argument (slave) expected")
	if (nslaves > mpi.universe.size()){
                tmp <- paste("Number of R slaves is over",
                        mpi.universe.size(),": maximum CPUs.")
                warning(tmp)
        }
	else if (nslaves <= 0)
		stop("Choose a positive number of slaves.")
	.Call("mpi_comm_spawn",
                as.character(slave),
                as.character(slavearg),
                as.integer(nslaves),
		as.integer(info),
		as.integer(root),
		as.integer(intercomm))
}

mpi.comm.get.parent <- function(comm=2){
	.Call("mpi_comm_get_parent", as.integer(comm))
}

mpi.comm.is.null <- function(comm){
	as.logical(.Call("mpi_comm_is_null", as.integer(comm)))
}

mpi.intercomm.merge <- function(intercomm=2,high=0,comm=1){
	.Call("mpi_intercomm_merge", as.integer(intercomm),
				     as.integer(high),
				     as.integer(comm))
}

#mpi.realloc.comm <- function(maxsize=10){
#	.Call("mpi_realloc_comm", as.integer(maxsize))
#}
