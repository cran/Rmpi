if (!library(Rmpi,logical.return = TRUE)){
    warning("Rmpi cannot be loaded")
    q(save = "no")
}
options(error=quote(assign(".mpi.err", TRUE, env = .GlobalEnv)))
.comm <- 1
.intercomm <- 2
mpi.comm.get.parent(.intercomm)
mpi.intercomm.merge(.intercomm,1,.comm)
mpi.comm.set.errhandler(.comm)
mpi.comm.disconnect(.intercomm)
mpi.hostinfo(.comm)
while (1) {
    try(eval(mpi.bcast.cmd(rank=0,comm=.comm),envir=sys.parent()))
}
print("Done")
mpi.barrier(.comm)
mpi.comm.disconnect(.comm)
mpi.barrier(0)
mpi.abort(0)
#mpi.quit()
