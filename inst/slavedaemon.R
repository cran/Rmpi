library(Rmpi)
options(error=quote(assign(".mpi.err", TRUE, env = .GlobalEnv)))
.comm <- 1
.intercomm <- 2
mpi.comm.get.parent(.intercomm)
mpi.intercomm.merge(.intercomm,1,.comm)
mpi.comm.set.errhandler(.comm)
mpi.comm.disconnect(.intercomm)
init.sprng(who="slave")
mpi.hostinfo(.comm)
while (1) {
    try(eval(mpi.bcast.recv.cmd(0,.comm),envir=sys.parent()))
}
print("Done")
free.sprng()
mpi.barrier(.comm)
mpi.comm.disconnect(.comm)
mpi.barrier(0)
mpi.abort(0)
#mpi.exit()
