if (!library(Rmpi,logical.return = TRUE)){
    warning("Rmpi cannot be loaded")
    q(save = "no")
}
options(error=quote(assign(".mpi.err", TRUE, env = .GlobalEnv)))
.comm <- 1
.intercomm <- 2
invisible(mpi.comm.get.parent(.intercomm))
invisible(mpi.intercomm.merge(.intercomm,1,.comm))
invisible(mpi.comm.set.errhandler(.comm))
mpi.hostinfo(.comm)
invisible(mpi.comm.disconnect(.intercomm))
repeat 
    try(eval(mpi.bcast.cmd(rank=0,comm=.comm),envir=sys.parent()),TRUE)
print("Done")
invisible(mpi.comm.disconnect(.comm))
invisible(mpi.comm.set.errhandler(0))
mpi.quit()
