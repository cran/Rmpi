simple.pi <- function (n, comm=1) 
{
	#let slaves ready receive n
	mpi.bcast.send.cmd(n <- mpi.bcast.recv(0,comm=.comm), comm=comm)  
	
	#send n to all slaves
	mpi.bcast.send(n,comm=comm) 
	
	#let each slave find its own id(rank) and total number of slaves
	mpi.bcast.send.cmd(id <- mpi.comm.rank(.comm), comm=comm)
	mpi.bcast.send.cmd(nslaves <- mpi.comm.size(.comm)-1, comm=comm)

	#let each slave compute corresponding PI value
	mpi.bcast.send.cmd(mypi <- 
		4*sum(1/(1+((seq(id,n,nslaves)-.5)/n)^2))/n, comm=comm)
	#send computed values to master and sum together
	mpi.bcast.send.cmd(mpi.reduce(mypi,comm=.comm), comm=comm)

	#master must sum together as well so it adds 0 to it
	mpi.reduce(0,comm=comm)
}
