### Copyright (C) 2002 Hao Yu 
mpi.finalize <- function(){
    if(mpi.is.master())
        print("Exiting Rmpi. Rmpi cannot be used unless relaunching R.")
    .Call("mpi_finalize",PACKAGE = "Rmpi")
}

mpi.exit <- function(){
    if (mpi.is.master())
    	print("Detaching Rmpi. Rmpi cannot be used unless relaunching R.")
    .Call("mpi_finalize",PACKAGE = "Rmpi")
    detach(package:Rmpi)
}

mpi.quit <- function(save="no"){
    .Call("mpi_finalize",PACKAGE = "Rmpi")
    q(save=save,runLast=FALSE)
}

mpi.is.master <- function () 
{
    if (is.loaded("mpi_comm_get_parent"))
	as.logical(.Call("mpi_is_master",PACKAGE = "Rmpi"))
    else {
	if (mpi.comm.size(1)>0)
	    as.logical(mpi.comm.rank(1)==0)
	else
	    as.logical(mpi.comm.rank(0)==0)
    }
}

mpi.any.source <- function(){
    .Call("mpi_any_source",PACKAGE = "Rmpi")
}

mpi.any.tag <- function(){
    .Call("mpi_any_tag",PACKAGE = "Rmpi")
}

mpi.proc.null <- function(){
    .Call("mpi_proc_null",PACKAGE = "Rmpi")
}

string <- function(length){
    if (as.integer(length) < 1)
	stop("need positive length")

    .Call("mkstr",as.integer(length),PACKAGE = "Rmpi")
}

mpi.info.create <- function(info=0){
	.Call("mpi_info_create", as.integer(info),PACKAGE = "Rmpi")
}

mpi.info.set <- function(info=0, key, value){
    .Call("mpi_info_set", as.integer(info), as.character(key), 
	as.character(value),PACKAGE = "Rmpi")
}

mpi.info.get <- function(info=0, key, valuelen){
    .Call("mpi_info_get",as.integer(info), as.character(key), 
	as.integer(valulen), .Call("mkstr",as.integer(valuelen),
	PACKAGE = "Rmpi"),PACKAGE = "Rmpi")
}

mpi.info.free <- function(info=0){
	.Call("mpi_info_free", as.integer(info),PACKAGE = "Rmpi")
}

mpi.universe.size <- function(){
	if (!is.loaded("mpi_universe_size")) 
        stop("This function is not supported under MPI 1.2")
	.Call("mpi_universe_size",PACKAGE = "Rmpi")
}

mpi.get.processor.name <- function(short=TRUE){
    name <- .Call("mpi_get_processor_name",PACKAGE = "Rmpi")
    if (short)
	name <- unlist(strsplit(name, "\\."))[1]
    name
}

mpi.hostinfo <- function(comm=1){
    if (mpi.comm.size(comm)==0){
        err <-paste("It seems no members running on comm", comm)
        stop(err)
    }
    hostname <- mpi.get.processor.name() 
    rk <- mpi.comm.rank(comm=comm)
    size <- mpi.comm.size(comm=comm)
    cat("\tHost:",hostname,"\tRank(ID):",rk, "\tof Size:", size,
		"on comm", comm, "\n")
}

slave.hostinfo <- function(comm=1){
    if (!mpi.is.master())
	stop("cannot run slavehostinfo on slaves")
    size <- mpi.comm.size(comm)
    if (size==0){
        err <-paste("It seems no slaves running on comm", comm)
        stop(err)
    }
    if (size == 1)
	mpi.hostinfo(comm)
    else { 
    	master <-mpi.get.processor.name() 
    	slavehost <- unlist(mpi.remote.exec(mpi.get.processor.name(),comm=comm))
    	slavecomm <- as.integer(mpi.remote.exec(.comm,comm=comm))
    	ranks <- 1:(size-1)
    	commm <- paste(comm, ")",sep="")
    	if (size > 10){
		rank0 <- paste("master  (rank 0 , comm", commm)
      		ranks <- c(paste(ranks[1:9]," ",sep=""), ranks[10:(size-1)])
    	}
    	else
		rank0 <- paste("master (rank 0, comm", commm)
    	cat(rank0, "of size", size, "is running on:",master, "\n")
    	slavename <- paste("slave", ranks,sep="")
    	ranks <- paste("(rank ",ranks, ", comm ",slavecomm,")", sep="")
    	for (i in 1:(size-1)){
        	cat(slavename[i], ranks[i], "of size",size, 
		"is running on:",slavehost[i], "\n")
    	}
    }
}

getpid <- function(){
    .Call("pid",PACKAGE = "Rmpi")
}

lamhosts <- function(){
	hosts <- system("lamnodes C -c -n", TRUE)
	base <-character(0)
	for (host in hosts)
		base <- c(base, unlist(strsplit(host, "\\."))[1])
	nn <- 0:(length(hosts)-1)
        names(nn) <- base
	nn
}

mpi.spawn.Rslaves <- 
	function(Rscript=system.file("slavedaemon.R", package="Rmpi"),
	nslaves=mpi.universe.size(),
	root=0,
	intercomm=2,
	comm=1,
	hosts=NULL,
	needlog=TRUE,
	needsprng=TRUE) {
	if (!is.loaded("mpi_comm_spawn"))
	    stop("You cannot use MPI_Comm_spawn API")	
        if (mpi.comm.size(comm) > 0){
	     err <-paste("It seems there are some slaves running on comm ", comm)
	     stop(err)
	}
	tmp <- paste(getpid(), "+", comm, sep="")	
	if (needlog)
		arg <- c(Rscript, tmp, "needlog")
	else
		arg <- c(Rscript, tmp , "nolog")		
	if (!is.null(hosts)){
		hosts <- as.integer(hosts)
		if (any(is.na(hosts)))
		    stop("hosts argument contains non-integer object(s).")
		if (max(hosts) > mpi.universe.size() -1 ||min(hosts) < 0){
			tmp1 <- paste("hosts number should be within 0 to",
				mpi.universe.size()-1)
			stop(tmp1)
		}
		nslaves <- length(hosts)
		tmpfile <-paste(tmp, "appschema", sep="") 
		fileobj <- file(tmpfile,"w")
		cat("c", paste(hosts, collapse=","), sep="", file=fileobj)
		cat(" ", system.file("Rslaves.sh", package="Rmpi"), file=fileobj)
		cat(" ", paste(arg, collapse=" "), file=fileobj)
		close(fileobj)
		mpi.info.create(0)
		mpi.info.set(0,"file",tmpfile)
	}	
	realns<-mpi.comm.spawn(slave=system.file("Rslaves.sh",package="Rmpi"),
		slavearg=arg,
		nslaves=nslaves,
		info=0,
		root=root,
		intercomm=intercomm)
    if (!is.null(hosts)){
	mpi.info.free(0)
	unlink(tmpfile)
    }		
    if (realns==0)
	stop("It seems no single slave spawned.")
    if (mpi.intercomm.merge(intercomm,0,comm)) {
	mpi.comm.set.errhandler(comm)
	mpi.comm.disconnect(intercomm)
	slave.hostinfo(comm)
	if (needsprng) {
	   if (!suppressWarnings(require(rsprng,quietly=TRUE)))
		cat("rsprng package is not installed. Cannot use SPRNG.\n")
	   else {
		if (sum(mpi.remote.exec(as.integer(require(rsprng)),comm=comm))
			== mpi.comm.size(comm)-1){
		    mpi.bcast.cmd(mpi.init.sprng(comm=0))
		    #mpi.init.sprng()
		    cat("SPRNG has been initialized on all slaves.\n")
		}
		else
		    cat("It seems rsprng is not installed properly on slave machines.\n")
	   }
	}	
    }
    else
	stop("Fail to merge the comm for master and slaves.")
}	

mpi.remote.exec <- function(cmd, ...,  comm=1, ret=TRUE){
    if (mpi.comm.size(comm) < 2)
	stop("It seems no slaves running.")
    tag <- floor(runif(1,1,1000))
    scmd <- substitute(cmd)
    arg <-list(...)

    tag.ret <- c(tag, ret)
    mpi.bcast.cmd(mpi.remote.slave(), comm = comm)
    mpi.bcast(as.integer(tag.ret), type=1, comm=comm)
    mpi.bcast.Robj(list(scmd=scmd, arg=arg), comm=comm)

    if (ret){
    	size <- mpi.comm.size(comm) 
    	allcode <- mpi.allgather(integer(3), 1, integer(3*size), comm)
    	errsum <- sum(allcode[seq(1,3*size,3)])
    	if (errsum == size-1){
	    tmp <-paste("All slaves could not evaluate `", 
			deparse(substitute(cmd)), "'.", sep="")
	    stop(tmp)
    	}  
    	else if ( errsum > 0){
	    tmp <- paste("There are ",errsum,
			" slaves that could not evaluate `",
			deparse(substitute(cmd)), "'.", sep="")
	    stop(tmp)
    	}
	type <- allcode[seq(5,3*size,3)]
	len <- allcode[seq(6,3*size,3)]
	eqlen <- all(len==len[1])
	if (all(type==1)){
	    if (eqlen){
	    	out <- mpi.gather(integer(len[1]),1,integer(size*len[1]),0,comm)
	    	out <- out[(len[1]+1):(size*len[1])]
		dim(out) <- c(len[1], size-1)
		out <- data.frame(out)
	    }
	    else {
	      out1<-mpi.gatherv(integer(1),1,integer(1+sum(len)),c(1,len),0,comm)
	      	uplen <- cumsum(len)+1
	      	lowlen <-c(2, uplen[-(size-1)]+1)
    	    	out <- as.list(integer(size-1))
    	    	names(out) <- paste("slave",1:(size-1), sep="")
    	    	for (i in 1:(size-1))
		    out[[i]]<- out1[lowlen[i]:uplen[i]]
	    }
	}
	else if (all(type==2)){
	    if (eqlen){
                out <- mpi.gather(double(len[1]),2,double(size*len[1]),0,comm)
                out <- out[(len[1]+1):(size*len[1])]
                dim(out) <- c(len[1], size-1)
                out <- data.frame(out)
            }
	    else {
	      out1<-mpi.gatherv(double(1),2,double(1+sum(len)),c(1,len),0,comm)	
	      	uplen <- cumsum(len)+1
	      	lowlen <-c(2, uplen[-(size-1)]+1)
    	    	out <- as.list(integer(size-1))
    	    	names(out) <- paste("slave",1:(size-1), sep="")
    	    	for (i in 1:(size-1))
		    out[[i]]<- out1[lowlen[i]:uplen[i]]
	    }
	}
	else {
    	    out <- as.list(integer(size-1))
    	    names(out) <- paste("slave",1:(size-1), sep="")
    	    for (i in 1:(size-1)){
		tmp<- mpi.recv.Robj(mpi.any.source(),tag,comm)
		src <- mpi.get.sourcetag()[1] 
		out[[src]]<- tmp 
	    }
	}
    	out
    }
}

typeindex <- function (x) {
    if(class(x)=="integer")
            as.integer(c(1,length(x)))
    else if (class(x)=="numeric")
            as.integer(c(2,length(x)))
    else
            as.integer(-1)
}

mpi.remote.slave <- function(){
    assign(".mpi.err", FALSE,  env = .GlobalEnv)
    tag.ret <- mpi.bcast(integer(2), type=1, comm=.comm)
    tag <- tag.ret[1]
    ret <- as.logical(tag.ret[2])
    scmd.arg <- mpi.bcast.Robj(comm=.comm)

    if (ret){
	size <- mpi.comm.size(.comm)
   	myerrcode <- as.integer(0)
	if (length(scmd.arg$arg)>0)
            out <- try(do.call(as.character(scmd.arg$scmd), scmd.arg$arg),TRUE)
        else
            out <- try(eval(scmd.arg$scmd), TRUE)
    
    	if (.mpi.err){
	    print(geterrmessage())
	    myerrcode <-as.integer(1)
    	    type <- integer(2)
	}
    	else {
	    type <- typeindex(out)
	    if (is.na(type[2]))
 	        type[2] <- 0	
    	}
      	allcode <- mpi.allgather(c(myerrcode,type), 1, integer(3*size), .comm)
        if (sum(allcode[seq(1,3*size,3)]) > 0) {
	    stop("See above error message(s)")
	}
	type <- allcode[seq(5,3*size,3)]
        len <- allcode[seq(6,3*size,3)]
        eqlen <- all(len==len[1])
        if (all(type==1)) {
            if (eqlen)
                mpi.gather(out, 1, integer(1), 0, .comm)
	    else
		mpi.gatherv(out, 1, integer(1), integer(1), 0 ,.comm)
	}
	else if (all(type==2)) {
            if (eqlen)
                mpi.gather(out, 2, double(1), 0, .comm)
	    else
                mpi.gatherv(out, 2, double(1), integer(1), 0, .comm)
        }
	else {
	    mpi.send.Robj(out,0,tag,.comm)
	}		
    }
    else {
	if (length(scmd.arg$arg)>0)
            out <- try(do.call(as.character(scmd.arg$scmd), scmd.arg$arg))
        else
            out <- try(eval(scmd.arg$scmd))  
    }
}

mpi.close.Rslaves <- function(dellog=TRUE, comm=1){
    if (mpi.comm.size(comm) < 2){
	err <-paste("It seems no slaves running on comm", comm)
	stop(err)
    }
    mpi.bcast.cmd(break, rank=0, comm=comm)
    if (dellog){
	tmp <- paste(getpid(),"+",comm,sep="")	
  	logfile <- paste("*.",tmp,".*.log", sep="")
	if (length(system(paste("ls", 
		logfile),TRUE,ignore.stderr=TRUE))>=1)
	    system(paste("rm", logfile))
	}
#     mpi.barrier(comm)
    if (comm >0){
        if (is.loaded("mpi_comm_disconnect"))
            mpi.comm.disconnect(comm) 
        else
            mpi.comm.free(comm)
    }
#   mpi.comm.set.errhandler(0)
}

tailslave.log <- function(nlines=3,comm=1){
    if (mpi.comm.size(comm)==0)
	stop ("It seems no slaves running")
    tmp <- paste(getpid(),"+",comm,sep="")	
    logfile <- paste("*.",tmp,".*.log", sep="")
    if (length(system(paste("ls", logfile),TRUE,ignore.stderr=TRUE))==0)
	stop("It seems no slave log files.")
    system(paste("tail -",nlines," ", logfile,sep=""))
}

mpi.parallel.sim <- function(n=100,rand.gen=rnorm, rand.arg=NULL, 
			statistic, nsim=100, run=1, slaveinfo=TRUE, 
			comm=1, ...){

    if (!is.function(rand.gen))
	stop("rand.gen is not a function")
    if (!is.function(statistic))
	stop("statistic is not a function")

    if (!is.null(rand.arg))
	if (!is.list(rand.arg))
	    stop("rand.arg is not a list")

    if (mpi.comm.size(comm) < 2)
        stop("It seems no slaves running.")

    wrap.rand.gen <- mpi.wrap.fun(rand.gen,rand.arg,"tmp.rand.gen")
    wrap.statistic <- mpi.wrap.fun(statistic,list(...),"tmp.statistic")

    mpi.bcast.cmd(mpi.parallel.slave())
    
    mpi.bcast.Robj(rand.gen, comm=comm)
    mpi.bcast.Robj(wrap.rand.gen, comm=comm)
        
    mpi.bcast.Robj(statistic, comm=comm)
    mpi.bcast.Robj(wrap.statistic,comm=comm)

    nnr <- c(n,nsim,run)
    mpi.bcast(as.integer(nnr),type=1, comm=comm)

    slave.num <- mpi.comm.size(comm)-1
    i <- 0
    anysrc <- mpi.any.source()
    anytag <- mpi.any.tag()
	
    stat <- integer(slave.num)
    result <- numeric()

    while (i < slave.num*run){
   	i <- i+1
  	output <- mpi.recv.Robj(source=anysrc, tag=8, comm=comm)

	src <- mpi.get.sourcetag()[1]
        mpi.send(as.integer(i), type=1, dest=src, tag=88, comm=comm)
      	result <- cbind(result,output)
  	stat[src] <- stat[src]+1
    }
    if (slaveinfo){
	slavename <- paste("slave",1:slave.num, sep="")
	cat("Finished slave jobs summary:\n")
	for (i in 1:slave.num){
            if (i < 10)
	    	cat(slavename[i], " finished",stat[i], "job(s)\n")
	    else
	    	cat(slavename[i], "finished",stat[i], "job(s)\n")
	}
    }
    if(length(result)==slave.num*run*nsim)
	result <- as.vector(result)	
    result
}

mpi.parallel.slave <- function(){
    assign("tmp.rand.gen", mpi.bcast.Robj(comm=.comm),		
		envir=.GlobalEnv)
    assign("wrap.rand.gen", mpi.bcast.Robj(comm=.comm))

    assign("tmp.statistic", mpi.bcast.Robj(comm=.comm),
		envir=.GlobalEnv)
    assign("wrap.statistic", mpi.bcast.Robj(comm=.comm))

    nnr <- mpi.bcast(integer(3), type=1, comm=.comm)
    n <- nnr[1];  nsim <- nnr[2];  run <- nnr[3]

    i <- 0
    slave.num <- mpi.comm.size(.comm)-1
    
    while( i < slave.num*(run-1)+1){
	out <- replicate(nsim, wrap.statistic(wrap.rand.gen(n)))
	mpi.send.Robj(obj=out, dest=0, tag=8, comm=.comm)
	i <- mpi.recv(integer(1), type=1, source=0, tag=88, comm=.comm)
    }

    rm(tmp.rand.gen,envir=.GlobalEnv)
    rm(tmp.statistic,envir=.GlobalEnv)
}

mpi.wrap.fun <- function(cmd, arg, 
			cmd.name=as.character(substitute(cmd))){
   org.arg <- formals(cmd)
   if (length(org.arg)==0|length(arg)==0)
	return(cmd)
   arb.arg <- match("...", names(org.arg))
   if (!is.na(arb.arg))
 	org.arg <- org.arg[-arb.arg]
   org.arg.names <- names(org.arg)
   match.arg <- match(names(arg), org.arg.names)
   if(is.na(arb.arg)){
   	if (!any(is.na(match.arg))){
	   for (i in  1:length(match.arg))
		org.arg[[match.arg[i]]] <- arg[[i]]
   	   formals(cmd)<-org.arg
   	   return(cmd)
	}
	else
	    stop("Wrong argment(s) for the function", cmd.name)
    }
   else {
    	arg1 <- match.arg[!is.na(match.arg)]
    	arg2 <- which(is.na(match.arg))
      	for (i in seq(1,length(arg1),length=length(arg1)))
            org.arg[[arg1[i]]] <- arg[[i]]
   	if (length(arg2)>0)
	    org.arg <- c(org.arg, arg[arg2])

        fun <- function() {
           arg.names <- names(formals())
    	   arg.names <- arg.names[-length(arg.names)]
	   arg <-  mget(arg.names,envir=as.environment(-1))
	   do.call(cmd.name, arg)
        }
	formals(fun)<-c(org.arg,cmd.name=cmd.name)
   	return(fun)
   }
}

mpi.sendrecv <-  function(senddata, sendtype, dest, sendtag, recvdata, 
			recvtype, source, recvtag, 
         		comm = 1, status = 0) 
 {
   .Call("mpi_sendrecv", senddata, as.integer(sendtype), 
	  as.integer(dest), 
          as.integer(sendtag), recvdata, as.integer(recvtype), 
          as.integer(source), as.integer(recvtag), as.integer(comm),
          as.integer(status), PACKAGE="Rmpi")
}

mpi.sendrecv.replace <- function(x, type, dest, sendtag, source, recvtag,  
         comm = 1, status = 0)
 {
   .Call("mpi_sendrecv_replace", x, as.integer(type), as.integer(dest),
          as.integer(sendtag), as.integer(source), as.integer(recvtag), 
          as.integer(comm), as.integer(status), PACKAGE="Rmpi")
}

