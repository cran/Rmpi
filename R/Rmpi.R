### Copyright (C) 2002 Hao Yu 
mpi.finalize <- function(){
    if(mpi.is.master())
        print("Exiting Rmpi. Rmpi cannot be used unless relaunching R.")
    .Call("mpi_finalize")
}

mpi.exit <- function(){
    if (mpi.is.master())
    	print("Detaching Rmpi. Rmpi cannot be used unless relaunching R.")
    .Call("mpi_finalize")
    detach(package:Rmpi)
}

mpi.quit <- function(save="no"){
    .Call("mpi_finalize")
    q(save=save,runLast=FALSE)
}

mpi.is.master <- function () 
{
    if (is.loaded("mpi_comm_get_parent"))
	as.logical(.Call("mpi_is_master"))
    else {
	if (mpi.comm.size(1)>0)
	    as.logical(mpi.comm.rank(1)==0)
	else
	    as.logical(mpi.comm.rank(0)==0)
    }
}

mpi.any.source <- function(){
    .Call("mpi_any_source")
}

mpi.any.tag <- function(){
    .Call("mpi_any_tag")
}

mpi.proc.null <- function(){
    .Call("mpi_proc_null")
}

mpi.info.create <- function(info=0){
	.Call("mpi_info_create", as.integer(info))
}

mpi.info.set <- function(info=0, key, value){
    .Call("mpi_info_set", as.integer(info), as.character(key), 
	as.character(value))
}

mpi.info.get <- function(info=0, key, valuelen){
    .Call("mpi_info_get",as.integer(info), as.character(key), 
	as.integer(valulen), .Call("mkstr",as.integer(valuelen)))
}

mpi.info.free <- function(info=0){
	.Call("mpi_info_free", as.integer(info))
}

mpi.universe.size <- function(){
	.Call("mpi_universe_size")
}

mpi.get.processor.name <- function(short=TRUE){
    name <- .Call("mpi_get_processor_name")
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
    .Call("pid")
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
#	mergecomm=TRUE,
	needlog=TRUE) {	
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
		if (is.na(hosts))
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
#    if(mergecomm){
	if(mpi.intercomm.merge(intercomm,0,comm)) {
#	    tmp <- paste("The comm number for master and slaves is", comm)
	    mpi.comm.set.errhandler(comm)
	    mpi.comm.disconnect(intercomm)
	    slave.hostinfo(comm)	
#	    print(tmp)
    	}
    	else
	    stop("Fail to merge the comm for master and slaves.")
#    }
}	

mpi.remote.exec <- function(cmd, ...,  comm=1, ret=TRUE){
	#	retobj=c("auto","list")
    tag <- floor(runif(1,1,1000))
    scmd <- substitute(cmd)
    arg <-list(...)
  #  retobj <- match.arg(retobj)
    if (length(arg)>0){
	scmd <- mpi.remote.fun(scmd, ..., needsub=FALSE, width.cutoff=500)
        scmd1 <- paste("mpi.remote.slave(cmd=",scmd,",",sep="")
        scmd2 <- deparse(substitute(ekaf(tag=tag,ret=ret)),   
                width.cutoff=500)
        scmd2 <-unlist(strsplit(scmd2,"ekaf\\("))[-1]
        scmd <- paste(scmd1,scmd2)
    }
    else {
        scmd <- deparse(substitute(mpi.remote.slave(cmd=scmd, tag=tag, 
		ret=ret)), width.cutoff = 500)
    }
    scmd <- paste(scmd, collapse="\"\"/")
    mpi.bcast(x=nchar(scmd), type=1, rank=0, comm=comm)
    mpi.bcast(x=scmd, type=3, rank=0, comm=comm)
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
    if(is.null(class(x))){ 
        if (is.integer(x))
            as.integer(c(1,length(x)))
        else if (is.double(x))
            as.integer(c(2,length(x)))
        else
            as.integer(-1)
    }
    else
	as.integer(-1)
}

mpi.remote.slave <- function(cmd,tag,ret){
    assign(".mpi.err", FALSE,  env = .GlobalEnv)
    if (ret){
	size <- mpi.comm.size(.comm)
   	myerrcode <- as.integer(0)
    	out <- try(eval(expression(cmd)))
    	if (.mpi.err){
	    print(.Last.value)        #Leave real error messages in log file
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
    else
	try(eval(expression(cmd)))
}

mpi.remote.fun <- function (cmd, ..., needsub=TRUE, width.cutoff=500) {
    if (needsub)
    	cmd <- substitute(cmd)
    cmd <- paste(cmd, "(", sep="")
    arg <- list(...)
    if (length(arg)==0)
	stop("missing argument(s)")
    argname <- names(arg)
    argn <- length(arg)
    for (i in 1:argn){
	arg[[i]] <- deparse(arg[[i]], width.cutoff= width.cutoff)
	if (length(arg[[i]]) >1)
	   stop("one of arguments is too long")
    }
    if (is.null(argname)){
        for (i in seq(1, argn-1, length=argn-1))
            cmd <- paste(cmd, arg[[i]], ",", sep="")
        cmd <- paste(cmd, arg[[argn]], ")", sep="")
    }
    else {
        for (i in seq(1, argn-1, length=argn-1)){
            if (argname[i]=="")
                cmd <- paste(cmd, arg[[i]], ",", sep="")
            else
                cmd <- paste(cmd, argname[i],"=", arg[[i]], ",", sep="")
        }
        if (argname[argn]=="")
            cmd <- paste(cmd, arg[[argn]], ")", sep="")
        else
            cmd <- paste(cmd, argname[argn],"=", arg[[argn]], ")", sep="")
   }
   cmd
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
	if (length(system(paste("ls", logfile),TRUE,ignore.stderr=TRUE))>1)
	    system(paste("rm", logfile))
	}
     mpi.barrier(comm)
    if (comm >0)
	 mpi.comm.disconnect(comm)
#   mpi.comm.set.errhandler(0)
}

tail.slave.log <- function(nlines=3,comm=1){
    if (mpi.comm.size(comm)==0)
	stop ("It seems no slaves running")
    tmp <- paste(getpid(),"+",comm,sep="")	
    logfile <- paste("*.",tmp,".*.log", sep="")
    if (length(system(paste("ls", logfile),TRUE,ignore.stderr=TRUE))==0)
	stop("It seems no slave log files.")
    system(paste("tail -",nlines," ", logfile,sep=""))
}
