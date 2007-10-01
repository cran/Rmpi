### Copyright (C) 2002 Hao Yu 
.First.lib <- function (lib, pkg) {
    #mylib <- dirname(.path.package("Rmpi"))
    #ver <- packageDescription("Rmpi", lib = mylib)["Version"]
    #vertxt <- paste("\tRmpi version:", ver, "\n")
    #if(interactive() || getOption("verbose"))
    #    cat(vertxt)
		
    # Check if lam-mpi is running
    if (.Platform$OS=="unix"){
    	if (length(system("lamnodes",TRUE,ignore.stderr = TRUE)) == 0){
    		#cat("\n\tLAM/MPI runtime environment is not operating.\n")
    		#cat("\tStarting LAM/MPI runtime environment.\n")
	    	system("lamboot -H",ignore.stderr = TRUE)
		}
    }
	
    library.dynam("Rmpi", pkg, lib)
    if (!TRUE)
	stop("Fail to load Rmpi dynamic library.")
    if (!is.loaded("mpi_initialize"))
	stop("Probably Rmpi has been detached. Please quit R.")
    if(!.Call("mpi_initialize",PACKAGE = "Rmpi"))
	stop("Cannot start MPI_Init(). Exit")
    if (exists(".Random.seed") && 
	round(.Random.seed[1]-5,-1) == .Random.seed[1]-5) {
        rm(.Random.seed, envir=.GlobalEnv)
    }	
}

.Last.lib <- function(libpath){
    dyn.unload(file.path(libpath, "libs",
	paste("Rmpi", .Platform$"dynlib.ext", sep="")))
}
