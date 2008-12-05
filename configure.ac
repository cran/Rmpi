 Process this file with autoconf to produce a configure script.

AC_INIT(DESCRIPTION)

AC_PROG_CC

AC_ARG_WITH(mpi,
[   --with-mpi=/opt/lib32/usr/lib          Location of MPI library.],
[   if test "${withval}" != no; then
        MPI_ROOT=${withval}
        MPI_INCLUDE="-I${MPI_ROOT}/include"
        LDFLAGS="$LDFLAGS -L${withval}/lib"
        CFLAGS="$CFLAGS -I${withval}/include"
    fi  ]
)

MPITYPE="UNKNOWN"

if [ [ "$MPI_ROOT" = "" ] ]; then
  for d in /opt/lib /usr/lib /usr /usr/local/lib /usr/local; do
    if test -f $d/include/mpi.h && test -d $d/lib/openmpi; then
      echo "I am here $d and it is OpenMPI"
      MPI_ROOT=$d
  	  MPITYPE="OPENMPI"
      break
    elif test -f $d/include/openmpi/mpi.h && test -d $d/lib/openmpi; then
      echo "I am here $d and it is OpenMPI"
      MPI_ROOT=$d
  	  MPITYPE="OPENMPI"
      break
    elif test -f $d/include/openmpi/mpi.h && test -d $d/lib64/openmpi; then
      echo "I am here $d and it is OpenMPI"
      MPI_ROOT=$d
  	  MPITYPE="OPENMPI"
      break
    elif test -f $d/openmpi/include/mpi.h && test -d $d/openmpi/lib; then
      echo "I am here $d/openmpi and it is OpenMPI"
      MPI_ROOT=$d/openmpi
          MPITYPE="OPENMPI"
      break
   elif test -f $d/lam/include/mpi.h && test -f $d/lam/lib/libmpi.so; then
      echo "I am here $d/lam and it is LAM"
      MPI_ROOT=$d/lam
	  MPITYPE="LAM"
      break
	elif test -f $d/include/lam/mpi.h && test -f $d/lib/lam/libmpi.so; then
	  echo "I am here $d and it is LAM"
	  MPI_ROOT=$d
	  MPITYPE="LAM"
	  break
	elif test -f $d/include/lam/mpi.h && test -f $d/lib64/lam/libmpi.so; then
	  echo "I am here $d and it is LAM"
	  MPI_ROOT=$d
	  MPITYPE="LAM"
	  break
    elif test -f $d/mpich/include/mpi.h; then
      echo "I am here $d/mpich and it is MPICH"
      MPI_ROOT=$d/mpich
	  MPITYPE="MPICH"
      break
    elif test -f $d/mpi/include/mpi.h; then
      echo "I am here $d/mpi"
      MPI_ROOT=$d/mpi
      break
    elif test -f $d/include/mpi.h; then
      echo "I am here $d"
      MPI_ROOT=$d
      break
    fi
  done
fi

if test -n "$MPI_ROOT"; then
	#guess MPITYPE if it is unknown and user supplies MPI_ROOT
	if [ [ $MPITYPE = "UNKNOWN" ]]; then
		if test -d ${MPI_ROOT}/lib/openmpi; then
			MPITYPE="OPENMPI"
		elif test -d ${MPI_ROOT}/lib64/openmpi; then
		 	MPITYPE="OPENMPI"
		elif test -f ${MPI_ROOT}/lib/liblam.so; then
		 	MPITYPE="LAM"
		elif test -d ${MPI_ROOT}/lib/lam; then
		 	MPITYPE="LAM"
		elif test -d ${MPI_ROOT}/lib64/lam; then
		 	MPITYPE="LAM"
		elif test -f ${MPI_ROOT}/lib/libmpich.a; then
		 	MPITYPE="MPICH"
		elif test -f ${MPI_ROOT}/lib64/libmpich.a; then
		 	MPITYPE="MPICH"
		fi
	fi
    echo "Try to find mpi.h ..."
    if test -f ${MPI_ROOT}/include/mpi.h; then
         echo "Found in ${MPI_ROOT}/include"
         MPI_INCLUDE="-I${MPI_ROOT}/include"		  
    elif test -f ${MPI_ROOT}/include/openmpi/mpi.h; then
         echo "Found in ${MPI_ROOT}/include/openmpi"
         MPI_INCLUDE="-I${MPI_ROOT}/include/openmpi"
    elif test -f ${MPI_ROOT}/include/lam/mpi.h; then
         echo "Found in ${MPI_ROOT}/include/lam"
         MPI_INCLUDE="-I${MPI_ROOT}/include/lam"
    else 
         echo "Cannot find mpi head file"
         echo "Please use --with-mpi=/path/to/mpi or specify the environment variable MPI_ROOT"
         exit 1
    fi
else
    AC_CHECK_HEADER(mpi.h)
fi 

echo "Try to find libmpi.so or libmpich.a"
if test -f ${MPI_ROOT}/lib/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib"
        MPI_LIBS="-L${MPI_ROOT}/lib -lmpi"
elif test -f ${MPI_ROOT}/lib/openmpi/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib/openmpi"
        MPI_LIBS="-L${MPI_ROOT}/lib/openmpi -lmpi"
elif test -f ${MPI_ROOT}/lib64/openmpi/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib64/openmpi"
        MPI_LIBS="-L${MPI_ROOT}/lib64/openmpi -lmpi"
elif test -f ${MPI_ROOT}/lib/lam/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib/lam"
        MPI_LIBS="-L${MPI_ROOT}/lib/lam -lmpi"
	MPI_INCLUDE="${MPI_INCLUDE} ${MPI_INCLUDE}/32"
elif test -f ${MPI_ROOT}/lib64/lam/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib64/lam"
        MPI_LIBS="-L${MPI_ROOT}/lib64/lam -lmpi"
	MPI_INCLUDE="${MPI_INCLUDE} ${MPI_INCLUDE}/64"
elif test -f ${MPI_ROOT}/lib/libmpich.a; then
        echo "Found libmpich in ${MPI_ROOT}/lib"
        MPI_LIBS="-L${MPI_ROOT}/lib -lmpich"
	AC_CHECK_DECLS([MPICH2, MPICH2],[  MPI_DEPS="-DMPI2" ],, 
		[#include <mpi.h>])
elif test -f ${MPI_ROOT}/lib64/libmpich.a; then
        echo "Found libmpich in ${MPI_ROOT}/lib64"
        MPI_LIBS="-L${MPI_ROOT}/lib64 -lmpich"
	AC_CHECK_DECLS([MPICH2, MPICH2],[  MPI_DEPS="-DMPI2" ],, 
		[#include <mpi.h>])
else
    AC_CHECK_LIB(mpi, main,
    	[   MPI_LIBS="-L${MPI_ROOT}/lib -lmpi"  ],
    	[   echo "libmpi not found. exiting..."
        	exit 1  ]
)
fi    

if [ [ $MPITYPE = "OPENMPI" ] ]; then
	MPI_DEPS="-DMPI2"
elif [ [ $MPITYPE = "LAM" ] ]; then
	MPI_DEPS="-DMPI2"
	echo "Try to find liblam.so ..."
	if test -f ${MPI_ROOT}/lib/liblam.so; then
        echo "Found liblam in ${MPI_ROOT}/lib"
        MPI_LIBS="${MPI_LIBS} -llam"
	elif test -f ${MPI_ROOT}/lib/lam/liblam.so; then
        echo "Found liblam in ${MPI_ROOT}/lib/lam"
        MPI_LIBS="${MPI_LIBS} -llam"
	elif test -f ${MPI_ROOT}/lib64/lam/liblam.so; then
        echo "Found liblam in ${MPI_ROOT}/lib64/lam"
        MPI_LIBS="${MPI_LIBS} -llam"
	else
    	AC_CHECK_LIB(lam, main,
    	[   MPI_LIBS="$MPI_LIBS -llam" ],
    	[   echo "liblam not found. Probably not LAM-MPI"]
    	)
	fi    
fi

AC_CHECK_LIB(util, openpty, [ MPI_LIBS="$MPI_LIBS -lutil" ])
AC_CHECK_LIB(pthread, main, [ MPI_LIBS="$MPI_LIBS -lpthread" ])


PKG_LIBS="${MPI_LIBS} -fPIC"
PKG_CPPFLAGS="${MPI_INCLUDE} ${MPI_DEPS} -D${MPITYPE} -fPIC"

AC_SUBST(PKG_LIBS)
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(DEFS)

AC_OUTPUT(src/Makevars)
