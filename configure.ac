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

if test "$MPI_ROOT"==""; then
  for d in /opt/lib /usr /usr/lib /usr/lib64 /usr/local/lib /usr/local; do
    if test -f $d/lam/include/mpi.h && test -f $d/lam/lib/libmpi.so; then
      echo "I am here $d/lam"
      MPI_ROOT=$d/lam
      break
    elif test -f $d/mpi/include/mpi.h; then
      echo "I am here $d/mpi"
      MPI_ROOT=$d/mpi
      break
    elif test -f $d/mpich/include/mpi.h; then
      echo "I am here $d/mpich"
      MPI_ROOT=$d/mpich
      break
    elif test -f $d/include/mpi.h; then
      echo "I am here $d"
      MPI_ROOT=$d
      break
    fi
  done
fi

if test -n "$MPI_ROOT"; then
    echo "Try to find mpi.h ..."
    if test -f ${MPI_ROOT}/include/mpi.h; then
         echo "Found in ${MPI_ROOT}/include"
         MPI_INCLUDE="-I${MPI_ROOT}/include"
    elif test -f /usr/include/lam/mpi.h; then
         echo "Found in /usr/include/lam"
         MPI_INCLUDE="-I/usr/include/lam"
    elif test -f /usr/local/include/mpi.h; then
         echo "Found in /usr/local/include"
         MPI_INCLUDE="-I/usr/local/include"
    else 
         echo "Cannot find mpi head file"
         echo "Please use --with-mpi=/path/to/mpi or specify the environment variable MPI_ROOT"
         exit 1
    fi
else
    AC_CHECK_HEADER(mpi.h)
fi 

echo "Try to find libmpi or libmpich ..."
if test -f ${MPI_ROOT}/lib/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib"
        MPI_LIBS="-L${MPI_ROOT}/lib -lmpi"
	if test -d  ${MPI_ROOT}/lib/openmpi; then
        	echo "Found openmpi dir in ${MPI_ROOT}/lib"
		MPI_DEPS="-DMPI2"
	fi
elif test -f ${MPI_ROOT}/lib64/lam/libmpi.so; then
        echo "Found libmpi in ${MPI_ROOT}/lib64/lam"
        MPI_LIBS="-L${MPI_ROOT}/lib64/lam -lmpi"
elif test -f ${MPI_ROOT}/lib/libmpich.a; then
        echo "Found libmpich in ${MPI_ROOT}/lib"
        MPI_LIBS="-L${MPI_ROOT}/lib -lmpich"
	MPI_DEPS="-DMPI2"
else
    AC_CHECK_LIB(mpi, main,
    	[   MPI_LIBS="-L${MPI_ROOT}/lib -lmpi"  ],
    	[   echo "libmpi not found. exiting..."
        	exit 1  ]
)
fi    

echo "Try to find liblam ..."
if test -f ${MPI_ROOT}/lib/liblam.a; then
        echo "Found liblam in ${MPI_ROOT}/lib"
        MPI_LIBS="${MPI_LIBS} -llam"
	MPI_DEPS="-DMPI2"
elif test -f ${MPI_ROOT}/lib64/lam/liblam.so; then
        echo "Found liblam in ${MPI_ROOT}/lib64/lam"
        MPI_LIBS="${MPI_LIBS} -llam"
	MPI_DEPS="-DMPI2"
else
    AC_CHECK_LIB(lam, main,
    	[   MPI_LIBS="$MPI_LIBS -llam" 
	    MPI_DEPS="-DMPI2" ],
    	[   echo "liblam not found. Probably not LAM-MPI"]
    )
fi    

#AC_CHECK_LIB(util, openpty, [ MPI_LIBS="$MPI_LIBS -lutil" ])
AC_CHECK_LIB(pthread, main, [ MPI_LIBS="$MPI_LIBS -lpthread" ])


PKG_LIBS="${MPI_LIBS} -fPIC"
PKG_CPPFLAGS="${MPI_INCLUDE} ${MPI_DEPS} -fPIC"

AC_SUBST(PKG_LIBS)
AC_SUBST(PKG_CPPFLAGS)
AC_SUBST(DEFS)

AC_OUTPUT(src/Makevars)
