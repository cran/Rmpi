#include <mpi.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Random.h>

#define COMM_MAXSIZE 10

int mpi_errhandler(int errcode);
int erreturn(int errcode);

/*MPI_Datatype mpitype(SEXP sexp_type); 

void mystrcpy(char *new_str, char *old_str, int size); */

SEXP AsInt (int n);

struct Dblint {
	double x;
	int rank;

};


