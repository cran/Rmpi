/* Copyright (C) 2002 Hao Yu
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or   
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of 
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
   
#include "Rmpi.h"


int mpi_errhandler(int errcode)
{
	int errmsglen;
	char errmsg[MPI_MAX_ERROR_STRING];

    if (errcode != MPI_SUCCESS) {
		MPI_Error_string(errcode, errmsg, &errmsglen);
		error(errmsg);
	}
	
	return errcode;
}

int erreturn(int errcode){
	if (errcode==MPI_SUCCESS)
		return 1;
	else
		return 0;
}

/*
MPI_Datatype mpitype(SEXP sexp_type){
	MPI_Datatype datatype;
	
	switch(INTEGER(sexp_type)[0]){
	case 1:
		datatype=MPI_INT;
		break;
	case 2:
		datatype=MPI_DOUBLE;
		break;
	case 3:
		datatype=MPI_CHAR;
		break;
	}
	return datatype;
}

void mystrcpy(char *new_str, char *old_str, int size) {
	int i;
	for (i=0; i< size; new_str[i]=old_str[i++]);
}*/
 
