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

//static int MPI_COMM_MAXSIZE=10;
//static MPI_Comm MPI_MASTER_SLAVE;
static MPI_Comm	*comm;
static MPI_Status *status;
static MPI_Datatype *datatype;
static MPI_Info *info;

SEXP mpi_initialize(){
	int flag;
	MPI_Initialized(&flag);
	if (flag)
		return AsInt(1);
	else {	
		MPI_Init((void *)0,(void *)0);
		MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
		MPI_Comm_set_errhandler(MPI_COMM_SELF, MPI_ERRORS_RETURN);
		comm=(MPI_Comm *)Calloc(COMM_MAXSIZE, MPI_Comm); 
		status=(MPI_Status *)Calloc(1, MPI_Status); 
		datatype=(MPI_Datatype *)Calloc(1, MPI_Datatype); 
		info=(MPI_Info *)Calloc(1, MPI_Info);
		comm[0]=MPI_COMM_WORLD;

		return AsInt(1);
	}
}

SEXP mpi_finalize(){
	MPI_Finalize();
	Free(comm);
	Free(status);
	Free(datatype);
	Free(info);
	return AsInt(1);
}

SEXP mpi_get_processor_name (){
	int resultlen;
	char *name;
	SEXP sexp_name;
    PROTECT (sexp_name  = allocVector (STRSXP, 1));
	name = (char *)Calloc(MPI_MAX_PROCESSOR_NAME, char);
	MPI_Get_processor_name(name, &resultlen);
	SET_STRING_ELT(sexp_name, 0, COPY_TO_USER_STRING(name));
	UNPROTECT(1);
	Free(name);

	return sexp_name;
}

SEXP bin_nchar(SEXP sexp_data){
	return AsInt(LENGTH(STRING_ELT(sexp_data,0)));
}

SEXP mpi_universe_size(){
	int *MPI_Universe_Size, univ_flag;
	MPI_Comm_get_attr(comm[0], MPI_UNIVERSE_SIZE, &MPI_Universe_Size, &univ_flag);
    return AsInt(*MPI_Universe_Size);
}

SEXP mpi_any_source(){
	return AsInt(MPI_ANY_SOURCE);
}

SEXP mpi_any_tag(){
	return AsInt(MPI_ANY_TAG);
}

SEXP mpi_proc_null(){
	return AsInt(MPI_PROC_NULL);
}

SEXP mpi_info_create(SEXP sexp_info){
	return AsInt(erreturn(mpi_errhandler(MPI_Info_create( &info[INTEGER(sexp_info)[0]]))));
}

SEXP mpi_info_set(SEXP sexp_info, SEXP sexp_key, SEXP sexp_value){
	return AsInt(erreturn(mpi_errhandler(MPI_Info_set(info[INTEGER(sexp_info)[0]], 
		CHAR (STRING_ELT (sexp_key,0)),
		CHAR (STRING_ELT (sexp_value,0))))));
}

SEXP mpi_info_get(SEXP sexp_info, SEXP sexp_key, SEXP sexp_valuelen, SEXP sexp_value){
	int flag;
	mpi_errhandler(MPI_Info_get(info[INTEGER(sexp_info)[0]], 
		CHAR (STRING_ELT (sexp_key,0)),
		INTEGER(sexp_valuelen)[0],
		CHAR (STRING_ELT (sexp_value,0)), &flag));
	return sexp_value;
}

SEXP mpi_info_free(SEXP sexp_info){
	return AsInt(erreturn(mpi_errhandler(MPI_Info_free( &info[INTEGER(sexp_info)[0]]))));
}

SEXP mpi_realloc_comm(SEXP sexp_newncomm){
	comm=(MPI_Comm *)Realloc(comm, INTEGER(sexp_newncomm)[0], MPI_Comm); 
	return AsInt(1);
}

SEXP mpi_realloc_status(SEXP sexp_newnstatus){
	status=(MPI_Status *)Realloc(status, INTEGER(sexp_newnstatus)[0], MPI_Status); 
	return AsInt(1);
}

SEXP mpi_realloc_datatype(SEXP sexp_newndatatype){
	datatype=(MPI_Datatype *)Realloc(datatype, INTEGER(sexp_newndatatype)[0], MPI_Datatype); 
	return AsInt(1);
}

/******************** Collective ***************************************/
SEXP mpi_gather(SEXP sexp_sdata,
				   SEXP sexp_type,
				   SEXP sexp_rdata,
				   SEXP sexp_root,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_sdata), commn=INTEGER(sexp_comm)[0], root=INTEGER(sexp_root)[0];
	
	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Gather(INTEGER(sexp_sdata), len, MPI_INT, 
			INTEGER(sexp_rdata), len, MPI_INT, root, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Gather(REAL(sexp_sdata), len, MPI_DOUBLE, 
			REAL(sexp_rdata), len, MPI_DOUBLE, root, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}

	return sexp_rdata;
}

SEXP mpi_gatherv(SEXP sexp_sdata,
				   SEXP sexp_type,
				   SEXP sexp_rdata,
				   SEXP sexp_recvcounts,
				   SEXP sexp_root,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_sdata), commn=INTEGER(sexp_comm)[0], root=INTEGER(sexp_root)[0];
	int *displs, gsize, rank, i;
	
	MPI_Comm_size(comm[commn], &gsize);
	MPI_Comm_rank(comm[commn], &rank);
	if (rank==root){
		displs=(int *)Calloc(gsize, int);
		displs[0]=0;
		for (i=1; i < gsize; i++)
			displs[i]=displs[i-1]+INTEGER(sexp_recvcounts)[i-1];
	}

	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Gatherv(INTEGER(sexp_sdata), len, MPI_INT, 
			INTEGER(sexp_rdata), INTEGER(sexp_recvcounts), displs, MPI_INT, 
				root, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Gatherv(REAL(sexp_sdata), len, MPI_DOUBLE, 
			REAL(sexp_rdata), INTEGER(sexp_recvcounts), displs, 
				MPI_DOUBLE, root, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}
	if (rank == root)
		Free(displs);
	return sexp_rdata;
}

SEXP mpi_scatter(SEXP sexp_sdata,
				   SEXP sexp_type,
				   SEXP sexp_rdata,
				   SEXP sexp_root,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_sdata), commn=INTEGER(sexp_comm)[0], root=INTEGER(sexp_root)[0];
	
	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Scatter(INTEGER(sexp_sdata), len, MPI_INT, 
			INTEGER(sexp_rdata), len, MPI_INT, root, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Scatter(REAL(sexp_sdata), len, MPI_DOUBLE, 
			REAL(sexp_rdata), len, MPI_DOUBLE, root, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}

	return sexp_rdata;
}

SEXP mpi_scatterv(SEXP sexp_sdata,
				  SEXP sexp_sendcounts,
				  SEXP sexp_type,
				  SEXP sexp_rdata,
				  SEXP sexp_root,
				  SEXP sexp_comm){
	int len=LENGTH(sexp_rdata), commn=INTEGER(sexp_comm)[0], root=INTEGER(sexp_root)[0];
	int *displs, gsize, rank, i;
	
	MPI_Comm_size(comm[commn], &gsize);
	MPI_Comm_rank(comm[commn], &rank);
	if (rank==root){
		displs=(int *)Calloc(gsize, int);
		displs[0]=0;
		for (i=1; i < gsize; i++)
			displs[i]=displs[i-1]+INTEGER(sexp_sendcounts)[i-1];
	}

	
	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Scatterv(INTEGER(sexp_sdata), INTEGER(sexp_sendcounts),
			displs, MPI_INT, INTEGER(sexp_rdata), len, MPI_INT, 
				root, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Scatterv(REAL(sexp_sdata), INTEGER(sexp_sendcounts),
			displs, MPI_DOUBLE, REAL(sexp_rdata), len,  
				MPI_DOUBLE, root, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}
	if (rank == root)
		Free(displs);
	return sexp_rdata;
}

SEXP mpi_allgather(SEXP sexp_sdata,
				   SEXP sexp_type,
				   SEXP sexp_rdata,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_sdata), commn=INTEGER(sexp_comm)[0];
	
	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Allgather(INTEGER(sexp_sdata), len, MPI_INT, 
			INTEGER(sexp_rdata), len, MPI_INT, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Allgather(REAL(sexp_sdata), len, MPI_DOUBLE, 
			REAL(sexp_rdata), len, MPI_DOUBLE, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}

	return sexp_rdata;
}

SEXP mpi_allgatherv(SEXP sexp_sdata,
				   SEXP sexp_type,
				   SEXP sexp_rdata,
				   SEXP sexp_recvcounts,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_sdata), commn=INTEGER(sexp_comm)[0], *displs, gsize, i;
	
	MPI_Comm_size(comm[commn], &gsize);
	displs=(int *)Calloc(gsize, int);
	displs[0]=0;
	for (i=1; i < gsize; i++)
		displs[i]=displs[i-1]+INTEGER(sexp_recvcounts)[i-1];

	switch (INTEGER(sexp_type)[0]){
	case 1:
		mpi_errhandler(MPI_Allgatherv(INTEGER(sexp_sdata), len, MPI_INT, 
			INTEGER(sexp_rdata), INTEGER(sexp_recvcounts), displs, MPI_INT, 
				comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Allgatherv(REAL(sexp_sdata), len, MPI_DOUBLE, 
			REAL(sexp_rdata), INTEGER(sexp_recvcounts), displs, 
				MPI_DOUBLE, comm[commn]));
		break;
/*	case 3:
		MPI_Comm_rank(comm[commn], &myrank);
		if (myrank==rank) {
			for (i=0; i<len; i++){
				slen=strlen(CHAR (STRING_ELT ((sexp_data),i)));    
				MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
			}
		}
		else {
			slen=strlen(CHAR(STRING_ELT(sexp_data,0)));
			tmp=(char *)R_alloc(slen, sizeof(char));
			for (i=0; i < len; i++){
				MPI_Bcast(tmp,slen,MPI_CHAR,rank, comm[commn]);
				SET_STRING_ELT(sexp_data, i, COPY_TO_USER_STRING(tmp));
			}
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		*/
	}
	Free(displs);
	return sexp_rdata;
}

SEXP mpi_bcast(SEXP sexp_data,
			   SEXP sexp_type,
			   SEXP	sexp_rank,
			   SEXP sexp_comm){

	int i, len=LENGTH(sexp_data), type=INTEGER(sexp_type)[0];
	int rank=INTEGER(sexp_rank)[0], commn=INTEGER(sexp_comm)[0],slen;
	int errcode=0;

	switch (type){
	case 1:
		errcode=MPI_Bcast(INTEGER(sexp_data), len, MPI_INT, rank, comm[commn]);
		break;
	case 2:
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), len, MPI_DOUBLE, rank, comm[commn]));
		break;
	case 3:
		for (i=0; i<len; i++){
			slen=LENGTH(STRING_ELT ((sexp_data),i)); 
			MPI_Bcast(CHAR (STRING_ELT ((sexp_data),i)), slen, MPI_CHAR, rank, comm[commn]);
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Bcast(REAL(sexp_data), 1, datatype[0], rank, comm[commn]));
		UNPROTECT(1);
		break;		
	}
	if (errcode!=MPI_SUCCESS){
		int errmsglen;
		char errmsg[MPI_MAX_ERROR_STRING];
		MPI_Error_string(errcode, errmsg, &errmsglen);
		Rprintf("%s\n",errmsg);
		return mkString("error");
	}
	else
		return sexp_data;
}

SEXP mpi_send(SEXP sexp_data, 
			  SEXP sexp_type,
			  SEXP sexp_dest, 
			  SEXP sexp_tag,
			  SEXP sexp_comm){
	int i,slen,len=LENGTH(sexp_data),type=INTEGER(sexp_type)[0], dest=INTEGER(sexp_dest)[0];
	int commn=INTEGER(sexp_comm)[0], tag=INTEGER(sexp_tag)[0];

	switch (type){
	case 1:
		mpi_errhandler(MPI_Send(INTEGER(sexp_data), len, MPI_INT, dest, tag, comm[commn]));
		break;
	case 2:
		mpi_errhandler(MPI_Send(REAL(sexp_data), len, MPI_DOUBLE, dest, tag, comm[commn]));
		break;
	case 3:
		for (i=0; i<len; i++){
			slen=LENGTH(STRING_ELT(sexp_data,i));
			MPI_Send(CHAR(STRING_ELT(sexp_data,i)),slen, MPI_CHAR, dest, tag, comm[commn]); 
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Send(REAL(sexp_data), 1, datatype[0], dest, tag, comm[commn]));
		UNPROTECT(1);
		break;		
	}
	return R_NilValue;
}

SEXP mpi_recv(SEXP sexp_data, 
  			  SEXP sexp_type,
			  SEXP sexp_source, 
			  SEXP sexp_tag,
			  SEXP sexp_comm,
			  SEXP sexp_status){
	int i, len=LENGTH(sexp_data), type=INTEGER(sexp_type)[0], source=INTEGER(sexp_source)[0];
	int tag=INTEGER(sexp_tag)[0],commn=INTEGER(sexp_comm)[0], statusn=INTEGER(sexp_status)[0];
	int slen;

	switch (type){
	case 1:
		mpi_errhandler(MPI_Recv(INTEGER(sexp_data), len, MPI_INT, source, tag, comm[commn],
			&status[statusn]));
		break;
	case 2:
		mpi_errhandler(MPI_Recv(REAL(sexp_data), len, MPI_DOUBLE, source, tag, comm[commn],
			&status[statusn]));
		break;
	case 3:
		slen=LENGTH(STRING_ELT(sexp_data,0));
		for (i=0; i < len; i++){
			MPI_Recv(CHAR(STRING_ELT(sexp_data,i)),
				slen,MPI_CHAR,source,tag, comm[commn],&status[statusn]);
		}
		break;
	default:
		PROTECT(sexp_data=AS_NUMERIC(sexp_data));
		mpi_errhandler(MPI_Recv(REAL(sexp_data), 1, datatype[0], source, tag, comm[commn],
			&status[statusn]));
		UNPROTECT(1);
		break;		
	}
	return sexp_data;
}

SEXP mpi_reduce(SEXP sexp_send, 
				SEXP sexp_type,
				SEXP sexp_op, 
				SEXP sexp_dest,
				SEXP sexp_comm){
	int len=LENGTH(sexp_send), type=INTEGER(sexp_type)[0], dest=INTEGER(sexp_dest)[0];
	int commn=INTEGER(sexp_comm)[0], intop = INTEGER(sexp_op)[0];
	MPI_Op op;
	SEXP sexp_recv;

	switch(intop){
	case 1:
		op=MPI_SUM;
		break;
	case 2:
		op=MPI_PROD;
		break;
	case 3:
		op=MPI_MAX;
		break;
	case 4:
		op=MPI_MIN;
		break;
	case 5:
		op=MPI_MAXLOC;
		break;
	case 6:
		op=MPI_MINLOC;
		break;

	}
	switch(type){
	case 1:
		if (intop < 5){
			PROTECT (sexp_recv = allocVector(INTSXP, len));
			mpi_errhandler(MPI_Reduce(INTEGER(sexp_send), INTEGER(sexp_recv), 
			len, MPI_INT, op, dest, comm[commn])); 
			break;
		}
		else{
			int *send, rank, i;
			MPI_Comm_rank(comm[commn], &rank);
			send = (int *)Calloc(2*len, int);
			for (i=0; i < len; i++){
				send[2*i] = INTEGER(sexp_send)[i];
				send[2*i+1] = rank; 
			}
			PROTECT (sexp_recv = allocVector(INTSXP, 2*len));
			mpi_errhandler(MPI_Reduce(send, INTEGER(sexp_recv), 
			len, MPI_2INT, op, dest, comm[commn])); 
			Free(send);
			break;
		}
	case 2:
		if (intop < 5){
			PROTECT (sexp_recv = allocVector(REALSXP, len));
			mpi_errhandler(MPI_Reduce(REAL(sexp_send), REAL(sexp_recv), 
			len, MPI_DOUBLE, op, dest, comm[commn])); 
			break;
		}
		else {
			int i, rank;
			struct Dblint *send, *recv;
			send=(struct Dblint *)Calloc(len, struct Dblint);
			recv=(struct Dblint *)Calloc(len, struct Dblint);
			MPI_Comm_rank(comm[commn], &rank);
			for (i=0;i<len;i++){
				send[i].x = REAL(sexp_send)[i];
				send[i].rank = rank;
			}
			mpi_errhandler(MPI_Reduce(send, recv, len, MPI_DOUBLE_INT, op, dest, comm[commn])); 
			PROTECT (sexp_recv = allocVector(REALSXP, 2*len));
			for (i=0; i<len; i++){
				REAL(sexp_recv)[2*i] = recv[i].x;
				REAL(sexp_recv)[2*i+1] = recv[i].rank;
			}
			Free(send);
			Free(recv);
			break;
		}
	}
	
	UNPROTECT(1);			
	return sexp_recv;
}


SEXP mpi_allreduce(SEXP sexp_send, 
				   SEXP sexp_type,
				   SEXP sexp_op,
				   SEXP sexp_comm){
	int len=LENGTH(sexp_send), type=INTEGER(sexp_type)[0], commn=INTEGER(sexp_comm)[0];
	int intop = INTEGER(sexp_op)[0];
	MPI_Op op;
	SEXP sexp_recv;

	switch(intop){
	case 1:
		op=MPI_SUM;
		break;
	case 2:
		op=MPI_PROD;
		break;
	case 3:
		op=MPI_MAX;
		break;
	case 4:
		op=MPI_MIN;
		break;
	case 5:
		op=MPI_MAXLOC;
		break;
	case 6:
		op=MPI_MINLOC;
		break;

	}

	switch(type){
	case 1:
		if (intop < 5){
			PROTECT (sexp_recv = allocVector(INTSXP, len));
			mpi_errhandler(MPI_Allreduce(INTEGER(sexp_send), INTEGER(sexp_recv), 
			len, MPI_INT, op, comm[commn])); 
		break;
		}
		else{
			int *send, rank, i;
			MPI_Comm_rank(comm[commn], &rank);
			send = (int *)Calloc(2*len, int);
			for (i=0; i < len; i++){
				send[2*i] = INTEGER(sexp_send)[i];
				send[2*i+1] = rank; 
			}
			PROTECT (sexp_recv = allocVector(INTSXP, 2*len));
			mpi_errhandler(MPI_Allreduce(send, INTEGER(sexp_recv), 
			len, MPI_2INT, op, comm[commn])); 
			Free(send);
		break;
		}
	case 2:
		if (intop < 5) {
			PROTECT (sexp_recv = allocVector(REALSXP, len));
			mpi_errhandler(MPI_Allreduce(REAL(sexp_send), REAL(sexp_recv), 
				len, MPI_DOUBLE, op, comm[commn])); 
			break;
		}
		else {
			int i, rank;
			struct Dblint *send, *recv;
			send=(struct Dblint *)Calloc(len, struct Dblint);
			recv=(struct Dblint *)Calloc(len, struct Dblint);
			MPI_Comm_rank(comm[commn], &rank);
			for (i=0;i<len;i++){
				send[i].x = REAL(sexp_send)[i];
				send[i].rank = rank;
			}
			mpi_errhandler(MPI_Allreduce(send, recv, len, MPI_DOUBLE_INT, op, comm[commn])); 
			PROTECT (sexp_recv = allocVector(REALSXP, 2*len));
			for (i=0; i<len; i++){
				REAL(sexp_recv)[2*i] = recv[i].x;
				REAL(sexp_recv)[2*i+1] = recv[i].rank;
			}
			Free(send);
			Free(recv);
			break;
		}
	}
	
	UNPROTECT(1);			
	return sexp_recv;
}

SEXP mpi_probe(SEXP sexp_source, SEXP sexp_tag, SEXP sexp_comm, SEXP sexp_status){
	return AsInt(erreturn(mpi_errhandler(MPI_Probe(INTEGER (sexp_source)[0], 
		INTEGER(sexp_tag)[0], comm[INTEGER(sexp_comm)[0]], 
		&status[INTEGER(sexp_status)[0]]))));
}

SEXP mpi_get_count(SEXP sexp_status, SEXP sexp_type){
	SEXP sexp_count;
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
	
	PROTECT (sexp_count = allocVector(INTSXP, 1));
	mpi_errhandler(MPI_Get_count(&status[INTEGER(sexp_status)[0]], datatype, INTEGER(sexp_count)));
	UNPROTECT(1);

	return sexp_count;
}

SEXP mpi_get_sourcetag (SEXP sexp_status){
	int statusn =INTEGER(sexp_status)[0];
	SEXP sexp_st;
	PROTECT(sexp_st=allocVector(INTSXP,2));
	INTEGER(sexp_st)[0]=status[statusn].MPI_SOURCE;
	INTEGER(sexp_st)[1]=status[statusn].MPI_TAG;
	UNPROTECT(1);
	return sexp_st;
}

/******************************* COMM **************************************/
SEXP mpi_barrier(SEXP sexp_comm){
	return AsInt(erreturn(mpi_errhandler(MPI_Barrier(comm[INTEGER(sexp_comm)[0]])))); 
}

SEXP mpi_comm_is_null(SEXP sexp_comm){
	return AsInt(comm[INTEGER(sexp_comm)[0]]==MPI_COMM_NULL);
}

SEXP mpi_comm_maxsize(){
	return AsInt(COMM_MAXSIZE);
}

SEXP mpi_comm_size(SEXP sexp_comm){
	int size;
	MPI_Comm_size(comm[INTEGER(sexp_comm)[0]], &size); 
	return AsInt(size);
}

SEXP mpi_comm_rank(SEXP sexp_comm){
	int rank;
	MPI_Comm_rank(comm[INTEGER(sexp_comm)[0]], &rank);
	return AsInt(rank);
}

SEXP mpi_comm_dup(SEXP sexp_comm, SEXP sexp_newcomm){
    int commn=INTEGER(sexp_comm)[0], newcommn=INTEGER(sexp_newcomm)[0];
    if (comm==0)
        return AsInt(erreturn(mpi_errhandler(MPI_Comm_dup(MPI_COMM_WORLD,
                &comm[newcommn]))));
    else
        return AsInt(erreturn(mpi_errhandler(MPI_Comm_dup(comm[commn],
                &comm[newcommn]))));
}

SEXP mpi_comm_free(SEXP sexp_comm){
	return AsInt(erreturn(mpi_errhandler(MPI_Comm_free(&comm[INTEGER(sexp_comm)[0]]))));
}

SEXP mpi_abort(SEXP sexp_comm){
	int errcode=0, commn=INTEGER(sexp_comm)[0];
	if (commn==0)
		MPI_Abort(MPI_COMM_WORLD, errcode);
	else
		MPI_Abort(comm[commn], errcode);
	Rprintf("The return errcode for mpi.abort() is %d\n", errcode);
	return AsInt(errcode);
}

/********************Intercomm********************************************/
SEXP mpi_comm_set_errhandler(SEXP sexp_comm){
	return AsInt(erreturn(MPI_Comm_set_errhandler(comm[INTEGER(sexp_comm)[0]], 
		MPI_ERRORS_RETURN)));
}

SEXP mpi_comm_test_inter(SEXP sexp_comm){
	int flag;
	MPI_Comm_test_inter(comm[INTEGER(sexp_comm)[0]], &flag);
	return AsInt(flag);
}

SEXP mpi_comm_spawn (SEXP sexp_slave,
					 SEXP sexp_argv,
					 SEXP sexp_nslave,
					 SEXP sexp_info,
					 SEXP sexp_root,
					 SEXP sexp_intercomm){
    int i, nslave = INTEGER (sexp_nslave)[0], len = LENGTH (sexp_argv);
	int infon=INTEGER(sexp_info)[0], root=INTEGER(sexp_root)[0];
	int intercommn=INTEGER(sexp_intercomm)[0], *slaverrcode, realns;

	slaverrcode = (int *)Calloc(nslave, int);
	if (len==0)
		mpi_errhandler(MPI_Comm_spawn (CHAR (STRING_ELT (sexp_slave, 0)), MPI_ARGV_NULL, nslave,   
					info[infon], root, MPI_COMM_SELF, &comm[intercommn],
					slaverrcode)); 
	else {
		char **argv = (char **) R_alloc (len+1, sizeof (char *));
		for (i = 0; i < len; i++)
			argv[i] = CHAR (STRING_ELT (sexp_argv, i));
		argv[len] = NULL;
		mpi_errhandler(MPI_Comm_spawn (CHAR (STRING_ELT (sexp_slave, 0)), argv, nslave,   
					info[infon], root, MPI_COMM_SELF, &comm[intercommn],
					slaverrcode)); 
	}

	MPI_Comm_remote_size(comm[intercommn], &realns);
	if (realns < nslave)
		for (i=0; i < nslave; mpi_errhandler(slaverrcode[i++]));

	Free(slaverrcode);
	
	Rprintf("\t%d slaves are spawned successfully. %d failed.\n", realns, nslave-realns);
    return AsInt(realns);
}

SEXP mpi_comm_remote_size(SEXP sexp_comm){
	int size;
	mpi_errhandler(MPI_Comm_remote_size(comm[INTEGER(sexp_comm)[0]], &size));
	return AsInt(size);
}

SEXP mpi_comm_get_parent(SEXP sexp_comm){
	return AsInt(erreturn(mpi_errhandler(MPI_Comm_get_parent(&comm[INTEGER(sexp_comm)[0]]))));
}

SEXP mpi_intercomm_merge(SEXP sexp_intercomm, SEXP sexp_high, SEXP sexp_comm){
	return AsInt(erreturn(mpi_errhandler(MPI_Intercomm_merge(comm[INTEGER(sexp_intercomm)[0]],
		INTEGER(sexp_high)[0],
		&comm[INTEGER(sexp_comm)[0]]))));
}


SEXP mpi_comm_disconnect(SEXP sexp_comm){
	return AsInt(erreturn(mpi_errhandler(MPI_Comm_disconnect(&comm[INTEGER(sexp_comm)[0]]))));
}

SEXP mpi_is_master(){
	int check;
	MPI_Comm master;
	MPI_Comm_get_parent(&master);
	check=(master==MPI_COMM_NULL);
	MPI_Comm_free(&master);
	return AsInt(check);
}

