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
   
#define USE_MPI
#include "Rmpi.h"

static int *stream = 0;

static struct {
	int size;
	char *schar;
} streamchar = {0, 0};

static struct {
	int nspawned;
	char **spawnchar;
} spstrmchar = {0, 0};

static double uurand;
double *user_unif_rand(){
    uurand = sprng (stream);
    return &uurand;
}


SEXP r_make_sprng_seed(){
	SEXP sexp_seed;
	PROTECT(sexp_seed= allocVector(INTSXP, 1));
	INTEGER(sexp_seed)[0] = make_sprng_seed();
	UNPROTECT(1);
	return sexp_seed;
}

SEXP r_init_sprng (SEXP sexp_kind,
                   SEXP sexp_streamnum,
                   SEXP sexp_nstreams,
                   SEXP sexp_seed)
{
    int kind    = INTEGER (sexp_kind)[0];
    int streamnum = INTEGER (sexp_streamnum)[0];
    int nstreams = INTEGER (sexp_nstreams)[0];
    int seed     = INTEGER (sexp_seed)[0];

    stream = init_sprng(kind, streamnum, nstreams, seed, SPRNG_DEFAULT );
	if (stream)
		return AsInt(1);
	else
		return AsInt(0);
}

SEXP r_pack_sprng()
{
    if (stream) {
        streamchar.size = pack_sprng(stream, &streamchar.schar); 
        return AsInt(1);
    }
	else
        return AsInt(0);
}

SEXP r_unpack_sprng(SEXP sexp_packold){
    char *old_stream_char;
	if (streamchar.size==0)
		return AsInt(0);
	else{
		if (stream) {
			if (INTEGER(sexp_packold)[0]) {
				old_stream_char = (char *) R_alloc(streamchar.size, sizeof(char));
				mystrcpy(old_stream_char, streamchar.schar, streamchar.size);
				streamchar.size=pack_sprng(stream, &streamchar.schar);
				free_sprng (stream);
				stream = unpack_sprng (old_stream_char);
			}
			else
				stream = unpack_sprng(streamchar.schar);
		}
		else
			stream = unpack_sprng(streamchar.schar);
		if (stream)
			return AsInt(1);
		else 
			return AsInt(0);
	}
}

SEXP r_print_sprng(){
	if (stream)
		return AsInt(print_sprng(stream));
	else
		return AsInt(0);
}

SEXP r_spawn_sprng (SEXP sexp_nspawned)
{
    int  **newstreams;
    int i;
	if (stream){
		spstrmchar.nspawned = spawn_sprng (stream, INTEGER (sexp_nspawned)[0],
                                &newstreams);
		spstrmchar.spawnchar= (char **) R_alloc (spstrmchar.nspawned, sizeof (char *));
		for (i = 0; i < spstrmchar.nspawned; i++) {
			pack_sprng (newstreams[i], &spstrmchar.spawnchar[i]);
			free_sprng (newstreams[i]);
		}
		return AsInt(spstrmchar.nspawned);
	}
	else
		return AsInt(0);
}

SEXP r_unpack_spawned_sprng(SEXP sexp_packold) 
{	
	if (spstrmchar.nspawned==0)
		return AsInt(0);
	else{
		if (INTEGER(sexp_packold)[0]) {
			streamchar.size=pack_sprng(stream, &streamchar.schar);
			free_sprng(stream);
		}
		stream = unpack_sprng(spstrmchar.spawnchar[0]);
		if (stream)
			return AsInt(1);
		else 
			return AsInt(0);
	}
}

/*
int *user_unif_nseed() { 
	return &nseed; 
}

void  user_unif_init(Int32 seed_in) { 
	seed = make_sprng_seed(); 
}
*/

SEXP r_free_sprng()
{
    if (stream) {
        free_sprng (stream);    
        stream = 0;
        return AsInt(1);
    } 
	else 
        return AsInt(0);
}
