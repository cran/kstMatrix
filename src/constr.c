#include <R.h>
#include <Rinternals.h>
#include "kstMatrix.h"

// #define DEBUG


#define BLK_SIZE (long)65536    /** Size of block to store knowledge states   **/
#define LD_BS 16         /** Similar to LD_BPL and BPL_M1              **/
#define BS_M1 (long)65535



/**********************************************************************
 Definition of basis and space limitations.
 **********************************************************************/

#define MAX_BASE_BLKS 5  /** Maximal number of blocks for the basis or  **/
#define MAX_SPACE_BLKS 65536    /** the space, respectively              **/

/* These constants mean that I can handle bases with up to     */
/* 10240 (= MAX_BASE_BLKS * BLK_SIZE) elements and spaces with */
/* up to 33554432 (= MAX_SPACE_BLKS * BLK_SIZE) states.        */




/**********************************************************************
 Some macros for easier conputation of bit positions and similar things.
 **********************************************************************/


#define blkno(i) ((i+BS_M1)>>LD_BS)
#define blkcnt(i) (i >> LD_BS)                    /** Similar to above **/
#define blkpos(i) (i & BS_M1)



/**********************************************************************
 Definition of global variables.
 **********************************************************************/

long **basis, **basis_e, **basis_u, **space, no_space_blks;







int import_basis(int *bas, long size_q, long size_b)
{
  long     i, j, k, q, bi, pi;
  int     w;

  w=word_no(size_q);


  if ((size_b) > (long)(BLK_SIZE * MAX_BASE_BLKS)) {
    error("\aToo many basis elements (max %ld)!\n", BLK_SIZE * MAX_BASE_BLKS);
  }
  i = blkno(size_b);
  for (k=0; k<i; k++) {
    if (!(basis[k] = (long *)R_Calloc(BLK_SIZE*w, long))) {
      for (q=0; q<k; k++) {
        R_Free(basis[q]);
        R_Free(basis_e[q]);
        R_Free(basis_u[q]);
      }
      R_Free(basis_e);
      R_Free(basis_u);
      R_Free(basis);
      R_Free(space);
      error("\aNot enough memory for basis block(s)!\n");
    }
    if (!(basis_e[k] = (long *)R_Calloc(BLK_SIZE*w, long))) {
      for (q=0; q<k; k++) {
        R_Free(basis[q]);
        R_Free(basis_e[q]);
        R_Free(basis_u[q]);
      }
      R_Free(basis[k]);
      R_Free(basis_e);
      R_Free(basis_u);
      R_Free(basis);
      R_Free(space);
      error("\aNot enough memory for basis block(s)!\n");
    }
    if (!(basis_u[k] = (long *)R_Calloc(BLK_SIZE*w, long))) {
      for (q=0; q<k; k++) {
        R_Free(basis[q]);
        R_Free(basis_e[q]);
        R_Free(basis_u[q]);
      }
      R_Free(basis[k]);
      R_Free(basis_e[k]);
      R_Free(basis_e);
      R_Free(basis_u);
      R_Free(basis);
      R_Free(space);
      error("\aNot enough memory for basis block(s)!\n");
    }
    for (q=0; q<BLK_SIZE*w; q++)
      basis[k][q] = basis_e[k][q] = basis_u[k][q] = 0L;
  }

  for (i=0; i<size_b; i++) {
    bi = blkcnt(i);
    pi = blkpos(i) * w;
    for (j=0; j<size_q; j++)
      if (bas[j+i*size_q]) {
        basis[bi][pi+word_cnt(j)] |= (1L << word_pos(j));
      }
    for (k=0; k<w; k++)
      basis_u[bi][pi+k] = basis_e[bi][pi+k] = basis[bi][pi+k];
    for (j=0; j<i; j++) {
      int bj=blkcnt(j);
      int pj=blkpos(j) * w;
      if (subset( &(basis[bj][pj]), &(basis[bi][pi]) , w) )
        for (k=0; k<w; k++)
          basis_e[bi][pi+k] &= (~(basis[bj][pj+k]));
    }
    j=1;
    for (k=0; k<w; k++)
      if (basis_e[bi][pi+k])
        j=0;
    if (j) {
      for (k=0; k<w; k++)
        basis_u[bi][pi+k] = basis_e[bi][pi+k] = basis[bi][pi+k] = 0L;
      i--;
      (size_b)--;
    }
    else
      for (k=0; k<w; k++)
         basis_u[bi][pi+k] &= (~(basis_e[bi][pi+k]));
  }

  return(0);
}



void constr_free_memory(void) {
  long i;
  // Free all the allocated memory
  for (i=0; i < no_space_blks; i++) {
    R_Free(space[i]);
  }
  no_space_blks = 0;
  R_Free(space);
  for (i=0; i<blkno(size_b); i++) {
    R_Free(basis[i]);
    R_Free(basis_e[i]);
    R_Free(basis_u[i]);
  }
  R_Free(basis);
  R_Free(basis_e);
  R_Free(basis_u);
}






void constr(int *noi, // number of items
            int *nob, // number of basis elements
            int *bas, // basis matrix (R format) as integer vector
            int *nos) // number of states in the resulting knowledge space (return value)
  {
  int err;
  register long i, j, k;
  long wsize_q, fail;
  long bi, pi, bj, pj, bk, pk;
  long size, *state;

  size_q = (long)*noi;
  size_b = (long)*nob;
  size_s = 0;
  if (!(space = (long **)R_Calloc(MAX_SPACE_BLKS, long *))) {
    error("\aNot enough memory for Blocklists!\n");
  }
  if (!(basis = (long **)R_Calloc(MAX_BASE_BLKS, long *))) {
    R_Free(space);
    error("\aNot enough memory for Blocklists!\n");
  }
  if (!(basis_e = (long **)R_Calloc(MAX_BASE_BLKS, long *))) {
    R_Free(space);
    R_Free(basis);
    error("\aNot enough memory for Blocklists!\n");
  }
  if (!(basis_u = (long **)R_Calloc(MAX_BASE_BLKS, long *)))   {
    R_Free(space);
    R_Free(basis);
    R_Free(basis_e);
    error("\aNot enough memory for Blocklists!\n");
  }
  if ((err = import_basis(bas, size_q, size_b))) {
    error("Error %d importing basis!\n", err);
  }

  wsize_q = word_no(size_q);
  if (!(space[0] = R_Calloc(BLK_SIZE  * wsize_q, long))) {
    constr_free_memory();
    error("\aNot enough memory for first block!\n");
  }
  if (!(state = R_Calloc(wsize_q, long))) {
    R_Free(space[0]);
    constr_free_memory();
    error("Not enough working memory!\n");
  }
  no_space_blks = 1;
  for (i=0; i<wsize_q; i++)
    space[0][i] = 0L;
  size_s = 1; /** emptyset is first state **/

for (i=0; i<size_b; i++) {
  bi = blkcnt(i);
  pi = blkpos(i) * wsize_q;
  size = size_s;

  for (j=0; j<size; j++) {
    bj = blkcnt(j);
    pj = blkpos(j) * wsize_q;
    /* Test the conditions as they are    */
    /* mentioned in definition 4.1 of the */
    /* article.                           */
    if (subset( basis_u[bi]+pi, space[bj]+pj, wsize_q)) {
      fail = 0;
      if (!(subset( basis_e[bi]+pi, space[bj]+pj, wsize_q))) {
        for (k=0; ( (k<i) && (!fail) ); k++) {
          bk = blkcnt(k);
          pk = blkpos(k) * wsize_q;
          diff_set(state, basis[bk]+pk, basis_e[bi]+pi,
                   wsize_q);
          if (subset(state, space[bj]+pj, wsize_q)) {
            section_set(state, basis_e[bi]+pi,
                        basis_e[bk]+pk, wsize_q);
            if (!(subset(state, space[bj]+pj,
                         wsize_q)))
              fail = 1;
          }
        }
        if (!fail) {
          if (!blkpos(size_s)) {
            if (no_space_blks >= MAX_SPACE_BLKS) {
              REprintf("Too many states !\n");
              error("%ld states were computed\n", size_s);
            }
            else if (!(space[no_space_blks++] =
                     R_Calloc(BLK_SIZE * wsize_q, long))) {
              no_space_blks--;
              constr_free_memory();
              R_Free(state);
              REprintf("\aNot enough memory for additional block!\n");
              error("%ld states were computed\n", ++size_s);
            }
          }
          union_set(space[no_space_blks-1] + (wsize_q *
              blkpos(size_s++)), space[bj]+pj,
              basis[bi]+pi, wsize_q);
        }
#ifdef DEBUG
        else Rprintf("fail set to %ld for basis element %ld and state %ld.\n", fail, i+1, j+1);
#endif
      }
    }

  }
#ifdef DEBUG
  Rprintf("Processed base element %ld: %ld states (%ld).\n", i+1, size_s, size);
#endif
}
(*nos) = (int)size_s;
}


void constr_results(int *spc) {
  register long i, j, bi, pi;
  // Copy/transfer data from **space to *spc
  // Try to re-use code from asc_write_space from con-tools.c
  for (i=0; i<size_s; i++) {
    bi = blkcnt(i);
    pi = blkpos(i) * word_no(size_q);
    for (j=0; j<size_q; j++) {
      if (space[bi][pi+word_cnt(j)] & (1L << word_pos(j))) {
        spc[(j*size_s)+i] = 1;
      }
      else
        spc[(j*size_s)+i] = 0;
    }
  }
  constr_free_memory();
}


