#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <limits.h>
#include "kstMatrix.h"

extern long *import_famset(int noi,     // number of items
                           int nos,            // number of sets (states, respense patterns, ...)
                           int *data           // vector of R matrix containing the data
);



tstate base,minimals;

void basis_reduction(int *noi, // number of items
           int *nos, // number of states
           int *bas, // basis matrix (R format) as integer vector
           int *nob) // number of states in the resulting basis (return value)
{
  size_b = *nos;
  size_q = *noi;
  size_w=(size_q+BPL_M1)>>LD_BPL;
  register long i,j,k;

  base = import_famset(size_q, size_b, bas);
  if (!(minimals = R_Calloc(size_b*size_w, long))) {
    R_Free(base);
    error("ERROR: Not enough memory.\n");
  }
  for (i=0; i<size_b*size_w; i++) minimals[i] = base[i];

  for (i=0; i<size_b-1; i++)  {
    if (non_empty(minimals+(size_w*i), size_w))
      for (j=i+1; j<size_b; j++) {
        if (subset(base+(size_w*i),base+(size_w*j),size_w))
          for (k=0;k<size_w;k++)
            minimals[(size_w*j)+k] &= (~(base[(size_w*i)+k]));
        else if (subset(base+(size_w*j),base+(size_w*i),size_w))
          for (k=0;k<size_w;k++)
            minimals[(size_w*i)+k] &= (~(base[(size_w*j)+k]));
      }
  }

  init_bit_count();
  j=0;
  for (i=0; i<size_b; i++) {
    if ((k=set_size(minimals+size_w*i)) > 0) j++;
  }
  *nob = j;
}


void basis_results(int *basis) {
  register long i, j, l, m;

  l = 1;
  for (m=1; m<=size_q; m++)
    for (i=0; i<size_b; i++) {
      if (set_size(minimals+size_w*i) == m) {
        for (j=0; j<size_q; j++) {
          if (base[i*size_w+word_cnt(j)] & (1L << word_pos(j))) basis[(l-1)*size_q+j] = 1;
          else basis[(l-1)*size_q+j] = 0;
        }
        l++;
      }
    }
  R_Free(base);
  R_Free(minimals);
}



void sf_results(int *basis, int *mins) {
  register long i, j, l, m;

  print_matrix(base, size_b);
  print_matrix(minimals, size_b);
  l = 1;
  for (m=1; m<=size_q; m++)
    for (i=0; i<size_b; i++) {
      if (set_size(minimals+size_w*i) == m) {
        for (j=0; j<size_q; j++) {
          if (base[i*size_w+word_cnt(j)] & (1L << word_pos(j))) basis[(l-1)*size_q+j] = 1;
          else basis[(l-1)*size_q+j] = 0;
          if (minimals[i*size_w+word_cnt(j)] & (1L << word_pos(j))) mins[(l-1)*size_q+j] = 1;
          else mins[(l-1)*size_q+j] = 0;
        }
        l++;
      }
    }
  R_Free(base);
  R_Free(minimals);
}
