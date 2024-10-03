#define helpers

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include "kstMatrix.h"


void print_matrix(tstate x, long rows) {
  register int i;

  for (i=0; i<rows; i++) {
    Rprintf("%03lx ", x[i]);
  }
  Rprintf("\n");
}

int non_empty(tstate s, int w_size)
{
  while (w_size--) {
    if (*(s++))
      return 1;
  }
  return 0;
}

long size_b, size_q, size_s, size_w;


int bit_count[256];
void init_bit_count(void)
{
  int i,j,k;

  bit_count[0] = 0;
  k = 1;
  for (i=0; i<8 ;i++) {
    for (j=0; j<k; j++)
      bit_count[k+j] = bit_count[j] + 1;
    k <<= 1;
  }
}
int set_size(tstate vector)
{
  unsigned char *v = (unsigned char*) vector;
  int i, s=0;
  int bytes=size_w*sizeof(long);

  for (i=0; i<bytes; i++) {
    s += bit_count[v[i]];
  }
  return(s);
}



long *temp;

int set_dist(long *set1, long *set2, int words) {
  int i;

  size_w = (long) words;

  for (i=0; i<words; i++)
    temp[i] = set1[i] ^ set2[i];
  return set_size(temp);

}





int subset(long *a, long *b, long l)
{
  long i;

  for (i=0; i<l; i++) {
    if ( ( (a[i] & b[i]) != a[i]) ||
         ( (a[i] | b[i]) != b[i])   )
      return(0);
  }
  return(1);
}



int equal_set(long *a, long *b, long l)
{
  long i;

  for (i=0; i<l; i++)
    if (a[i] != b[i])
      return(0);
  return(1);
}



void section_set(long *dest,
                 long *src1,
                 long *src2,
                 long  l)
{
  long i;

  for (i=0; i<l; i++)
    dest[i] = src1[i] & src2[i];
}




void union_set(long *dest,
               long *src1,
               long *src2,
               long  l)
{
  long i;

  for (i=0; i<l; i++)
    dest[i] = src1[i] | src2[i];
}





void diff_set(long *dest,
              long *src1,
              long *src2,
              long  l)
{
  long i;

  for (i=0; i<l; i++)
    dest[i] = src1[i] & (~(src2[i]));
}





long *import_famset(int noi,     // number of items
                    int nos,     // number of sets (states, respense patterns, ...)
                    int *data    // vector of R matrix containing the data
                    )
{
  register long i,j;
  long *famset;
  long w = word_no(noi);

  if (!(famset = (long *)R_Calloc(nos*w, long))) {
    warning("Not enough memory.\n");
    return(NULL);
  }
  for (i=0; i<nos; i++) {
    for (j=0; j<noi; j++) {
      if (data[i*noi+j]) {
        famset[i*w + word_cnt(j)] |= (1L << word_pos(j));
      }
    }
  }
  return(famset);
}
