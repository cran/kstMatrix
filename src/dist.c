#include <R.h>
#include <Rinternals.h>
#include "kstMatrix.h"


extern long *import_famset(int noi,     // number of items
                    int nos,            // number of sets (states, respense patterns, ...)
                    int *data           // vector of R matrix containing the data
);



void dist(int *noi,        // number of items
          int *data,       // data matrix as vector
          int *nor,        // number of response patterns
          int *structure,  // knowledge structure
          int *nos,        // number of knowledge states
          int *distvec)   // resulting vector of distances
{
  long *fs_data, *fs_structure;
  register int i, j, k, min;

  init_bit_count();

  fs_data = import_famset(*noi, *nor, data);
  fs_structure = import_famset(*noi, *nos, structure);

  temp = R_Calloc(word_no(*noi), long);

  for (i=0; i<*noi; i++) distvec[i] = 0;

  for (i=0; i<*nor; i++) {
    min = *noi;
    for (j=0; j<*nos; j++) {
      if ((k=set_dist(fs_data+word_no(*noi)*i,
                      fs_structure+word_no(*noi)*j,
                      word_no(*noi)))
            < min) {
        min = k;
      }
    }
    distvec[min]++;
  }
  R_Free(temp);
}
