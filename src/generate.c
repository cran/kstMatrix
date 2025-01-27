#include <R.h>
#include <Rinternals.h>
#include "kstMatrix.h"


unsigned long *gen_count;
int gen_items, gen_patterns, gen_states, threshold;

void generate(int *noi,
              int *norp,
              int *data,
              int *t,
              int *rc,
              int *nos)
{
  register int i;
  long *patterns;

  gen_items = *noi;
  gen_patterns = *norp;
  threshold = *t;

  if (*noi > BITS_PER_LONG-1) {
    *rc = 1;
    return;
  }

  if (!(gen_count = (unsigned long *)R_Calloc(1<<gen_items,long))) {
    *rc = 2;
    return;
  }
  patterns = import_famset(gen_items, gen_patterns, data);
  for (i=0; i<gen_patterns; i++)
    gen_count[patterns[i]]++;
  gen_states = 0;
  for (i=1; i<((1<<gen_items)-1); i++) {
    if (gen_count[i]>=threshold) {
      gen_states++;
    }
  }
  gen_states += 2;
  *nos = gen_states;
}


void generate_free_memory(void)
{
  R_Free(gen_count);
}



void generate_results(int *kstruct)
{
  register long i, j, k;

  for (i=0; i<gen_items*gen_states; i++)
    kstruct[i] = 0;
  k = 1;
  for (i=1; i<((1<<gen_items)-1); i++) {
    if (gen_count[i]>=threshold) {
      for (j=0; j<gen_items; j++) {
        if (i & (1 << word_pos(j))) {
          kstruct[(j*gen_states)+k] = 1;
          // kstruct[(k*gen_items)+j] = 1;
        }
      }
      k++;
    }
  }
  for (j=0; j<gen_items; j++)
    kstruct[(j*gen_states)+k] = 1;

  generate_free_memory();
}
