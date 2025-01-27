#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern void constr(int *noi,
                   int *nob,
                   int *bas,
                   int *nos);

static R_NativePrimitiveArgType constr_paramtypes[] = {
  INTSXP, INTSXP, VECSXP, INTSXP
};
static const R_CMethodDef cMethods[] = {
  {"constr", (DL_FUNC) &constr, 4, constr_paramtypes},
  {0,0,0,0}
};


extern void constr_free_memory(void);

// static R_NativePrimitiveArgType constr_free_memory_paramtypes[] = {};

static const R_CMethodDef cfMethods[] = {
  {"constr_free_memory", (DL_FUNC) &constr_free_memory, 4, NULL},
  {0,0,0,0}
};


extern void constr_results(int *spc);

static R_NativePrimitiveArgType constr_results_paramtypes[] = {VECSXP};

static const R_CMethodDef crMethods[] = {
  {"constr_results", (DL_FUNC) &constr_results, 4, constr_results_paramtypes},
  {0,0,0,0}
};



extern void basis_reduction(int *noi,
                            int *nos,
                            int *bas,
                            int *nob);

static R_NativePrimitiveArgType basis_paramtypes[] = {
  INTSXP, INTSXP, VECSXP, INTSXP
};
static const R_CMethodDef bMethods[] = {
  {"basis_reduction", (DL_FUNC) &basis_reduction, 4, basis_paramtypes},
  {0,0,0,0}
};

extern void basis_results(int *basis);

static R_NativePrimitiveArgType basis_results_paramtypes[] = {VECSXP};

static const R_CMethodDef brMethods[] = {
  {"basis_results", (DL_FUNC) &basis_results, 4, basis_results_paramtypes},
  {0,0,0,0}
};


extern void sf_results(int *basis, int *mins);

static R_NativePrimitiveArgType sf_results_paramtypes[] = {VECSXP, VECSXP};

static const R_CMethodDef sfrMethods[] = {
  {"sf_results", (DL_FUNC) &sf_results, 4, sf_results_paramtypes},
  {0,0,0,0}
};



extern void dist(int *noi,        // number of items
                 int *data,       // data matrix as vector
                 int *nor,        // number of response patterns
                 int *structure,  // knowledge structure
                 int *nos,        // number of knowledge states
                 int *distvec);   // resulting vector of distances

static R_NativePrimitiveArgType dist_paramtypes[] = {
  INTSXP, VECSXP, INTSXP, VECSXP, INTSXP, VECSXP
};
static const R_CMethodDef dMethods[] = {
  {"dist", (DL_FUNC) &dist, 4, dist_paramtypes},
  {0,0,0,0}
};




extern void generate(int *noi,
                     int *norp,
                     int *data,
                     int *t,
                     int *rc,
                     int *nos);

static R_NativePrimitiveArgType generate_paramtypes[] = {
  INTSXP, INTSXP, VECSXP, INTSXP, INTSXP, INTSXP
};
static const R_CMethodDef genMethods[] = {
  {"generate", (DL_FUNC) &generate, 6, generate_paramtypes},
  {0,0,0,0}
};


extern void generate_free_memory(void);

// static R_NativePrimitiveArgType constr_free_memory_paramtypes[] = {};

static const R_CMethodDef genfMethods[] = {
  {"generate_free_memory", (DL_FUNC) &generate_free_memory, 4, NULL},
  {0,0,0,0}
};


extern void generate_results(int *kstruct);

static R_NativePrimitiveArgType generate_results_paramtypes[] = {VECSXP};

static const R_CMethodDef genrMethods[] = {
  {"generate_results", (DL_FUNC) &generate_results, 4, generate_results_paramtypes},
  {0,0,0,0}
};





void R_init_kstMatrixCconstr(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, cfMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, crMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, dMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, bMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, brMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, sfrMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, genMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, genfMethods, NULL, NULL, NULL);
  R_registerRoutines(dll, genrMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
