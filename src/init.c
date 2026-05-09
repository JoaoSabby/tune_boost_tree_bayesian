#include <R.h>
/* Objetivo: registrar rotinas nativas explicitamente para chamadas C/Fortran seguras pelo pacote R. */
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/RS.h>

extern SEXP tbtb_pr_auc_c(SEXP actual_sexp, SEXP predicted_sexp);
void F77_NAME(tbtb_pr_auc_f)(int *n, int *actual, double *predicted, double *score);

/* Objetivo: limitar os símbolos C disponíveis ao ponto de entrada usado pelo scorer PR-AUC. */
static const R_CallMethodDef call_methods[] = {
  {"tbtb_pr_auc_c", (DL_FUNC) &tbtb_pr_auc_c, 2},
  {NULL, NULL, 0}
};

/* Objetivo: registrar o backend Fortran como alternativa compilada em ambientes HPC. */
static const R_FortranMethodDef fortran_methods[] = {
  {"tbtb_pr_auc_f", (DL_FUNC) &F77_NAME(tbtb_pr_auc_f), 4},
  {NULL, NULL, 0}
};

/* Objetivo: desabilitar símbolos dinâmicos para reduzir risco de chamadas nativas acidentais. */
void R_init_TuneBoostTreeBayesian(DllInfo *dll) {

  R_registerRoutines(dll, NULL, call_methods, fortran_methods, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
