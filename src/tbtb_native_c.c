#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

typedef struct {
  double predicted;
  int actual;
  R_xlen_t index;
} TbtbPair;

static int tbtb_compare_pair(const void *a, const void *b) {

  const TbtbPair *pa = (const TbtbPair *)a;
  const TbtbPair *pb = (const TbtbPair *)b;
  if (pa->predicted > pb->predicted) return -1;
  if (pa->predicted < pb->predicted) return 1;
  if (pa->index < pb->index) return -1;
  if (pa->index > pb->index) return 1;
  return 0;
}

SEXP tbtb_pr_auc_c(SEXP actual_sexp, SEXP predicted_sexp) {

  if (XLENGTH(actual_sexp) != XLENGTH(predicted_sexp) || XLENGTH(actual_sexp) < 1) {
    return ScalarReal(NA_REAL);
  }

  const R_xlen_t n = XLENGTH(actual_sexp);
  const int *actual = INTEGER(actual_sexp);
  const double *predicted = REAL(predicted_sexp);
  R_xlen_t positives = 0;
  TbtbPair *pairs = (TbtbPair *)R_alloc((size_t)n, sizeof(TbtbPair));

  for (R_xlen_t i = 0; i < n; ++i) {
    if (actual[i] == NA_INTEGER || !R_FINITE(predicted[i])) {
      return ScalarReal(NA_REAL);
    }
    if (actual[i] == 1) positives++;
    pairs[i].predicted = predicted[i];
    pairs[i].actual = actual[i];
    pairs[i].index = i;
  }

  if (positives == 0) return ScalarReal(NA_REAL);

  qsort(pairs, (size_t)n, sizeof(TbtbPair), tbtb_compare_pair);

  long double score = 0.0L;
  long double tp = 0.0L;
  long double fp = 0.0L;
  long double last_recall = 0.0L;
  const long double positive_count = (long double)positives;

  for (R_xlen_t i = 0; i < n; ++i) {
    if (pairs[i].actual == 1) {
      tp += 1.0L;
    } else {
      fp += 1.0L;
    }
    const long double recall = tp / positive_count;
    const long double precision = tp / (tp + fp);
    score += (recall - last_recall) * precision;
    last_recall = recall;
  }

  return ScalarReal((double)score);
}
