# TuneBoostTreeBayesian release and implementation checklist

Use this checklist before merging changes, publishing a release, or running long production tuning jobs.

## 1. API and argument validation

- [ ] `TuneBoostTreeBayesian()` exposes only high-level arguments: `formula`, `data`, `initial`, `nIter`, `engine`, and configuration blocks.
- [ ] `data` accepts `data.frame`, `tibble`, and `data.table`.
- [ ] Formula validation fails early for non-two-sided formulas.
- [ ] Binary target validation confirms both classes exist before training.
- [ ] Public boosting parameter names follow `parsnip::boost_tree()` where applicable.
- [ ] `initial = NULL` is accepted.
- [ ] `initial = integer(1)` creates random initial points.
- [ ] `initial = data.frame/tibble/data.table` is deduplicated as warm start.
- [ ] Warm-start grids require all parameter columns plus `Value`.

## 2. Configuration helper validation

- [ ] `TuneBoostTreeBoostParams()` returns valid `trees` and `stop_iter` defaults.
- [ ] `TuneBoostTreeSearchSpace()` validates finite increasing bounds.
- [ ] `mtry` bounds are fractions in `(0, 1]`.
- [ ] `sample_size` bounds are fractions in `(0, 1]`.
- [ ] `TuneBoostTreeCv()` rejects invalid fold counts.
- [ ] `TuneBoostTreeLimbo()` validates acquisition metadata.
- [ ] `TuneBoostTreeImbalance()` accepts only `"auto"`, `NULL`, or positive numeric `scale_pos_weight`.
- [ ] Every configuration helper that accepts a user function exposes `...` for function-specific arguments.
- [ ] `TuneBoostTreeUltraConfig()` returns a complete set of valid config lists.

## 3. Data preparation and sparse safety

- [ ] `data.frame`, `tibble`, and `data.table` subsetting works identically.
- [ ] Sparse-like columns are detected before matrix selection.
- [ ] Highly sparse numeric matrices are converted to `Matrix` sparse storage.
- [ ] Dense matrices remain dense when sparse conversion would add overhead.
- [ ] Predictor order is preserved from formula parsing through prediction.
- [ ] Prediction rejects missing columns before native engine calls.

## 4. Cross-validation and leakage prevention

- [ ] Folds are stratified by binary outcome.
- [ ] Balance functions are applied only to training partitions.
- [ ] Validation partitions are never balanced or resampled.
- [ ] Engine-native data objects are cached once per fold.
- [ ] Objective evaluations do not rebuild fold matrices unnecessarily.
- [ ] Fold-level seeds are deterministic.

## 5. Imbalance handling

- [ ] `scale_pos_weight = "auto"` computes majority/minority ratio.
- [ ] Auto weight is computed after fold balancing when `balance_fn` exists.
- [ ] Numeric `scale_pos_weight` is used exactly as provided.
- [ ] `scale_pos_weight = NULL` omits the native class-weight parameter.
- [ ] Balance function arguments in `...` are forwarded exactly once per fold.

## 6. Optimizer and Limbo bridge

- [ ] Limbo command normalization handles unset and whitespace-only commands.
- [ ] Strict Limbo mode aborts before CV if the executable is missing.
- [ ] Fallback mode emits a warning and uses the internal optimizer.
- [ ] `bounds.csv` contains parameter, lower, upper, and type columns.
- [ ] `observations.csv` contains parsnip-style parameter names and `Value`.
- [ ] `config.csv` contains acquisition, kappa, eps, seed, and iteration.
- [ ] `candidate.csv` must contain exactly one row.
- [ ] Candidate values are finite, clamped to bounds, and integer-rounded where required.
- [ ] Duplicate candidates are cached and not re-evaluated.

## 7. Parallel execution

- [ ] `parallel = "auto"` avoids CPU oversubscription.
- [ ] Small data defaults to sequential fold execution.
- [ ] Explicit `TuneBoostTreeParallel()` validates workers and threads.
- [ ] Windows uses PSOCK clusters safely.
- [ ] Unix-like systems can use forked workers.
- [ ] Native engine thread counts respect the resolved runtime limits.

## 8. Engine translation

- [ ] XGBoost receives `eta`, `max_depth`, `min_child_weight`, `subsample`, `colsample_bynode`, `gamma`, and `max_bin`.
- [ ] LightGBM receives `learning_rate`, `max_depth`, `min_sum_hessian_in_leaf`, `bagging_fraction`, `feature_fraction_bynode`, `min_gain_to_split`, and `max_bin`.
- [ ] `scale_pos_weight` is omitted when configured as `NULL`.
- [ ] XGBoost defaults to `tree_method = "hist"`.
- [ ] LightGBM uses `average_precision` for PR-AUC-compatible tuning.

## 9. Scoring

- [ ] PR-AUC returns `NA_real_` for invalid shapes or missing positives.
- [ ] `backend = "auto"` prefers C, Fortran, Rfast, then R.
- [ ] Explicit unavailable compiled backends degrade safely to R.
- [ ] Tuning and holdout scoring use compatible PR-AUC semantics.

## 10. Documentation

- [ ] README includes standard, warm-start, strict Limbo, balanced, parallel, and ultra scenarios.
- [ ] Vignette includes introduction, motivation, method, parameter documentation, and examples.
- [ ] `inst/limbo/README.md` documents the current CSV schema.
- [ ] References include Limbo, XGBoost, LightGBM, parsnip, and PR-AUC literature.
- [ ] All exported helpers have Rd documentation.

## 11. Package checks

- [ ] `R CMD build .`
- [ ] `R CMD check --as-cran TuneBoostTreeBayesian_*.tar.gz`
- [ ] `tools::checkRd()` reports no critical Rd problems.
- [ ] `codetools::checkUsagePackage("TuneBoostTreeBayesian")` reports no critical issues.
- [ ] Vignettes build successfully.
- [ ] Native C and Fortran symbols are registered and loadable.

## 12. Functional smoke tests

- [ ] Minimal LightGBM tuning runs with `initial = 1L`, `nIter = 1L`, and `folds = 2L` when LightGBM is installed.
- [ ] Minimal XGBoost tuning runs as the alternative engine with `initial = 1L`, `nIter = 1L`, and `folds = 2L`.
- [ ] Warm start using returned `initial` runs.
- [ ] Balance function with custom `...` argument runs and is called once per fold.
- [ ] `scale_pos_weight = "auto"`, numeric, and `NULL` each run.
- [ ] `parallel = FALSE`, `parallel = "auto"`, and explicit parallel config each run.
- [ ] Strict Limbo mode fails early with a clear error when command is absent.
- [ ] Fallback Limbo mode runs internal optimizer when command is absent.

## 13. Reproducibility and release

- [ ] Same seed and same environment produce identical fold assignments.
- [ ] Evaluation logs are stable enough for warm start reuse.
- [ ] NEWS/release notes summarize API-breaking changes.
- [ ] Git status is clean after documentation generation.
- [ ] Pull request includes test output and environment limitations.

## 14. Full binary-classification integration tests

- [ ] LightGBM + Limbo ask/tell executable/fake runs end-to-end on `modeldata::two_class_dat` as a tibble when `lightgbm` is installed.
- [ ] LightGBM + `rBayesianOptimization` runs end-to-end on `modeldata::two_class_dat` as a tibble when `lightgbm` is installed.
- [ ] XGBoost + Limbo ask/tell executable/fake runs end-to-end on `modeldata::two_class_dat` as a tibble.
- [ ] XGBoost + `rBayesianOptimization` runs end-to-end on `modeldata::two_class_dat` as a tibble.
- [ ] Each full integration path performs `rsample::initial_split()`, tuning on `training(split)`, final fit with `FitBoostTreeModel()`, prediction on `testing(split)`, and `yardstick` metrics: PR-AUC, ROC-AUC, sensitivity, specificity, accuracy, and balanced accuracy.
