# Limbo ask/tell adapter contract

`TuneBoostTreeBayesian()` can call an external Limbo-compatible ask/tell executable, while still providing a package-native Bayesian optimizer fallback when the executable is unavailable and `fallback = TRUE`.

## Invocation

Configure the executable with:

```r
Sys.setenv(TBTB_LIMBO_COMMAND = "/path/to/tbtb-limbo-ask")
```

or pass `TuneBoostTreeLimbo(command = "/path/to/tbtb-limbo-ask")`. If neither is set, the package also checks `system.file("bin", "tbtb-limbo-ask", package = "TuneBoostTreeBayesian")`.

The executable is called once for each model-based ask:

```bash
tbtb-limbo-ask bounds.csv observations.csv config.csv candidate.csv
```

## Input files

### `bounds.csv`

Columns:

- `parameter`: parameter name.
- `lower`: lower bound.
- `upper`: upper bound.
- `type`: `double` or `integer`.

The default HPC profile optimizes only:

- `learn_rate`
- `tree_depth`
- `min_n`
- `loss_reduction`
- `sample_size`

### `observations.csv`

Columns are the active parameter names followed by `Value`, the cross-validated PR-AUC score to maximize.

### `config.csv`

Columns:

- `acq`: acquisition label.
- `kappa`: exploration parameter.
- `eps`: improvement jitter.
- `seed`: deterministic seed from R.
- `iteration`: 1-based ask iteration.

## Output file

The executable must create `candidate.csv` with exactly one row and every active parameter column present in `bounds.csv`. For the default search space this includes:

```csv
learn_rate,tree_depth,min_n,loss_reduction,sample_size
0.04,6,12,0.1,0.85
```

R validates all values, clamps them to configured bounds, rounds integer parameters such as `tree_depth` and `min_n`, and only then evaluates cross-validation.

## Safety behavior

- If `command` is unset or non-executable and `fallback = TRUE`, R uses the package-native Bayesian optimizer and emits a warning.
- If `fallback = FALSE`, an unset or invalid command aborts before expensive CV work begins.
- If the executable exits non-zero, times out, omits `candidate.csv`, writes more than one candidate, omits a required column, or writes a non-finite value, tuning aborts with an explicit error.
- `TBTB_LIMBO_TIMEOUT` controls the per-ask timeout in seconds; the default is 600 seconds.
