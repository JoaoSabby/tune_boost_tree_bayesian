# Limbo ask/tell adapter contract

`TuneBoostTreeBayesian()` uses Limbo through a file-based ask/tell bridge so cross-validation stays in R while candidate generation runs in a high-performance C++ executable built with [`resibots/limbo`](https://github.com/resibots/limbo).

## Invocation

Configure the executable with:

```r
Sys.setenv(TBTB_LIMBO_COMMAND = "/path/to/tbtb-limbo-ask")
```

or pass `TuneBoostTreeLimbo(command = "/path/to/tbtb-limbo-ask")`. The executable is called once for each model-based ask:

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

### `observations.csv`

Columns:

- `learn_rate`
- `tree_depth`
- `min_n`
- `sample_size`
- `mtry`
- `loss_reduction`
- `max_bin`
- `Value`

### `config.csv`

Columns:

- `acq`: acquisition label.
- `kappa`: exploration parameter.
- `eps`: improvement jitter.
- `seed`: deterministic seed from R.
- `iteration`: 1-based ask iteration.

## Output file

The executable must create `candidate.csv` with exactly one row and all seven parameter columns:

```csv
learn_rate,tree_depth,min_n,sample_size,mtry,loss_reduction,max_bin
0.04,6,12,0.85,0.7,0.1,128
```

R validates all values, clamps them to configured bounds, rounds `tree_depth`, `min_n`, and `max_bin`, and only then evaluates cross-validation.

## Safety behavior

- If `command` is unset and `fallback = TRUE`, R uses the internal safe optimizer and emits a warning.
- If `fallback = FALSE`, an unset or invalid command aborts before expensive CV work begins.
- If the executable exits non-zero, omits `candidate.csv`, writes more than one candidate, omits a required column, or writes a non-finite value, tuning aborts with an explicit error.
