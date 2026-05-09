# Limbo ask/tell adapter contract

`TuneBoostTreeBayesian()` can call an external Limbo-compatible ask/tell executable, while still providing a package-native Bayesian optimizer fallback when the executable is unavailable and `fallback = TRUE`.

## Invocation

Configure the executable with:

```r
Sys.setenv(TBTB_LIMBO_COMMAND = "/path/to/tbtb-limbo-ask")
```

or pass `TuneBoostTreeLimbo(command = "/path/to/tbtb-limbo-ask")`. If neither is set, the package also checks `system.file("bin", "tbtb-limbo-ask", package = "TuneBoostTreeBayesian")`.

The installer writes a packaged reference adapter to `PREFIX/bin/tbtb-limbo-ask` by default, after cloning/building the upstream Limbo library. Disable that behavior with `--no-reference-adapter` if you provide your own adapter.

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

## Oracle Linux 9 installation smoke test

For an Oracle Linux 9 host, run the installer with the default system dependency step:

```bash
bash inst/scripts/install_limbo.sh --prefix "$HOME/.local/tbtb-limbo"
```

The script now supports `dnf`, `yum`, and `apt-get`. On Oracle Linux 9 it tries to enable the CodeReady Builder repository and Oracle EPEL before installing the C++ build dependencies used by Limbo.

If you only want to validate the installer without cloning or compiling Limbo, run:

```bash
bash -n inst/scripts/install_limbo.sh
bash inst/scripts/install_limbo.sh --dry-run --no-system-deps --no-profile --no-renviron --prefix /tmp/tbtb-limbo --timeout 30
```

A successful dry run prints the commands it would execute and the `TBTB_LIMBO_COMMAND` value that R should use.

## How to confirm Limbo tests passed

From the repository root, use one of these checks:

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-limbo-bridge.R")'
Rscript -e 'testthat::test_dir("tests/testthat", filter = "limbo")'
Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")'
```

The expected result is a zero exit status and testthat output with no failed expectations.

In GitHub Actions, open the `R CI` workflow run for the pull request or branch and check these places:

1. The `Validate Limbo installer dry run` step must be green. Its log must show `TBTB_LIMBO_COMMAND`, and the workflow summary contains a `Limbo installer dry run` section with the captured dry-run output.
2. The `Install Limbo and reference ask/tell adapter` step must be green. This performs the non-dry-run Limbo clone/build path in the Oracle Linux container, installs the packaged `tbtb-limbo-ask` reference adapter, checks that it is executable, and exports `TBTB_LIMBO_COMMAND` for later steps.
3. The `Run Limbo bridge tests` step must be green. This runs `tests/testthat/test-limbo-bridge.R` explicitly so Limbo bridge failures are visible without opening the full package check log.
4. The `Run real Limbo optimizer integration` step must be green and its log must contain `TBTB_LIMBO_REAL_INTEGRATION_OK`. This installs the package, runs a small `TuneBoostTree()` optimization with `TuneBoostTreeOptimizerLimbo(command = Sys.getenv("TBTB_LIMBO_COMMAND"), fallback = FALSE)`, and verifies that the result has a finite score, Limbo optimizer configuration, evaluation log, and threshold summary.
5. The `Check` step must be green. This runs `rcmdcheck`, which also runs the package testthat suite through `tests/testthat.R`.

The unit tests use a fake `tbtb-limbo-ask` executable to validate the package ask/tell contract deterministically. The real integration step uses the installed reference ask/tell executable through the same external-command path and disables fallback, so a missing or broken command fails the workflow.
