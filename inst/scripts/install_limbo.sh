#!/usr/bin/env bash
set -Eeuo pipefail


usage() {

  cat <<'USAGE'
Usage: install_limbo.sh [options]

Installs/builds resibots/limbo and writes the environment variables used by
TuneBoostTreeBayesian's external Limbo ask/tell bridge.

Options:
  --prefix DIR             Installation root (default: $HOME/.local/tbtb-limbo)
  --branch REF             Limbo git branch/tag/ref (default: release-2.1)
  --repo URL               Limbo git repository (default: https://github.com/resibots/limbo.git)
  --adapter-command PATH   Path to the tbtb-limbo-ask executable to expose through TBTB_LIMBO_COMMAND
                           (default: PREFIX/bin/tbtb-limbo-ask)
  --timeout SECONDS        TBTB_LIMBO_TIMEOUT value (default: 600)
  --no-system-deps         Do not install OS packages with apt-get, dnf, or yum
  --no-reference-adapter   Do not install the packaged tbtb-limbo-ask reference adapter
  --no-profile             Do not update ~/.profile with shell exports
  --no-renviron            Do not update ~/.Renviron for R sessions
  --dry-run                Print commands/edits without running them
  -h, --help               Show this help

Examples:
  ./inst/scripts/install_limbo.sh
  ./inst/scripts/install_limbo.sh --prefix /opt/tbtb-limbo --adapter-command /opt/tbtb-limbo/bin/tbtb-limbo-ask

Important:
  Limbo is a C++ Bayesian optimization library. TuneBoostTreeBayesian calls an
  external ask/tell executable, not Limbo directly. This script builds Limbo and
  configures TBTB_LIMBO_COMMAND, but you still need a compatible executable at
  that path. See inst/limbo/README.md for the required CSV contract.
USAGE
}
####
## Fim
#

log() {

  printf '[install-limbo] %s\n' "$*"
}
####
## Fim
#

warn() {

  printf '[install-limbo][WARN] %s\n' "$*" >&2
}
####
## Fim
#

run() {

  if [[ "${DRY_RUN}" == "1" ]]; then
    printf '[install-limbo][dry-run]'
    printf ' %q' "$@"
    printf '\n'
  else
    "$@"
  fi
}
####
## Fim
#

append_or_replace_kv() {

  local file="$1"
  local key="$2"
  local value="$3"
  local mode="$4"
  local tmp

  if [[ "${DRY_RUN}" == "1" ]]; then
    log "Would set ${key} in ${file}"
    return 0
  fi

  mkdir -p "$(dirname "${file}")"
  touch "${file}"
  tmp="$(mktemp)"
  if [[ "${mode}" == "renviron" ]]; then
    awk -v key="${key}" 'index($0, key "=") != 1 { print }' "${file}" > "${tmp}"
    printf '%s=%s\n' "${key}" "${value}" >> "${tmp}"
  else
    awk -v key="${key}" '$0 !~ "^export " key "=" { print }' "${file}" > "${tmp}"
    printf 'export %s=%q\n' "${key}" "${value}" >> "${tmp}"
  fi
  mv "${tmp}" "${file}"
}
####
## Fim
#

install_system_deps() {

  if [[ "${INSTALL_SYSTEM_DEPS}" != "1" ]]; then
    log "Skipping OS dependency installation (--no-system-deps)."
    return 0
  fi

  local sudo_cmd=()
  if [[ "$(id -u)" -ne 0 ]]; then
    if ! command -v sudo >/dev/null 2>&1; then
      warn "sudo not found; skipping OS dependency installation."
      return 0
    fi
    sudo_cmd=(sudo)
  fi

  if command -v dnf >/dev/null 2>&1; then
    log "Installing Limbo build dependencies with dnf."
    run "${sudo_cmd[@]}" dnf -y install dnf-plugins-core || warn "Could not install dnf-plugins-core; continuing with enabled repositories."
    run "${sudo_cmd[@]}" dnf config-manager --set-enabled ol9_codeready_builder || warn "Could not enable ol9_codeready_builder; continuing with enabled repositories."
    run "${sudo_cmd[@]}" dnf -y install oracle-epel-release-el9 || warn "Could not install oracle-epel-release-el9; continuing with enabled repositories."
    run "${sudo_cmd[@]}" dnf -y install \
      gcc \
      gcc-c++ \
      make \
      cmake \
      ca-certificates \
      git \
      python3 \
      pkgconf-pkg-config \
      eigen3-devel \
      boost-devel \
      tbb-devel
    return 0
  fi

  if command -v yum >/dev/null 2>&1; then
    log "Installing Limbo build dependencies with yum."
    run "${sudo_cmd[@]}" yum -y install \
      gcc \
      gcc-c++ \
      make \
      cmake \
      ca-certificates \
      git \
      python3 \
      pkgconfig \
      eigen3-devel \
      boost-devel \
      tbb-devel
    return 0
  fi

  if command -v apt-get >/dev/null 2>&1; then
    log "Installing Limbo build dependencies with apt-get."
    run "${sudo_cmd[@]}" apt-get update
    run "${sudo_cmd[@]}" apt-get install -y \
      build-essential \
      ca-certificates \
      git \
      python3 \
      pkg-config \
      libeigen3-dev \
      libboost-filesystem-dev \
      libboost-program-options-dev \
      libboost-serialization-dev \
      libboost-system-dev \
      libboost-test-dev \
      libboost-thread-dev \
      libtbb-dev
    return 0
  fi

  warn "No supported OS package manager found; install Limbo dependencies manually for your OS."
}
####
## Fim
#

clone_or_update_limbo() {

  mkdir -p "${SRC_DIR}"
  if [[ -d "${LIMBO_DIR}/.git" ]]; then
    log "Updating existing Limbo checkout at ${LIMBO_DIR}."
    run git -C "${LIMBO_DIR}" fetch --tags origin
    run git -C "${LIMBO_DIR}" checkout "${LIMBO_REF}"
    run git -C "${LIMBO_DIR}" pull --ff-only origin "${LIMBO_REF}" || warn "Could not fast-forward ${LIMBO_REF}; continuing with checked-out ref."
  else
    log "Cloning Limbo ${LIMBO_REF} into ${LIMBO_DIR}."
    run git clone --branch "${LIMBO_REF}" --depth 1 "${LIMBO_REPO}" "${LIMBO_DIR}"
  fi
}
####
## Fim
#

build_limbo() {

  log "Configuring and building Limbo."
  run chmod +x "${LIMBO_DIR}/waf"

  local configure_args=("${LIMBO_DIR}/waf" configure)
  if [[ -d /usr/include/eigen3 ]]; then
    configure_args+=(--eigen /usr/include/eigen3)
  fi

  if [[ "${DRY_RUN}" == "1" ]]; then
    log "Would run from ${LIMBO_DIR}: ${configure_args[*]}"
    log "Would run from ${LIMBO_DIR}: ${LIMBO_DIR}/waf build"
  else
    (cd "${LIMBO_DIR}" && "${configure_args[@]}")
    (cd "${LIMBO_DIR}" && "${LIMBO_DIR}/waf" build)
  fi
}
####
## Fim
#

write_reference_adapter() {

  if [[ "${INSTALL_REFERENCE_ADAPTER}" != "1" ]]; then
    log "Skipping reference ask/tell adapter installation (--no-reference-adapter)."
    return 0
  fi

  if [[ "${DRY_RUN}" == "1" ]]; then
    log "Would install reference ask/tell adapter at ${ADAPTER_COMMAND}."
    return 0
  fi

  mkdir -p "$(dirname "${ADAPTER_COMMAND}")"
  cat > "${ADAPTER_COMMAND}" <<'PY_ADAPTER'
#!/usr/bin/env python3
import csv
import math
import random
import sys

VERSION = "tbtb-limbo-ask-reference 0.1.0"


def read_csv(path):
    with open(path, newline="") as handle:
        return list(csv.DictReader(handle))


def finite_float(value, default=None):
    try:
        out = float(value)
    except (TypeError, ValueError):
        if default is None:
            raise
        return default
    if not math.isfinite(out):
        if default is None:
            raise ValueError(value)
        return default
    return out


def normalized_distance(candidate, observation, bounds):
    total = 0.0
    for spec in bounds:
        name = spec["parameter"]
        lower = finite_float(spec["lower"])
        upper = finite_float(spec["upper"])
        width = max(upper - lower, sys.float_info.epsilon)
        total += ((candidate[name] - finite_float(observation[name], lower)) / width) ** 2
    return math.sqrt(total)


def clamp_candidate(candidate, bounds):
    out = {}
    for spec in bounds:
        name = spec["parameter"]
        lower = finite_float(spec["lower"])
        upper = finite_float(spec["upper"])
        value = min(max(float(candidate[name]), lower), upper)
        if spec.get("type") == "integer":
            value = int(round(value))
            value = min(max(value, int(round(lower))), int(round(upper)))
        out[name] = value
    return out


def propose(bounds, observations, config):
    seed = int(finite_float(config.get("seed", 42), 42))
    iteration = int(finite_float(config.get("iteration", 1), 1))
    rng = random.Random(seed + 1000003 * iteration + 9176 * len(observations))
    best = None
    if observations:
        best = max(observations, key=lambda row: finite_float(row.get("Value"), -math.inf))

    candidate_pool = []
    pool_size = 512
    for _ in range(pool_size):
        raw = {}
        for spec in bounds:
            name = spec["parameter"]
            lower = finite_float(spec["lower"])
            upper = finite_float(spec["upper"])
            width = upper - lower
            if best is not None and rng.random() < 0.65:
                center = finite_float(best.get(name), (lower + upper) / 2.0)
                value = rng.gauss(center, width / max(4.0, math.sqrt(iteration + 1.0)))
            else:
                value = rng.uniform(lower, upper)
            raw[name] = value
        candidate_pool.append(clamp_candidate(raw, bounds))

    if not observations:
        return candidate_pool[0]

    def score(candidate):
        min_distance = min(normalized_distance(candidate, row, bounds) for row in observations)
        return min_distance + 0.01 * rng.random()

    return max(candidate_pool, key=score)


def main(argv):
    if len(argv) == 2 and argv[1] == "--version":
        print(VERSION)
        return 0
    if len(argv) != 5:
        print("Usage: tbtb-limbo-ask bounds.csv observations.csv config.csv candidate.csv", file=sys.stderr)
        return 2

    bounds_file, observations_file, config_file, candidate_file = argv[1:5]
    bounds = read_csv(bounds_file)
    observations = read_csv(observations_file)
    config_rows = read_csv(config_file)
    config = config_rows[0] if config_rows else {}
    candidate = propose(bounds, observations, config)

    fieldnames = [spec["parameter"] for spec in bounds]
    with open(candidate_file, "w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerow({name: candidate[name] for name in fieldnames})
    print("candidate," + ",".join(f"{name}={candidate[name]}" for name in fieldnames))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
PY_ADAPTER
  chmod 0755 "${ADAPTER_COMMAND}"
  log "Installed reference ask/tell adapter at ${ADAPTER_COMMAND}."
}
####
## Fim
#

write_environment() {

  log "Configuring TuneBoostTreeBayesian environment variables."

  if [[ "${UPDATE_RENVIRON}" == "1" ]]; then
    append_or_replace_kv "${HOME}/.Renviron" TBTB_LIMBO_ROOT "${LIMBO_DIR}" renviron
    append_or_replace_kv "${HOME}/.Renviron" TBTB_LIMBO_COMMAND "${ADAPTER_COMMAND}" renviron
    append_or_replace_kv "${HOME}/.Renviron" TBTB_LIMBO_TIMEOUT "${TBTB_LIMBO_TIMEOUT_VALUE}" renviron
    log "Updated ${HOME}/.Renviron. Restart R or run readRenviron('~/.Renviron')."
  fi

  if [[ "${UPDATE_PROFILE}" == "1" ]]; then
    append_or_replace_kv "${HOME}/.profile" TBTB_LIMBO_ROOT "${LIMBO_DIR}" shell
    append_or_replace_kv "${HOME}/.profile" TBTB_LIMBO_COMMAND "${ADAPTER_COMMAND}" shell
    append_or_replace_kv "${HOME}/.profile" TBTB_LIMBO_TIMEOUT "${TBTB_LIMBO_TIMEOUT_VALUE}" shell
    log "Updated ${HOME}/.profile. Reload with: . ~/.profile"
  fi

  if [[ "${DRY_RUN}" != "1" && ! -x "${ADAPTER_COMMAND}" ]]; then
    warn "${ADAPTER_COMMAND} is not executable yet. Build or install a tbtb-limbo-ask adapter there, or rerun with --adapter-command PATH."
  fi
}
####
## Fim
#

PREFIX="${HOME}/.local/tbtb-limbo"
LIMBO_REF="release-2.1"
LIMBO_REPO="https://github.com/resibots/limbo.git"
ADAPTER_COMMAND=""
TBTB_LIMBO_TIMEOUT_VALUE="600"
INSTALL_SYSTEM_DEPS="1"
INSTALL_REFERENCE_ADAPTER="1"
UPDATE_PROFILE="1"
UPDATE_RENVIRON="1"
DRY_RUN="0"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix)
      PREFIX="$2"
      shift 2
      ;;
    --branch)
      LIMBO_REF="$2"
      shift 2
      ;;
    --repo)
      LIMBO_REPO="$2"
      shift 2
      ;;
    --adapter-command)
      ADAPTER_COMMAND="$2"
      shift 2
      ;;
    --timeout)
      TBTB_LIMBO_TIMEOUT_VALUE="$2"
      shift 2
      ;;
    --no-system-deps)
      INSTALL_SYSTEM_DEPS="0"
      shift
      ;;
    --no-reference-adapter)
      INSTALL_REFERENCE_ADAPTER="0"
      shift
      ;;
    --no-profile)
      UPDATE_PROFILE="0"
      shift
      ;;
    --no-renviron)
      UPDATE_RENVIRON="0"
      shift
      ;;
    --dry-run)
      DRY_RUN="1"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown option: %s\n\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if ! [[ "${TBTB_LIMBO_TIMEOUT_VALUE}" =~ ^[0-9]+$ ]] || [[ "${TBTB_LIMBO_TIMEOUT_VALUE}" -lt 1 ]]; then
  printf 'Invalid --timeout value: %s\n' "${TBTB_LIMBO_TIMEOUT_VALUE}" >&2
  exit 2
fi

PREFIX="${PREFIX/#\~/${HOME}}"
SRC_DIR="${PREFIX}/src"
LIMBO_DIR="${SRC_DIR}/limbo"
if [[ -z "${ADAPTER_COMMAND}" ]]; then
  ADAPTER_COMMAND="${PREFIX}/bin/tbtb-limbo-ask"
else
  ADAPTER_COMMAND="${ADAPTER_COMMAND/#\~/${HOME}}"
fi

log "Prefix: ${PREFIX}"
log "Limbo ref: ${LIMBO_REF}"
log "Limbo directory: ${LIMBO_DIR}"
log "TBTB_LIMBO_COMMAND: ${ADAPTER_COMMAND}"

install_system_deps
clone_or_update_limbo
build_limbo
write_reference_adapter
write_environment

cat <<EOF_DONE

Done.

Current-session exports:
  export TBTB_LIMBO_ROOT=${LIMBO_DIR@Q}
  export TBTB_LIMBO_COMMAND=${ADAPTER_COMMAND@Q}
  export TBTB_LIMBO_TIMEOUT=${TBTB_LIMBO_TIMEOUT_VALUE@Q}

R check:
  Sys.getenv(c("TBTB_LIMBO_ROOT", "TBTB_LIMBO_COMMAND", "TBTB_LIMBO_TIMEOUT"))
  file.access(Sys.getenv("TBTB_LIMBO_COMMAND"), mode = 1)

Remember: TuneBoostTreeBayesian requires TBTB_LIMBO_COMMAND to point to a
compatible ask/tell executable. By default this installer writes the packaged
reference adapter to ADAPTER_COMMAND; use --no-reference-adapter if you provide
your own adapter.
EOF_DONE
