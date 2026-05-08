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
  --no-system-deps         Do not install OS packages with apt-get
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

log() {
  printf '[install-limbo] %s\n' "$*"
}

warn() {
  printf '[install-limbo][WARN] %s\n' "$*" >&2
}

run() {
  if [[ "${DRY_RUN}" == "1" ]]; then
    printf '[install-limbo][dry-run]'
    printf ' %q' "$@"
    printf '\n'
  else
    "$@"
  fi
}

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

install_apt_deps() {
  if [[ "${INSTALL_SYSTEM_DEPS}" != "1" ]]; then
    log "Skipping OS dependency installation (--no-system-deps)."
    return 0
  fi

  if ! command -v apt-get >/dev/null 2>&1; then
    warn "apt-get not found; install Limbo dependencies manually for your OS."
    return 0
  fi

  local sudo_cmd=()
  if [[ "$(id -u)" -ne 0 ]]; then
    if ! command -v sudo >/dev/null 2>&1; then
      warn "sudo not found; skipping apt-get dependency installation."
      return 0
    fi
    sudo_cmd=(sudo)
  fi

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
}

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

  if [[ ! -x "${ADAPTER_COMMAND}" ]]; then
    warn "${ADAPTER_COMMAND} is not executable yet. Build or install a tbtb-limbo-ask adapter there, or rerun with --adapter-command PATH."
  fi
}

PREFIX="${HOME}/.local/tbtb-limbo"
LIMBO_REF="release-2.1"
LIMBO_REPO="https://github.com/resibots/limbo.git"
ADAPTER_COMMAND=""
TBTB_LIMBO_TIMEOUT_VALUE="600"
INSTALL_SYSTEM_DEPS="1"
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

install_apt_deps
clone_or_update_limbo
build_limbo
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
compatible ask/tell executable. Limbo itself is only the C++ optimization library.
EOF_DONE
