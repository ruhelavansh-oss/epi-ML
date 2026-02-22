#!/usr/bin/env bash
set -euo pipefail

# Optional local-at-rest decryption helper.
# Expects encrypted tarball at data/private/encrypted/pumf_bundle.tar.gz.enc
# Writes decrypted data into data/private/runtime/ (ignored by git).

PROJECT_ROOT="${CPADS_PROJECT_ROOT:-$(pwd)}"
ENC_FILE="${PROJECT_ROOT}/data/private/encrypted/pumf_bundle.tar.gz.enc"
RUNTIME_DIR="${PROJECT_ROOT}/data/private/runtime"
KEY="${CPADS_SECRET_KEY:-}"

if [[ -z "${KEY}" ]]; then
  echo "CPADS_SECRET_KEY is not set." >&2
  exit 1
fi
if [[ ! -f "${ENC_FILE}" ]]; then
  echo "Encrypted file not found: ${ENC_FILE}" >&2
  exit 1
fi

mkdir -p "${RUNTIME_DIR}"
TMP_TAR="$(mktemp /tmp/cpads_bundle.XXXXXX.tar.gz)"

openssl enc -aes-256-cbc -pbkdf2 -d -in "${ENC_FILE}" -out "${TMP_TAR}" -pass "pass:${KEY}"
tar -xzf "${TMP_TAR}" -C "${RUNTIME_DIR}"
rm -f "${TMP_TAR}"

echo "Data unlocked into ${RUNTIME_DIR}"
