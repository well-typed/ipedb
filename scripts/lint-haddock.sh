#!/bin/sh -e

# NOTE: Haddock cannot reliably find link destinations for data families.
#       See: https://github.com/haskell/haddock/issues/1071
#
#       As a workaround, this script runs haddock, greps for symbols matching
#       Rep_, and reruns haddock with corresponding --ignore-link-symbol flags.

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal:
#
# 1. Use CABAL if it is set.
# 2. Look for cabal-$EXPECTED_VERSION.
# 3. Look for cabal.
#
if [ "${CABAL}" = "" ]; then
	if ! CABAL="$(which "cabal-${EXPECT_VERSION}")"; then
		if ! CABAL="$(which "cabal")"; then
			echo "Requires cabal ${EXPECT_VERSION}; no version found"
			exit 1
		fi
	fi
fi

# Check cabal version:
ACTUAL_VERSION="$("${CABAL}" --numeric-version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
	# Version mismatch is never an error:
	echo "Requires cabal ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
fi

# Lint Haskell files
echo "Lint Haskell files with Haddock"
echo

# Temporary files for haddock's output and error streams:
HADDOCK_OUT=$(mktemp)
HADDOCK_ERR=$(mktemp)

# POSIX compliant method for 'pipefail':
FAIL=$(mktemp)

# Build haddock documentation:
if ! ${CABAL} haddock all 2>"${HADDOCK_ERR}" >"${HADDOCK_OUT}"; then
	cat "${HADDOCK_ERR}"
	cat "${HADDOCK_OUT}"
	printf "\nBuilding haddock failed.\n"
	printf "\n" >"${FAIL}"
fi

# Check haddock output for unlinked Rep_ symbol warnings.
SPURIOUS_WARNING_NUM="$(grep -c "Rep_" "${HADDOCK_OUT}" || true)"
if test "${SPURIOUS_WARNING_NUM}" -gt 0; then
	# Construct list of --ignore-link-symbol flags:
	IGNORE_LINK_SYMBOL_FLAGS="$(
		cat "${HADDOCK_OUT}" |
			grep 'Rep_' |
			sed 's/^\t- /--haddock-option=--ignore-link-symbol=/' |
			tr '\n' ' '
	)"

	# Rebuild haddock documentation with --ignore-link-symbol flags:
	# shellcheck disable=SC2086
	if ! ${CABAL} haddock all ${IGNORE_LINK_SYMBOL_FLAGS} 2>"${HADDOCK_ERR}" >"${HADDOCK_OUT}"; then
		cat "${HADDOCK_ERR}"
		cat "${HADDOCK_OUT}"
		printf "\nBuilding haddock failed.\n"
		printf "\n" >"${FAIL}"
	fi
fi

# Check haddock output for warnings:
WARNING_NUM="$(grep -c "Warning:" "${HADDOCK_OUT}" || true)"
if test "${WARNING_NUM}" -gt 0; then
	cat "${HADDOCK_ERR}"
	cat "${HADDOCK_OUT}"
	printf "Haddock found %s problems.\n" "${WARNING_NUM}"
	printf "\n" >"${FAIL}"
else
	printf "Haddock found no problems.\n"
fi

# Check whether or not any subcommand failed:
if [ -s "${FAIL}" ]; then
	rm "${HADDOCK_OUT}" "${HADDOCK_ERR}" "${FAIL}"
	exit 1
else
	rm "${HADDOCK_OUT}" "${HADDOCK_ERR}" "${FAIL}"
	exit 0
fi
