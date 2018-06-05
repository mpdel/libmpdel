PACKAGE_BASENAME = libmpdel

CURL = curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9
GITHUB = https://raw.githubusercontent.com

export CI=false

emake.mk:
	$(CURL) -O ${GITHUB}/vermiculus/emake.el/master/emake.mk

# Include emake.mk if present
-include emake.mk

.PHONY: check lint test

check: lint test

lint: PACKAGE_LISP += $(PACKAGE_TESTS)
lint: lint-checkdoc lint-package-lint compile

test: test-ert
