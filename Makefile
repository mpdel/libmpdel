PACKAGE_BASENAME = libmpdel

CURL = curl --fail --silent --show-error --insecure --location --retry 9 --retry-delay 9
GITHUB = https://raw.githubusercontent.com

export CI=false

EMAKE_SHA1=4323e76b4bf2c78c54e8d78f794ddf26898743de

emake.mk:
	$(CURL) -O ${GITHUB}/vermiculus/emake.el/${EMAKE_SHA1}/emake.mk

# Include emake.mk if present
-include emake.mk

.PHONY: check lint test

check: lint test

lint: PACKAGE_LISP += $(PACKAGE_TESTS)
lint: lint-checkdoc lint-package-lint compile

test: test-ert
