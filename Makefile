ELPA_DEPENDENCIES=package-lint let-alist

ELPA_ARCHIVES=melpa-stable gnu

TEST_ERT_FILES		= test/libmpdel-directory-test.el test/libmpdel-test.el
LINT_CHECKDOC_FILES	= libmpdel-directory.el  libmpdel.el ${TEST_ERT_FILES}
LINT_PACKAGE_LINT_FILES	= ${LINT_CHECKDOC_FILES}
LINT_COMPILE_FILES	= ${LINT_CHECKDOC_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://github.com/DamienCassou/makel/raw/v0.8.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
