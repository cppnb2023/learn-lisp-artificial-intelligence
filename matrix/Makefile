define SBCL_OPT
--eval "(require 'asdf)" \
--eval "(push #P\"${PWD}/\" asdf:*central-registry*)" \
--eval "(asdf:load-asd \"matrix.asd\")" \
--eval "(asdf:load-system \"matrix\")"
endef

all:
	sbcl ${SBCL_OPT}
