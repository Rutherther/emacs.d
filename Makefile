EMACS=emacs
PWD=$(abspath ./)/

.PHONY: run
run:
	$(EMACS) --init-directory "$(PWD)"
