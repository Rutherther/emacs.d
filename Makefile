EMACS=emacs
PWD=$(abspath ./)/

.PHONY: run
run:
	$(EMACS) -q --exec="(progn \
		(setq user-emacs-directory \"$(PWD)\") \
		(setq user-init-file (locate-user-emacs-file \"init.el\")) \
	)"
