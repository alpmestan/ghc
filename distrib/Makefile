MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

include mk/install.mk

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

.PHONY: install
install:
	cp settings lib/settings
	cp -R bin $(prefix)/
	cp -R lib $(prefix)/
