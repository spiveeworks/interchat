# Simple deps are ones whose source directory is in deps/<name>/src
#SIMPLE_DEPS := ?
#DEPS := $(SIMPLE_DEPS) ?
#DEPDIRS := $(patsubst %,deps/%,$(DEPS))

all: objects

# Single rule for all beam files: find an erlang file with the same name, and
# compile it. We could split this into each dependency's ebin directory, and
# then copy them all over to some other location, or do some
# application/release bundling, but whatever.

vpath %.erl src

ebin/%.beam: %.erl | ebin
	@echo Compiling $<
	@erlc -o ebin -pa ebin $<

ebin:
	mkdir ebin

objects: $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

# Other PHONY targets;
# `make run`         compiles everything, starts all of the applications
#                    one after another, and puts you in an erlang environment.
#                    i.e. it starts the website!
#
# `make clean`      deletes all .beam files, to quickly get you out of
#                   incremental compilation hell, or just to get rid of old
#                   files.
#
# `make distclean`  like `make clean`, but also deletes dependency repos
#                   entirely, so that you can/have to run `make pulldeps`
#                   again.

run: all
	@echo Running client...
	@erl -noshell -pa ebin -run interchat_client -s init stop

run_server: all
	@echo Running server...
	@erl -pa ebin -name interchat@localhost -run interchat_server

clean:
	rm -rf ebin

#distclean:
#	rm -rf ebin/*.beam
#	rm -rf deps

# More than twice as many phony targets as actual targets, lol.
.PHONY: all testdeps pulldeps behaviours $(DEPS) objects run distclean clean
