# Based on the makefile example of Joe Armstrong's book
# "Programming Erlang"
ERL = erl
ERLC = erlc

.SUFFIXES: .erl .beam

MODS = \
	src/erlagi \
	src/erlagi_debug \
	src/erlagi_defaults \
	src/erlagi_demo \
	src/erlagi_fastagi \
	src/erlagi_log \
	src/erlagi_io \
	src/erlagi_io_tcp \
	src/erlagi_io_normal \
	src/erlagi_misc \
	src/erlagi_options \
	src/erlagi_read_env \
	src/erlagi_result 

all: compile

.erl.beam:
	erlc -W -o compiled $<

prepare:
	mkdir -p compiled

compile: prepare ${MODS:%=%.beam}

clean:
	rm -rf compiled
