.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean
MODS = main conversions utils selection one_max random_max blotto

all: compile

compile: ${MODS:%=%.beam}

purge:
	-rm -rf *.beam erl_crash.dump *~
