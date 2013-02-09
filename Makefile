.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean
MODS = main conversions one_max utils selection_protocols selection_mechanisms

all: compile

compile: ${MODS:%=%.beam}

purge:
	-rm -rf *.beam erl_crash.dump *~
