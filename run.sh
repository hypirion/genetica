#! /bin/sh
make --quiet compile
erl -noshell -s main start Jean -s init stop
