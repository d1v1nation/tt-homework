#!/bin/sh

case "$1" in
    make)
        ocamlyacc parser.mly
        ocamllex lexer.mll

        ocamlc -I .. -c lambda.mli lambda.ml lambda.cmo parser.mli parser.ml lexer.ml
        ;;
    clean)
        rm -f ./*.cm*
        ;;
    help)
        echo "targets: make, clean, help"
        ;;
esac
