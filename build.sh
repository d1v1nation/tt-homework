#!/bin/bash

__args=""
H1='hw1'
H1src='hw1.mli hw1.ml'
H1c='hw1.cmo'
H2='hw2'
H2src='hw1_reduction.mli hw1_reduction.ml'
H2c='hw1_reduction.cmo'
H3='hw3'
H3src='hw2_unify.mli hw2_unify.ml'
H3c='hw2_unify.cmo'
H4='hw4'
H4src='hw2_inference.mli hw2_inference.ml'
H4c='hw2_inference.cmo'


once() {
    var="_guard_$@"
    val=${!var}
    if [ -z $val ]; then
        echo "running: $@"
        eval $@
        echo "done: $@"
    fi
    eval "$var=yes"
}

deps() {
    if [ $# -ne 0 ]; then
        echo "depends: $@"
        for i in $@ 
        do
            once $i
        done
    fi
}

collect() {
    for i in $@
    do
        __args="${__args} $i"
    done
    #~ echo "collected args: $__args"
}

compile() {
    echo "ocamlc $__args -c $@"
    ocamlc $__args -c $@
}

assemble() {
    echo "ocamlc $__args "${@:2}" -o $1"
    ocamlc $__args "${@:2}" -o $1
}

run_util() {
    deps 
    echo "running: util/build.sh $@"
    cd util
    ./build.sh $@
    cd ..
}

make_H1() {
    deps 
    
    compile $H1src
}

assemble_H1() {
    deps make_H1
    
    assemble $H1 $H1c
}

make_H2() {
    deps
        
    compile $H2src
}

assemble_H2() {
    deps make_H1 make_H2
    
    assemble $H2 $H1c $H2c
}

make_H3() {
    deps
    
    compile $H3src
}

assemble_H3() {
    deps make_H3 make_H2 make_H1
    
    
    assemble $H3 $H1c $H2c $H3c
}

make_H4() {
    deps
    
    compile $H4src
}

assemble_H4() {
    deps make_H4 make_H3 make_H2 make_H1
    
    assemble $H4 $H1c $H2c $H3c $H4c
}

help_z() {
    echo "targets: assemble, make, clean, purge, help"
    echo "usage: ./build.sh <target>"
}

help() {
    echo "targets: assemble, make, clean, purge, help"
    echo "usage: ./build.sh <target>"
    echo ""
    echo "assemble: builds hw[1-4]"
    echo "make: assemble + clean"
    echo "clean: removes ./cm*"
    echo "purge: removes ./cm*, hw[1-4]"
    echo "help: duh."
}

_t_clean() {
    run_util clean
    shopt -s nullglob
    list=(./*.cm*)
    for i in ${list[@]}
    do
        echo "deleting: $i"
    done
    rm -f ./*.cm*
}

purge() {    
    echo "purging: $@"
    rm -f $@
}

_t_assemble() {
    run_util make
    collect '-I ./util/ lambda.cmo lexer.cmo parser.cmo'
    deps assemble_H1 assemble_H2 assemble_H3 assemble_H4
}

case "$1" in
    assemble)
        _t_assemble
        ;;
    make)
        _t_assemble
        _t_clean
        ;;
    clean)
        _t_clean
        ;;
    purge)
        _t_clean 
        purge $H1 $H2 $H3 $H4
        ;;
    help)
        help
        ;;
    *)
        help_z
        ;;
esac

echo "done."
