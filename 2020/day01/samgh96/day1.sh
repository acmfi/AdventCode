#!/bin/bash

INPUT=$1
ARGS=$#

function check {

    if [[ $ARGS -lt 1 ]]; then
        echo "Usage: ./day1.sh input.txt"
        exit 1
    fi
    
    if [[ ! -f $INPUT ]]; then
        echo "File not found!"
        exit 1
    fi

}

function star1 {
    CNT=2
    CURRENT_VAL=$(sed -n 1p "$INPUT")
    RET=0
    
    while [ $RET -eq 0 ]; do
        
        for i in $(sed -n ${CNT},"${INPUT_LINES}"p "$INPUT"); do
            
            if [[ $((CURRENT_VAL + i)) -eq 2020 ]]; then
                RET=$((CURRENT_VAL * i))
                echo "STAR 1 SOLUTION: $RET"
                break
            fi
        
        done
        
        CURRENT_VAL=$(sed -n ${CNT}p "$INPUT")
        CNT=$((CNT + 1))
        
    done
}

function star2 {
    CNT=2
    CURRENT_VAL=$(sed -n 1p "$INPUT")
    RET=0
    
    while [ $RET -eq 0 ]; do
        
        for i in $(sed -n ${CNT},"${INPUT_LINES}"p "$INPUT"); do

            for j in $(sed -n $((CNT + 1)),"${INPUT_LINES}"p "$INPUT"); do
                
                if [[ $((CURRENT_VAL + i + j)) -eq 2020 ]]; then
                    RET=$((CURRENT_VAL * i * j))
                    echo "STAR 2 SOLUTION: $RET"
                    break
                fi

            done

            if [ $RET -ne 0 ]; then
                break
            fi
            
        done
        
        CURRENT_VAL=$(sed -n ${CNT}p "$INPUT")
        CNT=$((CNT + 1))
        
    done
}


check

INPUT_LINES=$(wc --lines "$INPUT" | cut -d " " -f1)

star1
star2
