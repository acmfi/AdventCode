#!/bin/bash

INPUT=$1
ARGS=$#

function check {

    if [[ $ARGS -lt 1 ]]; then
        echo "Usage: ./day02.sh input.txt"
        exit 1
    fi
    
    if [[ ! -f $INPUT ]]; then
        echo "File not found!"
        exit 1
    fi

}

function star1 {
    CNT=0
    
    while IFS= read -r line; do
        LOWER=$(echo "$line" | cut -d " " -f1 | cut -d "-" -f1)
        UPPER=$(echo "$line" | cut -d " " -f1 | cut -d "-" -f2)
        CHAR=$(echo "$line" | cut -d " " -f2 | cut -d ":" -f1)
        PASS=$(echo "$line" | cut -d " " -f3)

        OCURRENCES=$(tr -dc $CHAR <<< $PASS | awk '{ print length; }')
        
        if [[ $OCURRENCES -ge $LOWER ]] && [[ $OCURRENCES -le $UPPER ]]; then
            CNT=$((CNT + 1))
        fi
        
    done < "$INPUT"

    echo "STAR 1 SOLUTION: $CNT"
}

function star2 {
    CNT=0
    
    while IFS= read -r line; do
        FIRST=$(echo "$line" | cut -d " " -f1 | cut -d "-" -f1)
        SECOND=$(echo "$line" | cut -d " " -f1 | cut -d "-" -f2)
        CHAR=$(echo "$line" | cut -d " " -f2 | cut -d ":" -f1)
        PASS=$(echo "$line" | cut -d " " -f3)

        POS1=$(expr substr "$PASS" "$FIRST" 1)
        POS2=$(expr substr "$PASS" "$SECOND" 1)

        OCURRENCES=0
        
        if [[ $POS1 == $CHAR ]]; then
            OCURRENCES=$((OCURRENCES + 1))
        fi

        if [[ $POS2 == $CHAR ]]; then
            OCURRENCES=$((OCURRENCES + 1))
        fi
        
        if [[ $OCURRENCES == 1 ]]; then
            CNT=$((CNT + 1))
        fi
        
    done < "$INPUT"

    echo "STAR 2 SOLUTION: $CNT"
}

check
star1
star2
