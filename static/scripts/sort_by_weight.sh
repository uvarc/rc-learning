#!/bin/bash

declare -A weights

files=$(ls *md)

for file in ${files[@]}; do
    my_weight=`awk '/weight/{print $2}' $file`
    if [[ "$my_weight" =~ ^[0-9]+$ ]]; then
        weights[$file]=$my_weight
    else
       continue
    fi
done

readarray -t sorted < <(printf '%s\n' "${weights[@]}" | sort -n )

for weight in "${sorted[@]}"; do
    for file in ${files[@]}; do
         if [[ $weight -eq ${weights[$file]} ]]; then 
	     echo $file $weight
	     #remove each match as it is printed (gets rid of duplicates)
	     unset weights[$file]
	 fi
	 continue
    done
done
