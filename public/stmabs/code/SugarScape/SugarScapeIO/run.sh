#!/bin/bash
COUNT=1
# bash until loop
until [ $COUNT -gt 8 ]; do
	echo $COUNT
        echo `stack exec -- SugarScapeIO +RTS -N1 > dump.txt`
        let COUNT=COUNT+1
done 
