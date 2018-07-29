#!/bin/bash
COUNT=1
# bash until loop
until [ $COUNT -gt 8 ]; do
	echo $COUNT
        echo `stack exec -- SugarScapeSTMTArray +RTS -N3 > dump.txt 2>> 50x50_500_3_core_retries.txt`
        let COUNT=COUNT+1
done 
