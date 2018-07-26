#!/bin/bash
COUNT=1
# bash until loop
until [ $COUNT -gt 8 ]; do
	echo $COUNT
        echo `stack exec -- SugarScapeSTMTVar +RTS -N4 > dump.txt 2>> 50x50_2500_4_core_rebirth_retries.txt`
        let COUNT=COUNT+1
done 
