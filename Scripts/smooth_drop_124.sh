#!/bin/bash


for i in {81..124}
do

        # Grab file from list of files for analysis
        F=`head -$i files.txt | tail -1`
        NAME=`basename $F .nii.gz`
	
	echo ${NAME}
	
        # roi flag drops first 5 frames, the -s flag does the smoothing
        s=2.55
        fslmaths ${F} -roi 0 -1 0 -1 0 -1 5 -1 -s ${s} dropped_n_smoothed/${NAME}.nii.gz  
done


