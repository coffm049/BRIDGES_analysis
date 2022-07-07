#!/bin/bash


renice -n 17 $$

# Make a new file containing the list of files already run or running storing the process ID's
# Added something that will unlikely show up so won't confuse our counting 
echo "deletej;lkjad" > files_run

# Initialize current number of jobs runnign to 0
CUR_NUM=0

for i in {1..124}
do

	# Check if any of the PIDs from the fslmaths calls are currently being processed
	# Only add the next file in if there are fewer than 12 fslmaths commands running
	
        # Check to see if 12 or more current processes are matched by that string
	while [ $((CUR_NUM < 12)) ]
	do	
        	echo "Inside while loop"
		# Concatenate all files that have been run into one string and save that string to variable STRING s.t. grep looks for all of the strings
	        STRING=`cat files_run | tr -s "\n" "\|"`
        	# It throws on an empty character which will map to everything, so just remove the last character
	        STRING=${STRING::-1}
        	# Store the number of current processes that are running matching fslmaths PIDs
	        CUR_NUM=`ps -u ccoffma | awk '{print $1}' | grep ${STRING} | wc -l`
		
		echo ${CUR_NUM}
		echo $((CUR_NUM < 12))	

		# If there are too many scripts running wait 3 seconds and check again
		sleep 3
	done 

        echo "Outside while loop \n"

        # Grab file from list of files for analysis
        F=`head -$i files.txt | tail -1`
        NAME=`basename $F .nii.gz`
	
	echo ${NAME}
	
        # roi flag drops first 5 frames, the -s flag does the smoothing
        s=2.55
        fslmaths ${F} -roi 0 -1 0 -1 0 -1 5 -1 -s ${s} dropped_n_smoothed/${NAME}.nii.gz & 

	# store the process ID from this 
	echo $! >> files_run
done


