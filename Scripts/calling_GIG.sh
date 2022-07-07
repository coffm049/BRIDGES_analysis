#!/bin/bash
# Made sure to change priority before running so many threads
renice -n 17 $$

# nohup means we can leave and keep script running
# nice changes priority
# Run matlab without annoying pop-ups
nohup nice -n 17 matlab2017b -nodesktop -nosplash -nodisplay -r 'try; GIG_ICA ; catch; save code_err; end; quit' > output.log &  




