# BRIDGES_analysis

All of the current scripts are contained in the "Scripts" folder and are labeled with a leading number specifiying the order in which they are to be executed. The analysis has not been fully developed into a pipeline and so you will have to change the file paths that I hardcoded specific to my computer to reflect your organization. Lastly, some of the scripts are a little outdated, as we haven't been using them in the most recent iterations of the analysis, if I have some extra time, i will try to run through them again to weed those out. 

- 01x files are the most important files for anyone using this moving forward
  - These files are where IC filtering, labeling, and visualization is being done and the part in which most time has been devoted
- The rest of the scripts are regression analyses and data cleaning
  - Feel free to use the rest, but it is very messy because it is overly complicated by the fact as i was trying to make a pipeline that included imputation which never quite cohered. 
- XX and YY are useful for formating and saving data to redcap and other sources
- smooth_drop* are files that drop the first few frames of the scan because this necessary to allow the magnetic field to stabilize. If you have further questions ask Dr. Mueller
- calling_* are a set of commands that automate calling GIG and GICA ICA commands without pulling up Matlab everytime, this allows for parallelization outside of the matlab environment. It would be more efficient to loop within matlab, however I wasn't able to figure out how to do this without hogging way too many resources. these commands also set correct priorities
- extracting_metastates is a file describing how to extract the metastates from the matlab file using the matlab interface to save out a text copy of the metastate trajectories. This is a little dated since this is now included automatically in the R scripts I wrote, however this could potentiallly be useful so i left it in.
- GIG/GICA.m - matlab scripts for running GIG and GICA respectively. that are called by calling_* 
- smoothness_check - a script to double check the resulting smoothing after the smooth and drop step

