run("~/Analysis_tools/load_GIFT.m")

% Set maximum number of computational threads 
maxNumCompThreads(4) 

% GIG-ICA call
gica_cmd --d files.txt --o GIG/GIG --templates GICA/gica_cmd_agg__component_ica_.nii --n 100 --a gig-ica --pca MPOWIT --performance 2

 % Grabbed files for transfer to my computer to label and filter in R.
% Grabbed files for transfer to my computer to label and filter in R.
% Group SMs: gica_cmd_mean_component_ica_s_all_.nii
% Subject timecourses: gica_cmd_sub030_timecourses_ica_s1_.nii 


