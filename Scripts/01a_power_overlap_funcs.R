library(tidyverse)
library(fslr)
library(broom)
library(ramify)
library(MASS)
library(kableExtra)
library(naniar)
# library(nlme)
library(plyr)


#################################################
# Power spectra##################################
#################################################
calc_powerspec <- function(file) {
  nii.file <- readNIfTI(file)
  tps <- dim(nii.file)[1]
  IC_times <- as.data.frame(nii.file)
  IC_powerspec <- IC_times %>%
    mutate(time = 1:tps) %>%
    pivot_longer(colnames(IC_times) , names_to = "IC", values_to= "Signal") %>%
    nest(data=-IC) %>%
    mutate(powerspec = map(data, ~ fft(.$Signal, inverse = FALSE))) %>%
    unnest(powerspec) %>%
    mutate(fre = powerspec - 1/(tps*(1/tr)),
           sqr_mod = abs(fre) **2,
           power = sqr_mod /(tps * (1/tr)),
           Hz = rep(linspace(0, 1/ (2 * tr), tps), 100)) %>%
    filter(Hz < max(Hz)/2)
  # retrun power spec
  IC_powerspec
}

# gather all powerspectra and power ratios
power_ratio_looper <- function(files) {
  # initialize empty dataframes
  all_powerspec <- data.frame(matrix(nrow = 0, ncol = 4))
  all_highlow <- data.frame(matrix(nrow = 100, ncol = 1))
  # define time resoltuoin and number of time points
  tr <-0.8
  
  colnames(all_powerspec) <- c("subject", "IC", "Hz", "power")
  
  for (i in 1:length(files)) {
    # add poewr spectrum to list of powerspectra
    all_powerspec <- calc_powerspec(timecourse.files[[i]]) %>% 
      mutate(subject = rep(i, each = 100 * 453)) %>%
      dplyr::select(subject, IC, Hz, power) %>%
      rbind(all_powerspec)
    
    # get the low/high power ratio
    high_low <- IC_powerspec %>%
      filter(Hz > 0.01 & Hz < 0.25) %>%
      mutate(low = Hz < 0.1) %>%
      dplyr::group_by(IC, low) %>%
      dplyr::summarize(cumul_power = sum(power)) %>%
      pivot_wider(names_from = low, values_from = cumul_power) %>%
      mutate(ratio = `TRUE`/`FALSE`)
    
    # add ratio to list of ratios
    all_highlow[[paste0("subj", i)]] <- high_low$ratio
    
  }
  
  ICnumbs <- c("1", "10", "100","11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99")
  ICnumbs <- as.numeric(ICnumbs)
  all_highlow["IC"] <- ICnumbs
  
  
  
  
  highlow_long <- all_highlow %>%
    pivot_longer(cols = -IC, names_to= "Subject", values_to = "ratio")
  # Save the output for later use if needed
  write.csv(highlow_long, paste0(ics_dir, "timecourses/freq_analysis.csv"),  row.names = FALSE)
  write.csv(all_powerspec, paste0(ics_dir, "timecourses/powersepctra.csv"), row.names=FALSE)
}


####################################################################
# Spatial ##########################################################
####################################################################
# Establish Yeo dictionary index
Yeo_dict <- data.frame(
  ROI = c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 24, 30, 33, 36, 39, 48,
          51, 54, 78,  84,  141,  147,  150,  153,  156,  159,  162,  174,  180),
  ROI_name = c("Noise", "Visual A", "Visual B", "SomMotor A", "SomMotor B", "Temp Par", "Dor Attn A", "Dor Attn B",        
               "Sal/VenAttn A", "Sal/VenAttn B", "Control A", "Control B", "Control C", "Default A", "Default B", "Default C",           
               "Limbic A", "Limbic B", "Left Cerebellum", "Left Thalamus", "Left Caudate", "Left Putamen", "Left Pallidum", "Brainstem",        
               "Left Hippocampus", "Left Amygdala", "Left Accumbens", "Left Ventral Diencephalon", "Right Cerebellum", "Right Thalamus",     
               "Right Caudate", "Right Putamen", "Right Pallidum", "Right Hippocampus", "Right Amygdala", "Right Accumbens", "Right Ventral Diencephalon")
) %>%
  mutate(ROI = factor(ROI))

yeotable <- as.data.frame(t(matrix(Yeo_dict,2)))
colnames(yeotable)  <- c("ROI", "Name")
reftable <- tibble(ROI = factor(0:184))
ICsizes <- c()
threshold <- 50








# threshold and voxel count ROI
thresh_n_count <- function(file) {
  atlas_file = '/home/christian/Research/Brain_stuff/Atlases/Yeo_17_2mm_mni152.nii.gz'
  # thresholded at 50 percent signal (This was chosen rather hastily, so might be worth investigating an optimum)
  binary <- fslmaths(file, opts ='-abs -thrP 50 -bin')
  # multiply it by the Yeo atlas.
  ICsize <- sum(binary)
  ICsizes <- append(ICsizes, ICsize)
  
  yeo <- fslmaths(binary, opts =paste0('-mul ', atlas_file))
  in_Yeo_df <- as.data.frame(table(yeo)) %>%
    dplyr::rename(ROI = yeo, Size= Freq)
  reftable <- reftable %>%
    mutate(ROI = ROI) %>%
    left_join(in_Yeo_df, by = "ROI")
}

# Specify number of ICs
loop_thresh_n_count <- function(ICs){
  for (i in 1:ICs) {
    print(i)
    # thresholded at 50 percent signal (This was chosen rather hastily, so might be worth investigating an optimum)
    binary <- fslmaths(paste0('../SMs/IC_', i, '.nii.gz'), opts ='-abs -thrP 50 -bin')
    # multiply it by the Yeo atlas.
    ICsize <- sum(binary)
    ICsizes <- append(ICsizes, ICsize)
    
    yeo <- fslmaths(binary, opts =paste0('-mul ', atlas_file))
    in_Yeo_df <- as.data.frame(table(yeo)) %>%
      dplyr::rename(ROI = yeo, Size= Freq)
    reftable <- reftable %>%
      mutate(ROI = ROI) %>%
      left_join(in_Yeo_df, by = "ROI")
    colnames(reftable)[i+1] <- paste0("ICpixels_", i)
  }
  IC_sizes <- data.frame(IC=  factor(1:100), IC_size = ICsizes)
  # save the results
  write.csv(IC_sizes, "../thresh50/IC_sizes.csv",  row.names = FALSE)
  write.csv(reftable, "../thresh50/overlapped_ICs.csv",  row.names = FALSE)
  
}



# Max voel namer
max_namer <- function(ICfile, atlas = atlas, names = Yeo_dict) {
    # select max voxel. Map it to the atlas
  atval <- atlas[which(ICfile == max(ICfile), arr.ind=T)]
  # Map atlas value to name
  name <- Yeo_dict$ROI_name[Yeo_dict$ROI == atval]
  name <- ifelse(identical(name, character(0)), "Noise", name)
}

# Max looper namer
max_name_looper <- function(ICfiles) {
  # load atlas
  atlas <- readNIfTI(atlas_file)
  names <- c()
  is <- c()
  for (i in 1:length(ICfiles)){
    #print(i)
    # grab name
    n <- max_namer(ICfiles[[i]], atlas = atlas, names = Yeo_dict)
    names <- c(names, n)
    # print(paste(i, ": ", n))
  }
  # return names 
  names
}




