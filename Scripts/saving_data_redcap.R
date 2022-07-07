library(stringr)
library(tidyverse)
df <- read.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/files.txt", header = FALSE)

dfnew <- str_split(df$V1, "-", simplify = TRUE)[,10]

df <- str_split(dfnew , "_", simplify = TRUE)[,1]

write.csv(df, "Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/procs.txt", row.names = FALSE, col.names = FALSE)


z<- read.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/complete bridges y1.csv", header = TRUE)

znew <- z %>% 
  # filter(is.na(No_Scan), Rest_motion_exclusion !=1) %>%
  dplyr::select(grid, No_Scan, Rest_motion_exclusion, RS_fieldmap_exclusion, dropped.quit) %>%
  filter(is.na(No_Scan)) %>%
  mutate(grid = factor(grid))

znew %>% filter(Rest_motion_exclusion ==1, RS_fieldmap_exclusion == 1)


d1 <- readRDS("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Book_regressions/imputed_switching_data.rds")
d2 <- readRDS("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Book_regressions/imputed_dwell_data.rds")

d1 <- d1$data
d2 <- d2$data


d1 <- d1 %>% 
  inner_join(znew, by = "grid") %>%
  filter(is.na(No_Scan), (Rest_motion_exclusion == 0 | is.na(Rest_motion_exclusion)), RS_fieldmap_exclusion == 0 ) %>% 
  select(grid, age, BDI, BSSI, Lifetime.NSSI.Group, lifetime_nssie, Freq) 
  write.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Book_regressions/switching_data.csv", col.names = c("grid", "age", "BDI", "BSSI", "Lifetime.NSSI.Group", "lifetime_nssie", "Freq"), row.names = FALSE)

d2 %>% 
  inner_join(znew, by = "grid") %>%
  filter(is.na(No_Scan), (Rest_motion_exclusion == 0 | is.na(Rest_motion_exclusion)), RS_fieldmap_exclusion == 0 )%>% 
  select(grid, age, BDI, BSSI, Lifetime.NSSI.Group, lifetime_nssie, State, Dwell.time) %>%
  write.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Book_regressions/dwell_data.csv", 
            col.names = c("grid", "age", "BDI", "BSSI", "Lifetime.NSSI.Group", "lifetime_nssie", "State", "Dwell.time"), row.names = FALSE)


d3 <- read.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/states.csv")

colnames(d3) <- c("Time", df)

d3 %>% 
  pivot_longer(-Time, names_to = "grid", values_to="State") %>%
  arrange(grid) %>% 
  mutate(grid = factor(grid)) %>%
  filter(grid %in% factor(d1$grid)) %>%
  pivot_wider(names_from = grid, values_from = State) %>%
  write.csv("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/state_sequences.csv")
