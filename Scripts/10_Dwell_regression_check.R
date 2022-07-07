library(mice)
library(tidyverse)
library(micemd)
library(miceadds)
library(lme4)
library(broom.mixed)
library(mitml)
library(patchwork)



dwell_data <- readRDS("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/imputed_dwell_data.rds")$data
grid_data <- read.csv("/home/christian/Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/switching_data.csv") 


#########################
# Residual plots for dwell model

mod <- glmer(Dwell.time ~ ((scale(age) + scale(BSSI))|State) + (1|grid), family = Gamma(link = "log"), data = dwell_data)

# mod <- lmer(log(Dwell.time+ 1) ~ (scale(age) + scale(BSSI)) + (1|State) + (1|grid), data = dwell_data)



# # Residuals for AGE
# age_res <- dwell_data %>% 
#   dplyr::select(Dwell.time, State, age, BSSI) %>%
#   rename(Age = age) %>%
#   drop_na() %>%
#   mutate(Res = resid(mod),Age = as.numeric(Age)) %>%
#   ggplot(aes(x= Age, y = Res)) +
#   geom_point() +
#   facet_wrap(~State, nrow = 2)+ 
#   geom_smooth(method = "loess") + 
#   ylab("Residual") +
#   xlab("Age")
# # Residuals for BSSI
# BSSI_res <- dwell_data %>% 
#   dplyr::select(Dwell.time, State, age, BSSI) %>%
#   drop_na() %>%
#   mutate(Res = resid(mod)) %>%
#   ggplot(aes(x= BSSI, y = Res)) +
#   geom_point() +
#   facet_wrap(~State, nrow = 2)+ 
#   geom_smooth(method = "loess") + 
#   ylab("Residual") +
#   xlab("BSSI")
# 
# age_res / BSSI_res

# Age and BSSI trends
Dwell_age_trend <- dwell_data %>% 
  dplyr::select(Dwell.time, State, age, BSSI) %>%
  rename(Age = age) %>%
  drop_na() %>%
  mutate(Age = as.numeric(Age)) %>%
  ggplot(aes(x= Age, y = Dwell.time)) +
  geom_point(alpha = 0.33) + 
  scale_y_log10() + 
  facet_wrap(~State, nrow = 2)+ 
  geom_smooth(method = "loess") + 
  ylab("Dwell time (s)") +
  xlab("Age")
Dwell_bssi_trend <- dwell_data %>% 
  dplyr::select(Dwell.time, State, age, BSSI) %>%
  rename(Age = age) %>%
  drop_na() %>%
  mutate(Age = as.numeric(Age)) %>%
  ggplot(aes(x= BSSI, y = Dwell.time)) +
  geom_point(alpha = 0.33) +
  facet_wrap(~State, nrow = 2)+ 
  scale_y_log10() +
  geom_smooth(method = "loess") + 
  ylab("Dwell time (s)") +
  xlab("BSSI")

Dwell_age_trend / Dwell_bssi_trend



  

dwell_data %>% 
  dplyr::select(Dwell.time, State, age, BSSI) %>%
  drop_na() %>%
  mutate(Res = abs(resid(mod)), age = as.numeric(age)) %>%
  pivot_longer(c(BSSI, age), names_to = "Variable", values_to = "Measure") %>%
  ggplot(aes(x= Res)) +
  geom_histogram(bins = 40) +
#  facet_wrap(Variable~State, scales = "free_x") +
  xlab("Residual") +
  ylab("Density")
  
  



##### TEST out looking at specific effects on each state 
# average over each state and subject
dwells <- dwell_data %>% 
  group_by(grid, State) %>% 
  summarize(Dwell = mean(Dwell.time))
  
imps <- grid_data %>%
  mutate(grid = factor(grid)) %>%
  left_join(dwells, by = "grid") %>%
  # Checked for correct subjects
  # create imputations
  mice(seed = 123, m = 3, print = FALSE)
  


results <- imps %>%
  # Fit dwell time with log exponential link because it is an interarrival time
  with(glmer(Dwell ~ (scale(age) + scale(BDI))*State + (1|grid),  family = Gamma(link="log")))

# Estimate fixed effects
results %>% 
  pool() %>%
  summary()

# Estimate random effects

# Specific effects on each state were negligible, so reverting back to modeling them as random effects 
################################################



# original simpler model
dwell_data <- readRDS("Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/imputed_dwell_data.rds")


dwell_data %>%
  # Fit dwell time with log exponential link because it is an interarrival time
  with(glmer(Dwell.time ~ scale(age) + scale(BSSI)  + (1|grid) + (1|State),  family = Gamma(link="log"))) %>%
  saveRDS("/home/christian/Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/Dwell_reg_rand_state.rds")

results <- readRDS("/home/christian/Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/Dwell_reg_rand_state.rds")

pooled <- pool(results)


fit <- testEstimates(as.mitml.result(results), extra.pars = T)


fit1 <- results$analyses[[1]]
rs <- ranef(fit1)

tibble::rownames_to_column(rs$State, "State") %>%
  rename(Estimate = `(Intercept)`) %>%
  ggplot(aes(y= Estimate, x = State)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab(expression(paste(delta, ' sec x', 10^-2)))










## Plot for BSSI vs switching frequency
df <-readRDS("/home/christian/Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/Regressions/imputed_switching_data.rds")$data %>%
  dplyr::select(BSSI, age, Freq)

df %>%
  mutate(Freq = Freq * 60) %>%
  mutate(Missing= ifelse(is.na(BSSI), Freq, NA)) %>%
  ggplot(aes(x= BSSI, y = Freq)) +
  geom_point() +
  geom_rug(aes(y = Missing), inherit.aes = F, col = "red") +
  geom_smooth(method = "lm") + 
  ylab("Frequency (Switches / min)")



df %>%
  mutate(Freq = Freq * 60) %>%
  ggplot(aes(x= age, y = Freq)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  ylab("Frequency (Switches / min)") +
  xlab("Age (years)")

