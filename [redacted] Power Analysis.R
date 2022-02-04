# SET LIBRARY PATH -------------------------------------------------------------
# set librarby to avoid Onedrive:
.libPaths("C:/R library")
.libPaths()


# INSTALL PACKAGES AND ORGANISE LIBRARY ----------------------------------------
if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
if(!require("pwr")) {install.packages("pwr")}
library(pwr)

# POWER ANALYSIS FOR OP [classified] ----------------------------------------------
# Need three of the four criteria in order to calculate the fourth. 
?pwr::pwr.t.test
pwr::pwr.t.test(d = 0.8,      # Using Cohen's estimate for this test of difference
                power = 0.80,  # The probability of accepting the alternative hypothesis if it is true (common is .80)
                sig.level = 0.05,
                type = "paired",
                alternative = "two.sided")
# If weak difference between pre- & post- measures then n = 199 needed
# If medium difference n = 34 needed. 

# Cohen's effect sizes are seen as rough guidelines:
pwr::cohen.ES(test = "t", size = "medium")
# Reference for the effect sizes can be found at:
# Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.


# PLOT POWER ANALYSIS ----------------------------------------------------------

effect_size_list <- seq(0.1, 1.0, 0.1) # Cohen's D vector
samp.out <- NULL

# function to calculate n for each effect size:
for(i in 1:length(effect_size_list)) {
  n.xxx <- pwr::pwr.t.test(power = .80, sig.level = 0.05, type = "paired", alternative = "two.sided", d = effect_size_list[i])$n
  n.xxx <- data.frame(d = effect_size_list[i], sample.size = n.xxx)
  samp.out <- rbind(samp.out, n.xxx)
}

# plot results of function
ggplot(samp.out, aes(x = d, y = sample.size)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme_minimal() +
  geom_hline(yintercept = 89.15, lty = 2, colour = "blue") +
  ylab("Sample Size") +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  xlab("Cohen's D (effect size)") +
  scale_x_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  ggtitle("Op [classified]: Power curve to determine sample size")
  
  




