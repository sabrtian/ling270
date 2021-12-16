library(tidyverse)
library(lsr)
library(ggpubr)
library(ez)
library(pwr)
library(psych)
library(gridExtra)
library(gtools)
library(ggplot2)
library(ggbeeswarm)
library(BayesFactor)
rm(list = ls())
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Joan Danielle K. Ongchoco
# This code analyzes data for JULIA DEMO.
# CleanData, Exclusions, GetKeyDiff, PlotData, Statistics, Save

######################################################################
# CLEAN DATA
# Load Files
filenames = list.files(pattern=".csv")
ad = do.call("smartbind",lapply(filenames,read.csv,header=TRUE))

active_trial_data = ad %>% filter(test_part == "active_trial") %>% 
  select(subj_id, trial_num, image_1, image_2, keyword, responses)
end_trial_data = ad %>% filter(trial_type == 'jo-survey-multi-choice') %>%
  select(subj_id, responses)

end_trial_data$responses = as.character(end_trial_data$responses)
end_trial_data$correct = substr(end_trial_data$responses, 2, 8)
end_trial_data$correct = str_replace_all(end_trial_data$correct, "[[:punct:]]", "")
end_trial_data$final_guess = substrRight(end_trial_data$responses, 15)

for (i in 1:nrow(end_trial_data)){
  end_trial_data$performance[i] = grepl(end_trial_data$correct[i], end_trial_data$final_guess[i], fixed=TRUE)
}

end_trial_data$condition = rep(c("recall", "cross", "cross", "cross", "cross", "recall"), length(filenames))

end_trial_data %>% group_by(subj_id, condition) %>% summarize(mean=mean(performance)) %>%
  group_by(condition) %>% summarize(mean=mean(mean)) %>% 
  ggplot(aes(condition, mean)) + geom_bar(stat="identity") + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
  geom_hline(yintercept=0.125) + 
  theme_test() + 
  scale_color_brewer(palette="Paired") +
  scale_fill_brewer(palette="Paired") +
  theme(aspect.ratio=1, 
        legend.title = element_blank(), 
        legend.position="none", 
        axis.ticks = element_blank(),
        axis.title.x=element_text(face="bold"), 
        axis.title.y=element_text(face="bold"), 
        text=element_text(size=16, family="Helvetica")) +
  labs(x="Condition", y="Proportion Correct")
                               