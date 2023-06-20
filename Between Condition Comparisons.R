library("readxl")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("DescTools")
library("tidyverse")
library("rstatix")

#load the first condition into a table
all_conditions <- read_excel("C:/Users/dogaa/Desktop/Thesis/all_data.xlsx")

# create dummy variables using model.matrix()
dummies <- model.matrix(~ ., data = all_conditions)

# remove the intercept column from the dummy variable matrix
dummies <- dummies[, -1]

# combine the dummy variable matrix with the original dataframe
df_dummies <- data.frame(dummies)

colnames(df_dummies)[3] <- "GeneratedReal"
colnames(df_dummies)[4] <- "LeftRight"
colnames(df_dummies)[5] <- "EyesSkin"
colnames(df_dummies)[7] <- "ResponseTime"

#-------------------------------------------------------------------------
#Response Time Statistics 
df_dummies %>%
  group_by(Condition) %>%
  get_summary_stats('ResponseTime', type = "median_iqr")

bxp_between_conditions_RT <- ggboxplot(
  df_dummies, x = "Condition", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Condition")
bxp_between_conditions_RT

#Wilcox test for response time grouped by condition 
RT_by_cond <- df_dummies %>% 
  wilcox_test(`ResponseTime` ~ Condition) %>%
  add_significance()
RT_by_cond

RT_by_cond <- RT_by_cond %>% add_xy_position(x = "Condition")
bxp_between_conditions_RT + 
  stat_pvalue_manual(RT_by_cond, tip.length = 0) +
  labs(subtitle = get_test_label(RT_by_cond, detailed = TRUE))

median(df_dummies$`ResponseTime`[df_dummies$Condition == 1])
median(df_dummies$`ResponseTime`[df_dummies$Condition == 2])
IQR(df_dummies$`ResponseTime`[df_dummies$Condition == 1])
IQR(df_dummies$`ResponseTime`[df_dummies$Condition == 2])

df_dummies %>% wilcox_effsize(ResponseTime ~ Condition)

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
#Accuracy Statistic 

#accuracy grouped by condition
Accuracy_by_cond <- df_dummies %>% 
  wilcox_test(Accuracy ~ Condition) %>%
  add_significance()

Accuracy_by_cond

mean(df_dummies$Accuracy[df_dummies$Condition == 1])
mean(df_dummies$Accuracy[df_dummies$Condition == 2])

df_dummies %>% wilcox_effsize(Accuracy ~ Condition)

#-------------------------------------------------------------------------
