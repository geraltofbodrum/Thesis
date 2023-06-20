library("readxl")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("DescTools")
library("tidyverse")
library("rstatix")
library("writexl")

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
first_condition <- filter(df_dummies, Condition == 1)
second_condition <- filter(df_dummies, Condition == 2)



#---------------------------------------------
#Response Time Boxplots 
#response time boxplot grouped by generated / real first condition
bxp_first_GvR <- ggboxplot(
  first_condition, x = "GeneratedReal", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Generated / Real")
bxp_first_GvR

#response time boxplot grouped by left / right all conditions 
bxp_first_LvR <- ggboxplot(
  first_condition, x = "LeftRight", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Left / Right")
bxp_first_LvR

#response time boxplot grouped by eyes / skin all conditions 
bxp_first_EvS <- ggboxplot(
  first_condition, x = "EyesSkin", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Eyes / Skin")
bxp_first_EvS
#---------------------------------------------

#----------------------------------------------------------------- 
#First Condition RT
#wilcox test response time grouped by generated versus Real 
test_GvR_RT_c1 <- first_condition %>% 
  wilcox_test(ResponseTime ~ GeneratedReal) %>%
  add_significance()
test_GvR_RT_c1

test_GvR_RT_c1 <- test_GvR_RT_c1 %>% add_xy_position(x = "GeneratedReal")
bxp_first_GvR + 
  stat_pvalue_manual(test_GvR_RT_c1, tip.length = 0) +
  labs(subtitle = get_test_label(test_GvR_RT_c1, detailed = TRUE))

#generated condition
mean(first_condition$`ResponseTime`[first_condition$GeneratedReal == 0])
#real condition
mean(first_condition$`ResponseTime`[first_condition$GeneratedReal == 1])


#wilcox test response time grouped by left versus right
test_LvR_RT_c1 <- first_condition %>% 
  wilcox_test(ResponseTime ~ LeftRight) %>%
  add_significance()
test_LvR_RT_c1

test_LvR_RT_c1 <- test_LvR_RT_c1 %>% add_xy_position(x = "LeftRight")
bxp_first_LvR + 
  stat_pvalue_manual(test_LvR_RT_c1, tip.length = 0) +
  labs(subtitle = get_test_label(test_LvR_RT_c1, detailed = TRUE))

#left condition
mean(first_condition$`ResponseTime`[first_condition$LeftRight == 0])
#right condition
mean(first_condition$`ResponseTime`[first_condition$LeftRight == 1])

#wilcox test response time grouped by eyes versus skin
test_EvS_RT_c1 <- first_condition %>% 
  wilcox_test(ResponseTime ~ EyesSkin) %>%
  add_significance()
test_EvS_RT_c1

first_condition %>% wilcox_effsize(ResponseTime ~ EyesSkin)

test_EvS_RT_c1 <- test_EvS_RT_c1 %>% add_xy_position(x = "EyesSkin")
bxp_first_EvS + 
  stat_pvalue_manual(test_EvS_RT_c1, tip.length = 0) +
  labs(subtitle = get_test_label(test_EvS_RT_c1, detailed = TRUE))

#eye condition
median(first_condition$`ResponseTime`[first_condition$EyesSkin == 0])
#skin condition
median(first_condition$`ResponseTime`[first_condition$EyesSkin == 1])

#eye condition
IQR(first_condition$`ResponseTime`[first_condition$EyesSkin == 0])
#skin condition
IQR(first_condition$`ResponseTime`[first_condition$EyesSkin == 1])
#----------------------------------------------------------------- 

#---------------------------------------------
#Response Time Boxplots second condition
#response time boxplot grouped by generated / real second condition
bxp_second_GvR <- ggboxplot(
  second_condition, x = "GeneratedReal", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Generated / Real")
bxp_second_GvR

#response time boxplot grouped by left / right all conditions 
bxp_second_LvR <- ggboxplot(
  second_condition, x = "LeftRight", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Left / Right")
bxp_second_LvR

#response time boxplot grouped by eyes / skin all conditions 
bxp_second_EvS <- ggboxplot(
  second_condition, x = "EyesSkin", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Eyes / Skin")
bxp_second_EvS
#---------------------------------------------

#----------------------------------------------------------------- 
#Second Condition RT
#wilcox test response time grouped by generated versus Real 
test_GvR_RT_c2 <- second_condition %>% 
  wilcox_test(ResponseTime ~ GeneratedReal) %>%
  add_significance()
test_GvR_RT_c2

test_GvR_RT_c2 <- test_GvR_RT_c2 %>% add_xy_position(x = "GeneratedReal")
bxp_second_GvR + 
  stat_pvalue_manual(test_GvR_RT_c2, tip.length = 0) +
  labs(subtitle = get_test_label(test_GvR_RT_c2, detailed = TRUE))

#generated condition
mean(second_condition$`ResponseTime`[second_condition$GeneratedReal == 0])
#real condition
mean(second_condition$`ResponseTime`[second_condition$GeneratedReal == 1])


#Wilcox test response time grouped by left versus right
test_LvR_RT_c2 <- second_condition %>% 
  wilcox_test(ResponseTime ~ LeftRight) %>%
  add_significance()
test_LvR_RT_c2

test_LvR_RT_c2 <- test_LvR_RT_c2 %>% add_xy_position(x = "LeftRight")
bxp_second_LvR + 
  stat_pvalue_manual(test_LvR_RT_c2, tip.length = 0) +
  labs(subtitle = get_test_label(test_LvR_RT_c2, detailed = TRUE))

#left condition
mean(second_condition$`ResponseTime`[second_condition$LeftRight == 0])
#right condition
mean(second_condition$`ResponseTime`[second_condition$LeftRight == 1])

#Wilcox test response time grouped by eyes versus skin
test_EvS_RT_c2 <- second_condition %>% 
  wilcox_test(ResponseTime ~ EyesSkin) %>%
  add_significance()
test_EvS_RT_c2

test_EvS_RT_c2 <- test_EvS_RT_c2 %>% add_xy_position(x = "EyesSkin")
bxp_second_EvS + 
  stat_pvalue_manual(test_EvS_RT_c2, tip.length = 0) +
  labs(subtitle = get_test_label(test_EvS_RT_c2, detailed = TRUE))

second_condition %>% wilcox_effsize(ResponseTime ~ EyesSkin)

#eye condition
median(second_condition$`ResponseTime`[second_condition$EyesSkin == 0])
#skin condition
median(second_condition$`ResponseTime`[second_condition$EyesSkin == 1])

#eye condition
IQR(second_condition$`ResponseTime`[second_condition$EyesSkin == 0])
#skin condition
IQR(second_condition$`ResponseTime`[second_condition$EyesSkin == 1])
#----------------------------------------------------------------- 

#-----------------------------------------------------------------
#First Condition Accuracy 
cont_table_1 <- table(first_condition$Accuracy)
cont_table_1
chisq.test(cont_table_1)

#accuracy grouped by eyes and skin 
Accuracy_by_EvS_1 <- first_condition %>% 
  wilcox_test(Accuracy ~ EyesSkin) %>%
  add_significance()
Accuracy_by_EvS_1

#eye 
mean(first_condition$Accuracy[first_condition$EyesSkin == 0])
#skin
mean(first_condition$Accuracy[first_condition$EyesSkin == 1])

first_condition %>% wilcox_effsize(Accuracy ~ EyesSkin)

#accuracy grouped by left and right
Accuracy_by_LvR_1 <- first_condition %>% 
  wilcox_test(Accuracy ~ LeftRight) %>%
  add_significance()
Accuracy_by_LvR_1

#left 
mean(first_condition$Accuracy[first_condition$LeftRight == 0])
#right
mean(first_condition$Accuracy[first_condition$LeftRight == 1])

#accuracy grouped by generated and real
Accuracy_by_GvR_1 <- first_condition %>% 
  wilcox_test(Accuracy ~ GeneratedReal) %>%
  add_significance()
Accuracy_by_GvR_1

#generated 
mean(first_condition$Accuracy[first_condition$GeneratedReal == 0])
#real
mean(first_condition$Accuracy[first_condition$GeneratedReal == 1])
#-----------------------------------------------------------------

#-----------------------------------------------------------------
#Second Condition Accuracy 
cont_table_2 <- table(second_condition$Accuracy)
cont_table_2
chisq.test(cont_table_2)

#accuracy grouped by eyes and skin 
Accuracy_by_EvS_2 <- second_condition %>% 
  wilcox_test(Accuracy ~ EyesSkin) %>%
  add_significance()
Accuracy_by_EvS_2

#eye 
mean(second_condition$Accuracy[second_condition$EyesSkin == 0])
#skin
mean(second_condition$Accuracy[second_condition$EyesSkin == 1])

second_condition %>% wilcox_effsize(Accuracy ~ EyesSkin)

#accuracy grouped by left and right
Accuracy_by_LvR_2 <- second_condition %>% 
  wilcox_test(Accuracy ~ LeftRight) %>%
  add_significance()
Accuracy_by_LvR_2

#left 
mean(second_condition$Accuracy[second_condition$LeftRight == 0])
#right
mean(second_condition$Accuracy[second_condition$LeftRight == 1])

#accuracy grouped by generated and real
Accuracy_by_GvR_2 <- second_condition %>% 
  wilcox_test(Accuracy ~ GeneratedReal) %>%
  add_significance()
Accuracy_by_GvR_2

#generated 
mean(second_condition$Accuracy[second_condition$GeneratedReal == 0])
#real
mean(second_condition$Accuracy[second_condition$GeneratedReal == 1])

#-----------------------------------------------------------------

#--------------------------------------------------------
#Logistic Regression for Accuracy First Condition
model_1 <- glm(Accuracy ~ GeneratedReal + LeftRight + EyesSkin, data = first_condition, family = binomial)
model_1_v2 <- glm(Accuracy ~ GeneratedReal + LeftRight + EyesSkin + Participant, data = first_condition, family = binomial)

summary(model_1)
summary(model_1_v2)
#--------------------------------------------------------

#--------------------------------------------------------
#Logistic Regression for Accuracy Second Condition
model_2 <- glm(Accuracy ~ GeneratedReal + LeftRight + EyesSkin, data = second_condition, family = binomial)
model_2_v2 <- glm(Accuracy ~ GeneratedReal+ LeftRight + EyesSkin + Participant, data = second_condition, family = binomial)

summary(model_2)
summary(model_2_v2)
#--------------------------------------------------------