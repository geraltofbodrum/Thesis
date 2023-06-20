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

#---------------------------------------------
#Response Time Boxplots 
#response time boxplot grouped by generated / real all conditions 
bxp_all_GvR <- ggboxplot(
  df_dummies, x = "GeneratedReal", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Generated / Real")
bxp_all_GvR

#response time boxplot grouped by left / right all conditions 
bxp_all_LvR <- ggboxplot(
  df_dummies, x = "LeftRight", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Left / Right")
bxp_all_LvR

#response time boxplot grouped by eyes / skin all conditions 
bxp_all_EvS <- ggboxplot(
  df_dummies, x = "EyesSkin", y = "ResponseTime", 
  ylab = "Response Times", xlab = "Eyes / Skin")
bxp_all_EvS

#---------------------------------------------


#Response Time Statistics 
#--------------------------------------------------------
#median response time all



#Wilcox test response time grouped by generated versus Real 
test_GvR_RT <- df_dummies %>% 
  wilcox_test(ResponseTime ~ GeneratedReal) %>%
  add_significance()
test_GvR_RT

#boxplot grouped by generated and real all conditions
test_GvR_RT <- test_GvR_RT %>% add_xy_position(x = "GeneratedReal")
bxp_all_GvR + 
  stat_pvalue_manual(test_GvR_RT, tip.length = 0) +
  labs(subtitle = get_test_label(test_GvR_RT, detailed = TRUE))


#generated condition
mean(df_dummies$`ResponseTime`[df_dummies$GeneratedReal == 0])
#real condition
mean(df_dummies$`ResponseTime`[df_dummies$GeneratedReal == 1])


#Wilcox test response time grouped by left versus right
test_LvR_RT <- df_dummies %>% 
  wilcox_test(ResponseTime ~ LeftRight) %>%
  add_significance()
test_LvR_RT

#boxplot grouped by left and right all conditions 
test_LvR_RT <- test_LvR_RT %>% add_xy_position(x = "LeftRight")
bxp_all_LvR + 
  stat_pvalue_manual(test_LvR_RT, tip.length = 0) +
  labs(subtitle = get_test_label(test_LvR_RT, detailed = TRUE))

#left condition
mean(df_dummies$`ResponseTime`[df_dummies$LeftRight == 0])
#right condition
mean(df_dummies$`ResponseTime`[df_dummies$LeftRight == 1])

#wilcox test response time grouped by eyes versus skin
test_EvS_RT <- df_dummies %>% 
  wilcox_test(ResponseTime ~ EyesSkin) %>%
  add_significance()
test_EvS_RT

#boxplot grouped by eyes and skin in all conditions
test_EvS_RT <- test_EvS_RT %>% add_xy_position(x = "EyesSkin")
bxp_all_EvS + 
  stat_pvalue_manual(test_EvS_RT, tip.length = 0) +
  labs(subtitle = get_test_label(test_EvS_RT, detailed = TRUE))

#eye condition
mean(df_dummies$`ResponseTime`[df_dummies$EyesSkin == 0])
#skin condition
mean(df_dummies$`ResponseTime`[df_dummies$EyesSkin == 1])
#--------------------------------------------------------

#Accuracy Statistics 
#--------------------------------------------------------

#chi square for all conditions accuracy 
cont_table_all <- table(df_dummies$Accuracy)
cont_table_all
chisq.test(cont_table_all)

mean(df_dummies$Accuracy)

#accuracy grouped by eyes and skin 
Accuracy_by_EvS <- df_dummies %>% 
  wilcox_test(Accuracy ~ EyesSkin) %>%
  add_significance()
Accuracy_by_EvS

#eye 
mean(df_dummies$Accuracy[df_dummies$EyesSkin == 0])
#skin
mean(df_dummies$Accuracy[df_dummies$EyesSkin == 1])

df_dummies %>% wilcox_effsize(Accuracy ~ EyesSkin)

#accuracy grouped by left and right
Accuracy_by_LvR <- df_dummies %>% 
  wilcox_test(Accuracy ~ LeftRight) %>%
  add_significance()
Accuracy_by_LvR

#left 
mean(df_dummies$Accuracy[df_dummies$LeftRight == 0])
#right
mean(df_dummies$Accuracy[df_dummies$LeftRight == 1])

#accuracy grouped by generated and real
Accuracy_by_GvR <- df_dummies %>% 
  wilcox_test(Accuracy ~ GeneratedReal) %>%
  add_significance()
Accuracy_by_GvR

#generated 
mean(df_dummies$Accuracy[df_dummies$GeneratedReal == 0])
#real
mean(df_dummies$Accuracy[df_dummies$GeneratedReal == 1])
#--------------------------------------------------------

#--------------------------------------------------------
#Logistic Regression for Accuracy 
model <- glm(Accuracy ~ GeneratedReal + LeftRight + EyesSkin, data = df_dummies, family = binomial)
model2 <- glm(Accuracy ~ GeneratedReal + LeftRight + EyesSkin + Participant, data = df_dummies, family = binomial)

summary(model)
summary(model2)
#--------------------------------------------------------
