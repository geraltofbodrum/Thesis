#Make data into table

library("readxl")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("lme4")
library("lmerTest")
library("car")
library("DescTools")
library("multcomp")

#load the first condition into a table
first_condition <- read_excel("C:/Users/dogaa/Desktop/Thesis/First Condition (Fake-Real) Ready.xlsx")


#load the second condition into a table
second_condition <- read_excel("C:/Users/dogaa/Desktop/Thesis/Second Condition (Real-Fake) Ready.xlsx")

#check the mean of response time and remove any outliers according to 2 above or below 2 standard deviations. 

#first condition 
response_times_first <- first_condition$`Response Time`

#mean RT first condition
mean_first <- mean(response_times_first)

#sd RT first condition
sd_first <- sd(response_times_first)

#first condition RT outlier above limit 
above_first <- mean_first + (2*sd_first)

#second condition
response_times_second <- second_condition$`Response Time`

#mean RT second condition
mean_second <- mean(response_times_second)

#sd RT second condition 
sd_second <- sd(response_times_second)

#second condition RT outlier above limit
above_second <- mean_second + (2*sd_second)

#outliers removed first and second condition
first_condition_v2 <- filter(first_condition, first_condition$`Response Time` < above_first)
second_condition_v2 <- filter(second_condition, second_condition$`Response Time` < above_second)


#remove if response time is equal to zero, meaning the participant didnt answer the question 
first_condition_v3 <- filter(first_condition_v2, first_condition_v2$`Response Time` > 0)
second_condition_v3 <- filter(second_condition_v2, second_condition_v2$`Response Time` > 0)
first_condition_v3 <- filter(first_condition_v3, first_condition_v3$`Response Time` < 3)
second_condition_v3 <- filter(second_condition_v3, second_condition_v3$`Response Time` < 3)
first_condition_v3$Answer <- as.numeric(first_condition_v3$Answer)
second_condition_v3$Answer <- as.numeric(second_condition_v3$Answer)
first_condition_v3$RealValue <- as.numeric(first_condition_v3$RealValue)
second_condition_v3$RealValue <- as.numeric(second_condition_v3$RealValue)

#add an accuracy column 
first_condition_v3$Accuracy <- ifelse(first_condition_v3$Answer == first_condition_v3$RealValue, 1, 0)

second_condition_v3$Accuracy <- ifelse(second_condition_v3$Answer == second_condition_v3$RealValue, 1, 0)

#first and second condition combined for purposes of overall results 
all_conditions <- rbind(first_condition_v3, second_condition_v3)

#first condition eyes
first_condition_eyes <- filter(first_condition_v3, first_condition_v3$`Eyes / Skin` == "EYE")

#first condition skin
first_condition_skin <- filter(first_condition_v3, first_condition_v3$`Eyes / Skin` == "SKIN")

#second condition eyes
second_condition_eyes <- filter(second_condition_v3, second_condition_v3$`Eyes / Skin` == "EYE")

#second condition skin 
second_condition_skin <- filter(second_condition_v3, second_condition_v3$`Eyes / Skin` == "SKIN")

#all conditions skin
all_conditions_skin <- rbind(first_condition_skin, second_condition_skin)

#all conditions eyes
all_conditions_eyes <- rbind(first_condition_eyes, second_condition_eyes)


#check if response times are normally distributed overall and in each condition 

#mean RT first condition_v3
mean_first_v3 <- mean(first_condition_v3$`Response Time`)

#sd RT first condition_v3
sd_first_v3 <- sd(first_condition_v3$`Response Time`)

#mean RT second condition_v3
mean_second_v3 <- mean(second_condition_v3$`Response Time`)

#sd RT second condition_v3
sd_second_v3 <- sd(second_condition_v3$`Response Time`)

#RT normal distribution first condition
ggqqplot(first_condition_v3$`Response Time`)
ggdensity(first_condition_v3$`Response Time`)
shapiro.test(first_condition_v3$`Response Time`)
hist(first_condition_v3$`Response Time`)

#RT normal distribution second condition 
ggqqplot(second_condition_v3$`Response Time`)
ggdensity(second_condition_v3$`Response Time`)
shapiro.test(second_condition_v3$`Response Time`)

#all conditions RT
ggqqplot(all_conditions$`Response Time`)
ggdensity(all_conditions$`Response Time`)
shapiro.test(all_conditions$`Response Time`)

#check frequency of answers 

#first condition
freq_table_1 <- table(first_condition_v3$Answer)
barplot(freq_table_1, main = "Binary Data Distribution", xlab = "Value", ylab = "Frequency")

#second condition
freq_table_2 <- table(second_condition_v3$Answer)
barplot(freq_table_2, main = "Binary Data Distribution", xlab = "Value", ylab = "Frequency")

#overall
freq_table_overall <- table(all_conditions$Answer)
barplot(freq_table_overall, main = "Binary Data Distribution", xlab = "Value", ylab = "Frequency")



#First Condition Statistics Response Time
null_model <- glmer(first_condition_v3$`Response Time` ~ 1  + (1|first_condition_v3$Stimuli) + (1|first_condition_v3$Participant) + (1|first_condition_v3$`Eyes / Skin`) + (1|first_condition_v3$`Left / Right`), data = first_condition_v3, family = Gamma)
summary(null_model)
actual_model <- glmer(first_condition_v3$`Response Time` ~ 1 + first_condition_v3$`Generated / Real` + (1|first_condition_v3$Stimuli) + (1|first_condition_v3$Participant) + (1|first_condition_v3$`Eyes / Skin`) + (1|first_condition_v3$`Left / Right`), data = first_condition_v3, family = Gamma)
summary(actual_model)

anova(null_model, actual_model, test = "chisq")

#Second Condition Statistics Response Time
null_model_2 <- glmer(second_condition_v3$`Response Time` ~ 1  + (1|second_condition_v3$Stimuli) + (1|second_condition_v3$Participant) + (1|second_condition_v3$`Eyes / Skin`) + (1|second_condition_v3$`Left / Right`), data = second_condition_v3, family = Gamma)
summary(null_model_2)
actual_model_2 <- glmer(second_condition_v3$`Response Time` ~ 1 + second_condition_v3$`Generated / Real` + (1|second_condition_v3$Stimuli) + (1|second_condition_v3$Participant) + (1|second_condition_v3$`Eyes / Skin`) + (1|second_condition_v3$`Left / Right`), data = second_condition_v3, family = Gamma)
summary(actual_model_2)



anova(null_model_2, actual_model_2, test = "chisq")

#All Statistics Response Time
null_model_3 <- glmer(all_conditions$`Response Time` ~ 1  + (1|all_conditions$Stimuli) + (1|all_conditions$Participant) + (1|all_conditions$`Eyes / Skin`) + (1|all_conditions$`Left / Right`), data = all_conditions, family = Gamma)
summary(null_model_3)
actual_model_3 <- glmer(all_conditions$`Response Time` ~ 1 + all_conditions$`Generated / Real` + (1|all_conditions$Stimuli) + (1|all_conditions$Participant) + (1|all_conditions$`Eyes / Skin`) + (1|all_conditions$`Left / Right`), data = all_conditions, family = Gamma)
summary(actual_model_3)

anova(null_model_3, actual_model_3, test = "chisq")

#Wilxoc Test
wilcox.test(first_condition_v3$`Response Time`, second_condition_v3$`Response Time`)

#ACCURACY

#first condition accuracy
accuracy_1 <- sum(first_condition_v3$Answer == first_condition_v3$RealValue) / nrow(first_condition_v3)
print(accuracy_1)

#second condition accuracy
accuracy_2 <- sum(second_condition_v3$Answer == second_condition_v3$RealValue) / nrow(second_condition_v3)
print(accuracy_2)

#all accuracy
accuracy_1 <- sum(first_condition_v3$Answer == first_condition_v3$RealValue) / nrow(first_condition_v3)
print(accuracy_1)

#second condition accuracy
accuracy_total <- sum(all_conditions$Answer == all_conditions$RealValue) / nrow(all_conditions)
print(accuracy_total)

#chi squre test for accuracy first condition
cont_table_1 <- table(first_condition_v3$Accuracy)
cont_table_1
chisq_result_1 <- chisq.test(cont_table_1)
chisq_result_1

#chi squre test for accuracy second condition
cont_table_2 <- table(second_condition_v3$Accuracy)
cont_table_2
chisq_result_2 <- chisq.test(cont_table_2)
chisq_result_2

#chi squre test for accuracy third condition
cont_table_3 <- table(all_conditions$Accuracy)
cont_table_3
chisq_result_3 <- chisq.test(cont_table_3)
chisq_result_3


#compare accuracy between two groups 
wilcox.test(first_condition_v3$Accuracy, second_condition_v3$Accuracy)
mean(first_condition_v3$Accuracy)
mean(second_condition_v3$Accuracy)

#compare accuracy between eyes and skin first condition
wilcox.test(first_condition_eyes$Accuracy, first_condition_skin$Accuracy)
mean(first_condition_eyes$Accuracy)
mean(first_condition_skin$Accuracy)

#compare accuracy between eyes and skin second condition
wilcox.test(second_condition_eyes$Accuracy, second_condition_skin$Accuracy)
mean(second_condition_eyes$Accuracy)
mean(second_condition_skin$Accuracy)

#accuray between eyes and skin all conditions 
wilcox.test(all_conditions_eyes$Accuracy, all_conditions_skin$Accuracy)
mean(all_conditions_eyes$Accuracy)
mean(all_conditions_skin$Accuracy)


all_conditions_generated <- filter(all_conditions, all_conditions$`Generated / Real` == "GENERATED")
all_conditions_real <- filter(all_conditions, all_conditions$`Generated / Real` == "REAL")
wilcox.test(all_conditions_generated$Accuracy, all_conditions_real$Accuracy)
mean(all_conditions_generated$Accuracy)
mean(all_conditions_real$Accuracy)
  


# create dummy variables using model.matrix()
dummies <- model.matrix(~ ., data = all_conditions)

# remove the intercept column from the dummy variable matrix
dummies <- dummies[, -1]

# combine the dummy variable matrix with the original dataframe
df_dummies <- cbind(all_conditions, dummies)
#logistic regression for accuracy 
model <- glm(Accuracy ~ df_dummies$`\`Generated / Real\`REAL`+ df_dummies$`\`Left / Right\`RIGHT` + df_dummies$`\`Eyes / Skin\`SKIN`, data = df_dummies, family = binomial)

# Check the summary of the model
summary(model)


