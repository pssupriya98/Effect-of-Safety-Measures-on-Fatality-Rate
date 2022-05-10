library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
install.packages('visreg')
library(visreg)

# PART A
accident_data <- read.csv("Kag_Airbag.csv")
accident_data$safety_measure <- 0
accident_data$safety_measure[accident_data$airbag == "none" & 
                          accident_data$seatbelt == "none"]<-"none"
accident_data$safety_measure[accident_data$airbag == "airbag" & 
                          accident_data$seatbelt == "none"]<-"AB"
accident_data$safety_measure[accident_data$airbag == "none" & 
                          accident_data$seatbelt == "belted"] <-"SB"
accident_data$safety_measure[accident_data$airbag == "airbag" & 
                          accident_data$seatbelt == "belted"] <-"ABSB"
highSpeed_accidents <- accident_data[accident_data$dvcat=="55+",]

# Plotting Safety System VS Severity of Injury
counts<-table(highSpeed_accidents$injSeverity, highSpeed_accidents$safety_measure)
counts
safetyGraph <- barplot(counts,main = "Safety System VS Severity of Injury",
                       xlab ="Safety system",ylab = "Frequency",
                       col = c("#733080","#F4BAC8","#A40607","#7288B9", 
                               "#F0C595","#000000"),
                       legend=c("None", "Possible Injury", "No Incapacity",
                                "Incapacity","Killed","Unkown",
                                "Prior Death"),beside = TRUE,
                       args.legend = list(x="topleft"))


# Plotting Safety System VS Survival
death_alive_count <-table(highSpeed_accidents$dead,highSpeed_accidents$safety_measure)
death_alive_count
deathGraph <- barplot(death_alive_count,main = "Safety System VS Survival",
                      xlab ="Safety system",ylab = "Survival",
                      col = c("#808080","#FFFFFF"),
                      legend=rownames(death_alive_count),beside = TRUE, args.legend = list(x="topleft"))

# Logistic Regression Model for Safety_Measures Vs Survival Rate

set.seed(1)

train_index = createDataPartition(y = highSpeed_accidents$dead, p = 0.70, list = FALSE)
train_data = highSpeed_accidents[train_index, ]
test_data = highSpeed_accidents[-train_index, ]


train_data <- train_data %>%
  mutate(dead = relevel(factor(dead),
                        ref = "alive"))

logit_model_safety_measures <- glm(dead ~ safety_measure, family = binomial, data = train_data)
predict(logit_model_safety_measures, train_data, type="response")

logit_model_safety_measures
summary(logit_model_safety_measures)
str(logit_model_safety_measures)



# Visualisation on how safety_measures affect survival rate
visreg(logit_model_safety_measures, "safety_measure", scale = "response")

# Linear Regression Model for Safety_Measures Vs Injury Rate
injury_severity<- lm(injSeverity ~ safety_measure, data = train_data)

# Visualisation on how safety_measures affects injury rate
visreg(injury_severity, "safety_measure", scale = "response")


#######################################################################################

# Part B

accidents_safety_measures <- accident_data[accident_data$safety_measure == 'ABSB',]

set.seed(1)
train_data = accidents_safety_measures
train_data <- train_data %>%
  mutate(dead = relevel(factor(dead),
                        ref = "alive"))

# Logistic Regression Model 
logit_model_speeds<- glm(dead ~ dvcat,family = binomial, data = train_data)

summary(logit_model_speeds)

# Visualisation on how speeds affects survival rate even with both safety measures
visreg(logit_model_speeds, "dvcat", scale = "response")



