# Assignment 1

options(scipen=99)

# installed excel package 
install.packages("readxl")

# load readxl
library(readxl)

# read excel file
data <- read_excel("C:/Users/nones/OneDrive/Documents/Ivey/Term 1/Business Statistic/Assignment 1/PilgrimBank.xlsx")

# load data
head(data)

# Calculate the mean of 1999 customer profitability
mean_profit <- mean(data$Profit, na.rm = TRUE)

# calculate 95% C.I. around the mean

# calculate sd, se
std_dev <- sd(data$Profit, na.rm = TRUE)

n <- sum(!is.na(data[["Profit"]]))
z <- 1.96 

se <- std_dev/sqrt(n)

lower_bound <- mean_profit - (z*se)
upper_bound <- mean_profit + (z*se)

cat("95% C.I. :[", lower_bound, ",", upper_bound, "]")


# draw histogram

hist(data[["Profit"]],
     main = "Histogram of Customer Profitability",
     xlab = "Profitability")


# Including the line to show the mean
abline(v = mean_profit, col = "red", lwd = 2, lty = 2)

# draw QQ plot
qqnorm(data[["Profit"]],
       main = "QQ Plot of Profit",
       ylab = "Profitability")
qqline(data[["Profit"]], col = "red", lwd = 2)



# t-test 
profitability_online <- data[data$Online == 1, "Profit"]
profitability_non_online <- data[data$Online == 0, "Profit"]

t_test_result <- t.test(profitability_online, profitability_non_online)
print(t_test_result)

# Mann-Whitney U test (Wilkcoxon Rank Sum)
mwu_result <- wilcox.test(Profit ~ Online, data = data)

print(mwu_result)

# Chi-Squared
temp_table <- table(data$Online, data$District)
print(temp_table)

chi_square_test <- chisq.test(temp_table)
print(chi_square_test)




## REGRESSION ## 
library(readxl)
data <- read_excel("C:/Users/nones/OneDrive/Documents/Ivey/Term 1/Business Statistic/Assignment 1/PilgrimBank.xlsx")
head(data)

# Binary Predictor
data$Online <-factor(data$Online,
                     levels = c(0,1),
                     labels = c("Not Online","Online"))



# Set categorical variable
data$District <- factor(data$District,
                        levels = c(1100,1200,1300),
                        labels = c('1100',
                                   '1200',
                                   '1300'))

# when introduced district, income follows a linear pattern
data$Inc <- factor(data$Inc,
                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                   labels = c("< $15,000",
                              "$15,000-$19,999",
                              "$20,000-$29,999",
                              "$30,000-$39,999",
                              "$40,000-$49,999",
                              "$50,000-$74,999",
                              "$75,000-$99,999",
                              "$100,000-$124,999",
                              "> $125,000"))

# Linear Regression with one binary predictors
# Online users compared to non-online users, add 5.003 to Profit
# P-Value > 0.05, do not reject H0 due to insufficient evidence that there is an effect
linear_regression <- lm(Profit ~ Inc, data=data)
summary(linear_regression)

# Multiple linear regression (binary and continuous)
# Tenure: for each year, profit goes up by 5.6911 with 95% C.I. of [5.27, 6.12]
# Reject H0, statistically significant that tenure has an effect on profit 
# Online: online users compared to non-online users add 16.67 to profit
# Reject H0, online is statistically significant 
reg2 <- lm(Profit ~ Tenure + Online, data=data)
summary(reg2)



# Multiple Linear Regression (Continuous, Binary and Categorical)
reg3 <- lm(Profit ~ Tenure + Online + District, data=data)
summary(reg3)


reg5 <- lm(Profit ~ Tenure + Online + District + Inc, data=data)
summary(reg5)

reg6 <- lm(Profit ~ Tenure + Age + Online + District + Inc, data=data)
summary(reg6)

# prediction with linear regression
online_person <- data.frame(Tenure = 30,
                          Age = 5,
                          Online = "Online",
                          District = "1200",
                          Inc = 7)
predict(reg6,
        newdata = online_person,
        interval = "confidence")

non_online <- data.frame(Tenure = 30,
                         Age = 5,
                         Online = "Not Online",
                         District = "1200",
                         Inc = 7)
predict(reg6,
        newdata = non_online,
        interval = "confidence")

reg7 <- lm(Profit ~ Tenure + Online + District + Inc*Online, data=data)
summary(reg7)
head(data)

install.packages("car")
library(car)
vif_values <- vif(reg6)
print(vif_values)

cor(data$Tenure, data$Age)        # 0.42
cor(data$Tenure, data$Inc)        # 0.04
cor(data$Tenure, data$District)   # -0.01
cor(data$Tenure, data$Online)     # -0.08

cor(data$Age, data$Inc)           # -0.07
cor(data$Age, data$District)      # -0.03
cor(data$Age, data$Online)        # -0.16

cor(data$Inc, data$Online)        # 0.08
cor(data$Inc, data$District)      # 0.03

cor(data$District, data$Online)   # 0.01
















par(mfrow=c(2,2))
plot(reg6)




# Tenure and age are positively correlated 
tenure_age <- lm(Tenure ~ Age, data=data)
summary(tenure_age)
plot(data$Tenure, data$Age, main = "Tenure and Age",
     xlab="Predictor X",
     ylab="Predictor Y",
     pch=19, col="blue")
abline(other_reg, col = "red", lwd = 2)

ten_age <- cor(data$Tenure, data$Age)
print(ten_age)

# Tenure and Income
tenure_income <- lm(Income ~ Tenure, data=data)
summary(tenure_income)
plot(data$Income, data$Tenure, main = "Tenure and Income",
     xlab="Predictor X",
     ylab="Predictor Y",
     pch=19, col="blue")
abline(other_reg, col = "red", lwd = 2)


# age acts as a continuous variable so I will not factor it
data$Age <- factor(data$Age,
                   levels = c(1,2,3,4,5,6,7),
                   labels = c("< 15 years",
                              "15-24 years",
                              "25-34 years",
                              "35-44 years",
                              "45-54 years",
                              "55-64 years",
                              "> 65 years"))

data$Billpay <- factor(data$Billpay,
                       levels = c(0,1),
                       labels = c("No", "Yes"))

hist(data[["Inc"]])
hist(data[["Tenure"]])
summary(data$Tenure)
data$Tenure <- log(data$Tenure)
