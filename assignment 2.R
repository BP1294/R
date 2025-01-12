vanderbilt <- read.csv(url("https://laurencipriano.github.io/IveyBusinessStatistics/Datasets/VanderbiltData.csv"), 
                 header = TRUE)

head(vanderbilt)


#trying lasso
install.packages("glmnet")
library(glmnet)

X <- as.matrix(vanderbilt[, 4:19])
y <- vanderbilt$Actual
lasso_model <- cv.glmnet(X, y, alpha = 1)
best_lambda <- lasso_model$lambda.min
print(paste("Best lambda:", best_lambda))
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print(lasso_coefficients)


# Step 0: identify variables 
summary(vanderbilt)
par( mfrow= c( 1,3 ) )
###                    step 1: visualize the data               ###
## 1a. visualize univariate distribution
### histogram of Actual (Dependent variable)
hist(vanderbilt$Actual)

### histogram of Tless1 (Predictor Variable)
hist(vanderbilt$Tless1)

### histogram of Tless28 (Predictor variable)
hist(vanderbilt$Tless28)

## 1b. visualize bivariate distribution 
plot(vanderbilt$Tless1, vanderbilt$Actual, ylim=c(0,150))
abline(lm(Actual ~ Tless1, data = vanderbilt), col = "red")

plot(vanderbilt$Tless28, vanderbilt$Actual, ylim=c(0,150))
abline(lm(Actual ~ Tless28, data = vanderbilt), col = "red")


###         step 2: Evaluate Correlation among potential predictor variables    ### 
library(GGally)

pair1 <- data.frame(vanderbilt$Actual, vanderbilt$Tless1, vanderbilt$Tless2,
                    vanderbilt$Tless3, vanderbilt$Tless4, vanderbilt$Tless5,
                    vanderbilt$Tless6, vanderbilt$Tless7, vanderbilt$Tless8,
                    vanderbilt$Tless9, vanderbilt$Tless10, vanderbilt$Tless11,
                    vanderbilt$Tless12, vanderbilt$Tless13, vanderbilt$Tless14,
                    vanderbilt$Tless21, vanderbilt$Tless28)
ggpairs(pair1)

pair2 <- data.frame(vanderbilt$Actual, vanderbilt$Tless1, vanderbilt$Tless28)
ggpairs(pair2)



###        step 3: Develop candidate multiple regression models       ###

summary(vanderbilt)
options(cipen=999)

# Remove outliers to address skeweness
vanderbilt <- vanderbilt[vanderbilt$Tless1 >= 20,]

reg1 <- lm(Actual ~ DOW + Tless1 + Tless2 + Tless3 + Tless4 + Tless5
           + Tless6 + Tless7 + Tless8 + Tless9 + Tless10 + Tless11 +
             Tless12 + Tless13 + Tless14 + Tless21 + Tless28, data = vanderbilt)
summary(reg1)

# combining Wed & Thurs

vanderbilt$newDOW[vanderbilt$DOW == "Mon" | vanderbilt$DOW == "Tue"] <- "Mon and Tue"
vanderbilt$newDOW[vanderbilt$DOW == "Wed" | vanderbilt$DOW == "Thu"] <- "Wed and Thu"
vanderbilt$newDOW[vanderbilt$DOW == "Fri"] <- "Fri"
vanderbilt$newDOW <- factor(vanderbilt$newDOW, levels = c("Mon and Tue","Wed and Thu", "Fri"))

reg2 <- lm(Actual ~ newDOW + Tless1 + Tless10 + Tless14 + Tless21, data = vanderbilt)
summary(reg2)


# Assessing if it means assumption of regression 

par(mfrow= c(1,1))

# evaluating linearity:   residuals vs. leverage
plot(reg2,1)


# evaluating independent variable 
par(mfrow=c(1,1))
plot(rstandard(reg2))
abline(0, 0, lty=2, col="grey") # draw a straight line at 0 for a visual reference
lines(lowess( rstandard(reg2)), col = "red", lwd = 2)

# evaluating multicollinearity 
pair3 <- data.frame(vanderbilt$Tless1, vanderbilt$Tless10, vanderbilt$Tless14, vanderbilt$Tless21)
ggpairs(pair3)

# evaluating normal distribution of residuals
plot(reg2,2)



# Addressing the problems with the regressions
reg3 <- lm(Actual ~ newDOW + Tless1, data = vanderbilt)
summary(reg3)


# evaluating linearity:   residuals vs. leverage
plot(reg3,1)


# evaluating independent variable 
par(mfrow=c(1,1))
plot(rstandard(reg3))
abline(0, 0, lty=2, col="grey") # draw a straight line at 0 for a visual reference
lines(lowess( rstandard(reg3)), col = "red", lwd = 2)

# evaluating multicollinearity 
pair3 <- data.frame(vanderbilt$Tless1, vanderbilt$Tless10, vanderbilt$Tless14, vanderbilt$Tless21)
ggpairs(pair3)

# evaluating normal distribution of residuals
plot(reg3,2)

# pairwise - exploring relationship 
boxplot(Tless1 ~ newDOW, data = vanderbilt,
        xlab = "Day of the Week",
        ylab = "Tless1",
        col = "lightblue")



# prediction 

new_sched_surgery <- data.frame(newDOW = "Mon and Tue", Tless1 = 87)
predict(reg3,
        newdata = new_sched_surgery,
        interval = "confidence")





library(car)
vif(reg4)



