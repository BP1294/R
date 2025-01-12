################################################################################
###     How does frequency of social activity associate with happiness?      ###
################################################################################


##############################
# library, load data & others
##############################
library(mice)
library(dplyr)
library(GGally)
library(caret)
library(car)
library(ResourceSelection)

options(scipen = 5,
        digit =5)

happy <- read.csv("C:/Users/nones/OneDrive/Documents/Ivey/Term 1/Business Statistic/Assignment 3/GSS_HappyData.csv") 


################################
# selected columns for new data 
################################
select_happy <- happy %>%
  select(VHAPPY  , AGE     , SEX     ,
         SOCBAR  , SOCFREND, SOCOMMUN, SOCREL , TVHOURS , 
         LIFE    , HELPFUL, ATTEND)

################################
# evaluate chosen data points
# - rows: 67588 
# - col:  13
################################
dim(select_happy)
summary(select_happy)
colSums(is.na(select_happy))


###################################################
# Treatment of the NULLs - replace all with median
###################################################
before <- summary(select_happy)

# Age - continuous
median_age                                <- median(select_happy$AGE, na.rm = TRUE)
select_happy$AGE[is.na(select_happy$AGE)] <- median_age

# SEX - binary 
median_sex                                <- median(select_happy$SEX, na.rm = TRUE)
select_happy$SEX[is.na(select_happy$SEX)] <- median_sex

# spending evening at bar
median_socbar                                    <- median(select_happy$SOCBAR, na.rm = TRUE)
select_happy$SOCBAR[is.na(select_happy$SOCBAR)]  <- median_socbar

# spend evening with friends
median_socfrend                                      <- median(select_happy$SOCFREND, na.rm = TRUE)
select_happy$SOCFREND[is.na(select_happy$SOCFREND)]  <- median_socfrend

# spend evening with neighbor
median_socommun                                      <- median(select_happy$SOCOMMUN, na.rm = TRUE)
select_happy$SOCOMMUN[is.na(select_happy$SOCOMMUN)]  <- median_socommun

# spend evening with relatives
median_socrel                                        <- median(select_happy$SOCREL, na.rm = TRUE)
select_happy$SOCREL[is.na(select_happy$SOCREL)]      <- median_socrel   

# spend evening with relatives
median_tvhours                                         <- median(select_happy$TVHOURS, na.rm = TRUE)
select_happy$TVHOURS[is.na(select_happy$TVHOURS)]      <- median_tvhours  

# is life exciting or dull
median_life                                      <- median(select_happy$LIFE, na.rm = TRUE)
select_happy$LIFE[is.na(select_happy$LIFE)]      <- median_life  

# people helpful or looking out for selves
median_helpful                                         <- median(select_happy$HELPFUL, na.rm = TRUE)
select_happy$HELPFUL[is.na(select_happy$HELPFUL)]      <- median_helpful  

# how often r attends religious services
median_attend                                         <- median(select_happy$ATTEND, na.rm = TRUE)
select_happy$ATTEND[is.na(select_happy$ATTEND)]       <- median_attend 

after <- summary(select_happy)

before
after
###################
# Analysis of data: univariate
###################
for(i in c(1:11)){
  hist(select_happy[, i],
       main = colnames(select_happy)[i], 
       xlab = colnames(select_happy)[i])
}

table(select_happy$SEX)
t(table(select_happy$SEX == 1, select_happy$SEX == 2))/colSums(table(select_happy$SEX == 1, select_happy$SEX == 2))

counts <- table(select_happy$SOCBAR)
total <- sum(counts)
proportions <- counts/total
proportions

counts_SOCFREND <- table(select_happy$SOCFREND)
total_SOCFREND <- sum(counts_SOCFREND)
proportions_SOCFREND <- counts_SOCFREND/total_SOCFREND
proportions_SOCFREND


counts_SOCOMMUN <- table(select_happy$SOCOMMUN)
total_SOCOMMUN <- sum(counts_SOCOMMUN)
proportions_SOCOMMUN <- counts_SOCOMMUN/total_SOCOMMUN
proportions_SOCOMMUN

counts_SOCREL <- table(select_happy$SOCREL)
total_SOCREL <- sum(counts_SOCREL)
proportions_SOCREL <- counts_SOCREL/total_SOCREL
proportions_SOCREL


counts_LIFE <- table(select_happy$LIFE)
total_LIFE <- sum(counts_LIFE)
proportions_LIFE <- counts_LIFE/total_LIFE
proportions_LIFE

counts_HELPFUL <- table(select_happy$HELPFUL)
total_HELPFUL <- sum(counts_HELPFUL)
proportions_HELPFUL <- counts_HELPFUL/total_HELPFUL
proportions_HELPFUL

counts_HELPFUL <- table(select_happy$HELPFUL)
total_HELPFUL <- sum(counts_HELPFUL)
proportions_HELPFUL <- counts_HELPFUL/total_HELPFUL
proportions_HELPFUL

###################
# Analysis of data: bivariate
###################
cor(select_happy$AGE, select_happy$TVHOURS)


pair <- data.frame(select_happy$VHAPPY, select_happy$AGE, select_happy$SEX,
                   select_happy$TVHOURS)
## ggpairs(pair)



###################################
##    Evaluate log linearity 
###################################

# create binary variable for VHAPPY
table(select_happy$VHAPPY)

###################################
##   FUNCTIONS 
###################################

check.logLin <- function(var.cont, var.name, var.DV, lengthbins = 95, ordinal=FALSE) {
  # Create categorical variable
  # Identify where you want to create the bins
  if(ordinal == TRUE){
    bins = c(-99, as.numeric(levels(as.factor(var.cont))))
  } else {
    bins <- c(min(var.cont, na.rm=TRUE), 
              seq(max(quantile(var.cont, 0.025, na.rm=TRUE), min(var.cont, na.rm=TRUE) + 1), 
                  quantile(var.cont, 0.975, na.rm=TRUE), length = lengthbins))
  }
  
  # Initialize the new categorical variable
  var.cat <- cut(var.cont, breaks = bins, include.lowest = TRUE)
  var.cat <- factor(var.cat, levels = levels(var.cat))  # Ensure correct levels
  
  # Calculate the proportion vaccinated in each bin
  ct.All <- table(var.cat)
  ct.Pos <- table(var.cat[var.DV == 1])
  
  # Create a data frame with the counts and proportions
  ct.data <- data.frame(Labels = bins[-1],
                        EventCount = as.numeric(ct.Pos),
                        SampleCount = as.numeric(ct.All))
  
  ct.data$proportion <- ct.data$EventCount / ct.data$SampleCount
  
  # Avoid division by zero when calculating odds
  ct.data$odds <- with(ct.data, ifelse(proportion == 1, Inf, 
                                       ifelse(proportion == 0, NA, proportion / (1 - proportion))))
  
  # Calculate log-odds
  ct.data$log.odds <- log(ct.data$odds)
  
  # Visualize the log-odds
  plot(ct.data$Labels, ct.data$log.odds, xlab = paste0("Predictor variable:  ", var.name), 
       ylab = "Log(Odds)", main = "Log(Odds) vs. Binned Categories")
  
  # Fit a linear model to the data
  abline(lm(log.odds ~ Labels, data = ct.data), col = "red")
  
  # Return the data for further inspection
  #return(ct.data)
}



accuracy.table <- function(LR, data, truth, n.threshold){
  
  # initialize output dataframe
  output.table <- data.frame(threshold = rep(NA, n.threshold), 
                             sens = rep(NA, n.threshold), 
                             spec = rep(NA, n.threshold), 
                             accur = rep(NA, n.threshold) )
  
  threshold.list <- seq(0, 1, length = n.threshold)
  
  # generate predictions
  fit <- predict(LR, newdata = data, type="response")
  
  
  for (i in c(1:n.threshold)){          
    th = threshold.list[i] # choose a threshold value
    
    predict <- rep(0, length = length(fit))   # initialize 
    predict[fit >= th] <- 10   # predict all values greater than the threshold are "1s"
    
    predict = factor(predict, levels = c(0, 10))
    
    confusion <- table(truth, predict)
    accuracy.1 = confusion/rowSums(confusion)
    
    sens = accuracy.1[2,2]
    spec = accuracy.1[1,1]
    
    # sum of specificity and sensitivity
    accuracy.2 = (spec + sens )/2
    
    output.table[i, 1] <- th
    output.table[i, 2] <- sens
    output.table[i, 3] <- spec
    output.table[i, 4] <- accuracy.2
    
  }
  
  return(output.table)
  
}

######################
#  use function 
######################

check.logLin(var.cont = select_happy$AGE, 
             var.name = "AGE",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$SOCBAR, 
             var.name = "SOCBAR",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$SOCFREND, 
             var.name = "SOCFREND",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$SOCOMMUN, 
             var.name = "SOCOMMUN",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$SOCREL, 
             var.name = "SOCREL",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$TVHOURS, 
             var.name = "TVHOURS",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$LIFE, 
             var.name = "LIFE",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)

check.logLin(var.cont = select_happy$HELPFUL, 
             var.name = "HELPFUL",
             var.DV = select_happy$VHAPPY,
             lengthbins = 95)






################################
#          fix coding
################################

# VHAPPY
select_happy$VHAPPY <- as.factor(select_happy$VHAPPY)

# SEX
select_happy$SEX <- ifelse(select_happy$SEX == 1, 0, 1)
select_happy$SEX <-factor(select_happy$SEX,
                          levels = c(0,1),
                          labels = c("Male","Female"))
table(select_happy$SEX)


# SOCBAR
select_happy$SOCBAR <- factor(select_happy$SOCBAR,
                              levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("Almost Daily", "Sev Times a Week", "Sev Times a Month", 
                                         "Once a Month", "Sev Times a Year", "Once a Year", "Never"),
                              ordered = FALSE)

# SOCFREND
select_happy$SOCFREND <- factor(select_happy$SOCFREND,
                                levels = c(1, 2, 3, 4, 5, 6, 7),
                                labels = c("Almost Daily", "Sev Times a Week", "Sev Times a Month", 
                                           "Once a Month", "Sev Times a Year", "Once a Year", "Never"),
                                ordered = FALSE)


# SOCOMMUN
select_happy$SOCOMMUN <- factor(select_happy$SOCOMMUN,
                                levels = c(1, 2, 3, 4, 5, 6, 7),
                                labels = c("Almost Daily", "Sev Times a Week", "Sev Times a Month", 
                                           "Once a Month", "Sev Times a Year", "Once a Year", "Never"),
                                ordered = FALSE)


# SOCREL
select_happy$SOCREL <- factor(select_happy$SOCREL,
                              levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("Almost Daily", "Sev Times a Week", "Sev Times a Month", 
                                         "Once a Month", "Sev Times a Year", "Once a Year", "Never"),
                              ordered = FALSE)


# LIFE
select_happy$LIFE <- factor(select_happy$LIFE,
                            levels = c(1, 2, 3),
                            labels = c("Exciting", "Routine", "Dull"),
                            ordered = FALSE)


# HELPFUL
select_happy$HELPFUL <- factor(select_happy$HELPFUL,
                               levels = c(1, 2, 3),
                               labels = c("Try to be helpful", "Looking out for themselves", "Depends"),
                               ordered = FALSE)


# ATTEND 
select_happy$ATTEND <- factor(select_happy$ATTEND,
                                  levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                                  labels = c("Never", "< 1 Year", "1-2x a Year", "Sev Times a Year",
                                             "Once a Month", "2-3x a Month", "~ Every week", "Every Week", 
                                             "Sev times a Week"),
                                  ordered = FALSE)


# SOCBAR + SOCFREND + SOCOMMUN + SOCREL + LIFE + HELPFUL + ATTEND


#########################################
## Split data into training and testing
#########################################

train.size  <- 0.7 # use 70% of data for training
train.index <- createDataPartition(as.factor(select_happy$VHAPPY), p= train.size, list=F)

happy.train <- select_happy[train.index, ]
happy.test  <- select_happy[-train.index, ]

nrow(happy.train)
nrow(happy.test)


summary(happy.train)

###################################
## Logistic Regression with Each Predictor
###################################

## Understand which variables are good predictors on their own
for(i in c(2:11)){
  Reg1 <- glm(data = happy.train, 
              VHAPPY ~ happy.train[, i],
              family = 'binomial')
  print(list( colnames(happy.train)[i] , summary(Reg1), 
              exp(cbind(Reg1$coefficients, confint(Reg1)))
  ))
  
}


###################################
##   Develop candidate multiple variable models - using training data
###################################

# first model
RegA <- glm(data = happy.train,
            VHAPPY ~ AGE + SEX + SOCBAR + SOCFREND + SOCOMMUN + SOCREL + LIFE + HELPFUL + ATTEND,
            family = 'binomial')
summary(RegA)
exp(cbind(RegA$coefficients, confint(RegA)))


# second model 
RegB <- glm(data = happy.train,
            VHAPPY ~ AGE + ATTEND + LIFE + HELPFUL + SOCFREND + SOCOMMUN + SOCREL,
            family = 'binomial')
summary(RegB)
vif(RegB)

# third model 
RegC <- glm(data = happy.train,
            VHAPPY ~ AGE + ATTEND + LIFE + HELPFUL + SOCREL + SOCFREND,
            family = 'binomial')
summary(RegC)




# 4th model
# combining SOCFREND 

table(select_happy$SOCFREND)
select_happy$newSOCFREND <- NA
select_happy$newSOCFREND[select_happy$SOCFREND == "Once a Year" | select_happy$SOCFREND == "Never" | select_happy$SOCFREND == "Sev Times a Year" |
                           select_happy$SOCFREND == "Once a Month"| select_happy$SOCFREND == "Sev Times a Week"] <- "Infrequent Socializing"
select_happy$newSOCFREND[select_happy$SOCFREND == "Sev Times a Month"] <- "Sev Times a Month" 
select_happy$newSOCFREND[select_happy$SOCFREND == "Almost Daily"] <- "Almost Daily" 

RegD <- glm(data = happy.train,
            VHAPPY ~ AGE + ATTEND + LIFE + HELPFUL + SOCREL + newSOCFREND,
            family = 'binomial')
summary(RegD)



# 5th model
table(select_happy$ATTEND)
proportions_SOCFREND

select_happy$newATTEND <- NA
select_happy$newATTEND[select_happy$ATTEND == "Never"] <- "Never"#
select_happy$newATTEND[select_happy$ATTEND == "< 1 Year" | select_happy$ATTEND == "1-2x a Year"] <- "Rare Attendance"#
select_happy$newATTEND[select_happy$ATTEND == "Sev Times a Year"] <- "Sev Times a Year"
select_happy$newATTEND[select_happy$ATTEND == "Once a Month"] <- "Monthly"#
select_happy$newATTEND[select_happy$ATTEND == "2-3x a Month"] <- "2-3x a Month" #
select_happy$newATTEND[select_happy$ATTEND == "~ Every week"] <- "Almost Weekly"#
select_happy$newATTEND[select_happy$ATTEND == "Every Week"] <- "Every Week"#
select_happy$newATTEND[select_happy$ATTEND == "Sev times a Week"] <- "Sev Times a Week"#


happy.train <- select_happy[train.index, ]
happy.test  <- select_happy[-train.index, ]

table(select_happy$newATTEND)

RegH <- glm(data = happy.train,
            VHAPPY ~ AGE + newATTEND + LIFE + HELPFUL + SOCREL + newSOCFREND,
            family = 'binomial')
summary(RegH)



HL <- hoslem.test(RegH$y, fitted(RegH), g=10)
HL


barplot(t(cbind(HL$expected[, 2], HL$observed[, 2])), 
        beside = TRUE, 
        col = c("blue", "red"), 
        names.arg = c(1:10), 
        #  ylim = c(0, 1200),
        las = 2,
        main = "Expected vs Observed Values",
        xlab = "Group (equal number of observations in each bin)", ylab = "Value")
legend("topleft", legend = c("Expected (average of predicted values)", "Observed frequency"),
       fill = c("blue", "red"), bty = "n")




roc_data1 <- roc(happy.train$VHAPPY~ predict(RegD, newdata = meps.train, type="response"))
roc_data2 <- roc(happy.train$VHAPPY~ predict(RegH, newdata = meps.train, type="response"))

plot(roc_data1, col="red", smooth=TRUE)
lines(roc_data2, col="blue", smooth=TRUE)
