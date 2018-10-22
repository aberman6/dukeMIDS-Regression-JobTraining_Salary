setwd('/Users/annaberman/Desktop/702 Modeling/Assignments/Team Project 1')
library(ggplot2)
library(dplyr)
library(gridExtra)
library(arm)
library(MASS)
library(pROC)

# Load the data
lalonde <- read.csv('lalondedata.txt') %>%
    mutate(treat = as.factor(treat)) %>%
    mutate(treat = factor(treat, levels = c(0, 1))) %>%
    mutate(employed78 = ifelse(re78 > 0, 1, 0)) %>%
    mutate(employed75 = ifelse(re75 > 0, 1, 0)) %>%
    mutate(employed74 = ifelse(re74 > 0, 1, 0)) %>%
    # logs
    mutate(re78_altered = re78 + .00000001,
           re78L = log(re78_altered),
           re74_altered = re74 + .00000001,
           re74L = log(re74_altered),
           ageL = log(age)) %>%
    # mean centering
    mutate(re78c = re78 - mean(re78),
           re78cl = re78L - mean(re78L),
           re78_altered = re78 + .00000001,
           re74Lc = re74L - mean(re74L),
           re75c = re75 - mean(re75),
           re74c = re74 - mean(re74),
           agec = age - mean(age)) %>%
    # educ
    mutate(educ.bin = ifelse(educ < 9, 'MS or less',
                             ifelse(educ < 12, 'Some HS',
                                    ifelse(educ == 12, 'HS', 'More than HS')))) %>%
    mutate(educ.bin = factor(educ.bin, levels = c('HS', 'MS or less', 'Some HS', 'More than HS'))) %>%
    mutate(educ.bin2 = ifelse(educ < 9, 'MS or less', 'Some HS +')) %>%
    mutate(educ.bin2 = factor(educ.bin2, levels = c('Some HS +', 'MS or less'))) %>%
    # re74 bin
    mutate(re74.bin = ifelse(re74 < 2000, '<2000', '2000+')) %>%
    # quadratic age
    mutate(age2 = agec**2,
           age3 = agec**3) 
summary(lalonde)






# LOG: EXPLORATORY DATA ANALYSIS ------------------------------------------


# TREAT
# Negative effect
# You might be less likely to get the treatement if you're employeed already
tapply(lalonde$employed78, lalonde$treat, mean)

#EDUC
tapply(lalonde$employed78, lalonde$educ.bin, mean) 
plot(tapply(lalonde$employed78, lalonde$educ.bin, mean),
     ylim = c(0,1),
     xlab = 'educ.bin',
     ylab = 'Mean(employed78)')

# Bin MS or less, and then Some HS +
binnedplot(x = lalonde$educ, y = lalonde$employed78,
           xlab = 'Education', ylab = 'employed78 \'78 Cases',
           ylim = c(0,1))

# educ.bin2
tapply(lalonde$employed78, lalonde$educ.bin2, mean) 

# BLACK
# Negative effect
tapply(lalonde$employed78, lalonde$black, mean) 

# HISPANIC
# Positive effect
tapply(lalonde$employed78, lalonde$hispan, mean) 

# MARRIED
# Slight positive effect
# Not as big as effect as how much you make
tapply(lalonde$employed78, lalonde$married, mean) 


# NONDEGREE
# Use education
tapply(lalonde$employed78, lalonde$nodegree, mean) 



# AGE
# Linear with outier
binnedplot(x = lalonde$age, y = lalonde$employed78,
           xlab = 'Age', ylab = 'employed78 \'78 Cases',
           ylim = c(0,1))


# RE74
# Not a huge effect
binnedplot(x = lalonde$re74, y = lalonde$employed78,
           xlab = 'Re74', ylab = 'employed78 \'78 Cases',
           ylim = c(.6,1))

# RE75
# Not a huge effect
binnedplot(x = lalonde$re75, y = lalonde$employed78,
           xlab = 'Re75', ylab = 'employed78 \'78 Cases',
           ylim = c(.6,1))


# INTERACTION EFFECTS
par(mfrow = c(1,2))

# Treatment and 74 salary
# No reason for interaction effect
binnedplot(x = lalonde[lalonde$treat == 0, 're74'], y = lalonde[lalonde$treat == 0, 'employed78'],
           xlab = 'Re74', ylab = 'employed78 \'78 Cases',
           ylim = c(.6,1),
           xlim = c(0,20000),
           main = 'No Treatment')
binnedplot(x = lalonde[lalonde$treat == 1, 're74'], y = lalonde[lalonde$treat == 1, 'employed78'],
           xlab = 'Re74', ylab = 'employed78 \'78 Cases',
           ylim = c(.6,1),
           xlim = c(0,20000),
           main = 'Treatment')

# Treatment and 75 salary
# No reason for interaction effect
binnedplot(x = lalonde[lalonde$treat == 0, 're75'], y = lalonde[lalonde$treat == 0, 'employed78'],
           xlab = 'Re75', ylab = 'employed78 \'78 Cases', 
           ylim = c(.6,1),
           xlim = c(0,20000),
           main = 'No Treatment')
binnedplot(x = lalonde[lalonde$treat == 1, 're75'], y = lalonde[lalonde$treat == 1, 'employed78'],
           xlab = 'Re75', ylab = 'employed78 \'78 Cases',
           ylim = c(.6,1),
           xlim = c(0,20000),
           main = 'Treatment')

# Binary 75 employed
# Treatment doesn't have an effect for those who were in employed in 75
# but does have a significant effect if unemployed in 74
tab <- rbind(tapply(lalonde[lalonde$treat == 0, 'employed78'], lalonde[lalonde$treat == 0, 'employed75'], mean),
             tapply(lalonde[lalonde$treat == 1, 'employed78'], lalonde[lalonde$treat == 1, 'employed75'], mean))
rownames(tab) <- c('No Treatment', 'Treatment')
colnames(tab) <- c('Unemployed 75', 'Employed 75')
tab

# Binary 74 employed
# Interaction effect that might wash out when we add 75 interaction
tab <- rbind(tapply(lalonde[lalonde$treat == 0, 'employed78'], lalonde[lalonde$treat == 0, 'employed74'], mean),
             tapply(lalonde[lalonde$treat == 1, 'employed78'], lalonde[lalonde$treat == 1, 'employed74'], mean))
rownames(tab) <- c('No Treatment', 'Treatment')
colnames(tab) <- c('Unemployed 74', 'Employed 74')
tab


# Treatment and educ.bin2
# No reason for interaction effect
# Might help those with lower education
tab <- rbind(tapply(lalonde[lalonde$treat == 0, 'employed78'], lalonde[lalonde$treat == 0, 'educ.bin2'], mean),
             tapply(lalonde[lalonde$treat == 1, 'employed78'], lalonde[lalonde$treat == 1, 'educ.bin2'], mean))
rownames(tab) <- c('No Treatment', 'Treatment')
colnames(tab) <- c('Some HS +', 'MS or less')
tab




# LOG: MODEL FIT ----------------------------------------------------------

# Fit 1
# Treatment only
par(mfrow = c(1,1))
fit1 <- glm(employed78 ~ treat, data = lalonde, family = binomial)
summary(fit1)

# Area under the curve: 0.5087
roc(lalonde$employed78, fitted(fit1), plot=T, legacy.axes=T, print.thres="best")
threshold = .764
prop.table(table(lalonde$employed78, fit1$fitted > threshold),1)*100

# Fit 2
fit2 <- glm(employed78 ~ treat + agec + married + black + hispan + educ + re74 + re75, data = lalonde, family = binomial)
summary(fit2)

# Area under the curve: 0.649
roc(lalonde$employed78, fitted(fit2), plot=T, legacy.axes=T, print.thres="best")
threshold = .773
prop.table(table(lalonde$employed78, fit2$fitted > threshold),1)*100


# Fit 3
# Education bin2
fit3 <- glm(employed78 ~ treat + agec + married + black + hispan + educ.bin2 + re74 + re75, data = lalonde, family = binomial)
summary(fit3)

# Area under the curve: 0.6605
roc(lalonde$employed78, fitted(fit3), plot=T, legacy.axes=T, print.thres="best")
threshold = .803
prop.table(table(lalonde$employed78, fit3$fitted > threshold),1)*100


# Fit 5
# Binary 75 salary interaction
fit5 <- glm(employed78 ~ treat*employed75 + agec + married + black + hispan + educ.bin2 + re74, data = lalonde, family = binomial)
summary(fit5)

# Area under the curve: 0.6535
roc(lalonde$employed78, fitted(fit5), plot=T, legacy.axes=T, print.thres="best")
threshold = .750
prop.table(table(lalonde$employed78, fit5$fitted > threshold),1)*100


# Fit 6
# Binary 74 salary
fit6 <- glm(employed78 ~ treat + employed74 + agec + married + black + hispan + educ.bin2 + re75, data = lalonde, family = binomial)
summary(fit6)

# Area under the curve: 0.657
roc(lalonde$employed78, fitted(fit6), plot=T, legacy.axes=T, print.thres="best")
threshold = .804
prop.table(table(lalonde$employed78, fit6$fitted > threshold),1)*100


# Fit 7
# Binary 74 salary + interaction
fit7 <- glm(employed78 ~ treat*employed74 + agec + married + black + hispan + educ.bin2 + re75, data = lalonde, family = binomial)
summary(fit7)

# Area under the curve: 0.6697
roc(lalonde$employed78, fitted(fit7), plot=T, legacy.axes=T, print.thres="best")
threshold = .777
prop.table(table(lalonde$employed78, fit7$fitted > threshold),1)*100


# Fit 8
# Binary 74 salary + interaction + binary 75
fit8 <- glm(employed78 ~ treat*employed74 + agec + married + black + hispan + educ.bin2 + employed75, data = lalonde, family = binomial)
summary(fit8)

# Area under the curve: 0.6618
roc(lalonde$employed78, fitted(fit8), plot=T, legacy.axes=T, print.thres="best")
threshold = .764
prop.table(table(lalonde$employed78, fit8$fitted > threshold),1)*100


# Fit 9
# Binary 74 salary + interaction 
# Educ.bin2 interaction
fit9 <- glm(employed78 ~ treat*(employed74 + educ.bin2) + agec + married + black + hispan + re75, data = lalonde, family = binomial)
summary(fit9)

# Area under the curve: 0.6689
roc(lalonde$employed78, fitted(fit9), plot=T, legacy.axes=T, print.thres="best")
threshold = .779
prop.table(table(lalonde$employed78, fit9$fitted > threshold),1)*100


final_fit <- fit7

# Fit 10
# Binary 74 salary + interaction 
# Quadratic age
fit10 <- glm(employed78 ~ treat*employed74 + agec + age2 + age3 + married + black + hispan + educ.bin2 + re75, data = lalonde, family = binomial)
summary(fit10)
exp((summary(fit10)$coefficients))
exp(confint(fit10))

# Area under the curve: 0.6823
roc(lalonde$employed78, fitted(fit10), plot=T, legacy.axes=T, print.thres="best")
threshold = .752
prop.table(table(lalonde$employed78, fit10$fitted > threshold),1)*100


final_fit <- fit10





# LOG: RESIDUALS ----------------------------------------------------------

rawreds <- lalonde$employed78 - fitted(final_fit)

# educ.bin2
tapply(rawreds, lalonde$educ.bin2, mean) 

# black
tapply(rawreds, lalonde$black, mean) 

# hispan
tapply(rawreds, lalonde$hispan, mean) 

# married
tapply(rawreds, lalonde$married, mean)

# employed74
tapply(rawreds, lalonde$employed74, mean) 


# AGE
# add a third-degree poly?
binnedplot(x = lalonde$age, y = rawreds,
           xlab = 'Age', ylab = 'Residuals')

# Re75
# Outlier
# Model might not fit well for those that are older or make a lot of money
binnedplot(x = lalonde$re75, y = rawreds,
           xlab = 'Re75', ylab = 'employed78 \'78 Cases')




