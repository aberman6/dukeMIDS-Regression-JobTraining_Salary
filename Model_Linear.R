setwd('/Users/annaberman/Desktop/702 Modeling/Assignments/Team Project 1')
library(ggplot2)
library(dplyr)
library(gridExtra)


# Load the data
lalonde <- read.csv('lalondedata.txt') %>%
    mutate(treat = as.factor(treat)) %>%
    mutate(treat = factor(treat, levels = c(0, 1))) %>%
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
    # re74 bin
    mutate(re74.bin = ifelse(re74 < 2000, '<2000', '2000+'))
summary(lalonde)



# LINEAR: EXPLORATORY DATA ANALYSIS -----------------------------------------------

# Don't include nodegree
plot(nodegree ~ educ, data = lalonde)
table(lalonde$nodegree, lalonde$educ)

# TREAT
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re78)) + 
    ylim(c(0,60000))

# AGE
# Maybe needs a log
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = re78)) + 
    ylim(c(0,61000))

# EDUC
# Linear positive effect past 8th grade
# Bin before 8th, HS, post
# Maybe continuous, maybe bins
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(educ), y = re78)) + 
    ylim(c(0,61000))
table(as.factor(lalonde$educ))

ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = educ.bin, y = re78)) + 
    ylim(c(0,61000))

# BLACK
# Negative effect, outlier 
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78)) + 
    ylim(c(0,61000))

# HISPAN
# Maybe, outlier non-hispanic
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = re78)) + 
    ylim(c(0,61000))

# MARRIED
# Positive
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = re78)) + 
    ylim(c(0,61000))

# NODEGREE
# Negative
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(nodegree), y = re78)) + 
    ylim(c(0,61000))


# RE74
# Negative
ggplot(lalonde) +
    geom_point(mapping = aes(x = re74, y = re78)) + 
    ylim(c(0,61000)) + 
    xlim(c(0,61000)) 

# RE75
# Negative
ggplot(lalonde) +
    geom_point(mapping = aes(x = re75, y = re78)) + 
    ylim(c(0,61000)) + 
    xlim(c(0,61000))

# RE74 vs. RE75
# People making less in 75 than 74
ggplot(lalonde) +
    geom_point(mapping = aes(x = re74, y = re75)) + 
    ylim(c(0,61000)) + 
    xlim(c(0,61000))

# Histogram of salaries per year
ggplot(lalonde) +
    geom_histogram(mapping = aes(re78, alpha = .5), fill = 'green') + 
    geom_histogram(mapping = aes(re74, alpha = .5), fill = 'blue') +
    geom_histogram(mapping = aes(re75, alpha = .5), fill = 'red') 

# We can have both 74 and 75 in our model!
cor(lalonde$re74, lalonde$re75)



# INTERACTION EFFECTS
# AGE
# Probably no interaction effect
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)

# Logging didn't help
ggplot(lalonde) +
    geom_point(mapping = aes(x = ageL, y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)

# EDUC
# Yes
# The more education you have the better the treatment works
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(educ), y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)
table(as.factor(lalonde$educ))

ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = educ.bin, y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)


# No interaction
ggplot(lalonde) +
    geom_point(mapping = aes(x = re74, y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ educ.bin)



# BLACK
# Probably not
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)

# HISPAN
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)

# MARRIED
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)

# NODEGREE
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(nodegree), y = re78)) + 
    ylim(c(0,61000)) + 
    facet_grid(. ~ treat)






# LINEAR: LOGGED OUTCOME -----------
# TREAT
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re78L))

# AGE
# Linear trend over time
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = re78L)) 

# Still no log
ggplot(lalonde) +
    geom_point(mapping = aes(x = ageL, y = re78L))

# EDUC
# Linear positive effect past 8th grade
# Bin before 8th, HS, post
# Maybe continuous, maybe bins
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(educ), y = re78L)) 

# BLACK
# Negative effect, outlier 
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78L)) 
# HISPAN
# Maybe, outlier non-hispanic
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = re78L)) 

# MARRIED
# Positive
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = re78L)) 

# NODEGREE
# Negative
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(nodegree), y = re78L)) 



# INTERACTION EFFECTS
# AGE
# Probably no interaction effect
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = re78L)) + 
    facet_grid(. ~ treat)

# Logging didn't help
ggplot(lalonde) +
    geom_point(mapping = aes(x = ageL, y = re78L)) + 
    facet_grid(. ~ treat)

# EDUC
# Yes
# The more education you have the better the treatment works
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(educ), y = re78L)) + 
    facet_grid(. ~ treat)

# BLACK
# Probably not
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78L)) + 
    facet_grid(. ~ treat)

# HISPAN
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = re78L)) + 
    facet_grid(. ~ treat)

# MARRIED
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = re78L)) + 
    facet_grid(. ~ treat)

# NODEGREE
# No
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(nodegree), y = re78L)) + 
    facet_grid(. ~ treat)



# LINEAR: MODEL FITTING -----------------------------------------------------------

# Logged outcome
# multiple ways of binning education
# interaction with education
# interaction with black

# Fit 1
# Treatment has negative effect
# Doesn't control for starting salary
fit1 <- lm(re78c ~ treat, data = lalonde)
summary(fit1)

# Fit 2
fit2 <- lm(re78c ~ treat + age + educ + black + hispan + married + re74c + re75c, data = lalonde)
summary(fit2)

# Fit 3
# Logged re78
# Big fat nope
fit3 <- lm(re78cl ~ treat + age + educ + black + hispan + married + re74c + re75c, data = lalonde)
summary(fit3)

# Fit 4
# Binned education
fit4 <- lm(re78c ~ treat + age + educ.bin + black + hispan + married + re74c + re75c, data = lalonde)
summary(fit4)

# Fit 5
# Interaction with educ.in
fit5 <- lm(re78c ~ treat*educ.bin + age + black + hispan + married + re74c + re75c, data = lalonde)
summary(fit5)
# Not really sig 0.283
anova(fit4, fit5)

# Fit 6
# Interaction with educ.in
fit6 <- lm(re78c ~ treat*educ + age + black + hispan + married + re74c + re75c, data = lalonde)
summary(fit6)
# Not really sig 0.1096
anova(fit6, fit4)

# Fit 7
# Interaction with black
fit7 <- lm(re78c ~ treat*black + age + educ.bin + hispan + married + re74c + re75c, data = lalonde)
summary(fit7)
# Not significant
anova(fit4, fit7)


# Fit 8
# Interaction between educ.bin and re74
# It works, but for the appendix
# Outlier
fit8 <- lm(re78c ~ treat + age + educ.bin*re74c + black + hispan + married + re75c, data = lalonde)
summary(fit8)
# Yes!
anova(fit4, fit8)


# Fit 9
# Interaction between educ.bin and re74 and black
# It works, but for the appendix
# Outlier
fit9 <- lm(re78c ~ treat + age + educ.bin*re74c + educ.bin*black + hispan + married + re75c, data = lalonde)
summary(fit9)
# Yes!
anova(fit9, fit8)




final_fit <- fit4
summary(final_fit)

# Fit 10
# Interaction between educ.bin and re74 and black
# It works, but for the appendix
# Outlier
fit10 <- lm(re78c ~ treat + age + educ.bin + black + hispan + married + re74Lc + re75c, data = lalonde)
summary(fit10)
# Yes!
anova(fit9, fit8)



# LINEAR: RESIDUALS ---------------------------------------------------------------

# TREAT
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = final_fit$residuals))

# AGE
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = final_fit$residuals)) 

# EDUC
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = educ.bin, y = final_fit$residuals))

# BLACK
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = final_fit$residuals)) 

# HISPAN
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = final_fit$residuals)) 

# MARRIED
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = final_fit$residuals)) 

# RE74
# Log?
ggplot(lalonde) +
    geom_point(mapping = aes(x = re74, y = final_fit$residuals)) 

# RE75
ggplot(lalonde) +
    geom_point(mapping = aes(x = re75, y = final_fit$residuals)) 










