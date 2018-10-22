setwd('/Users/annaberman/Desktop/702 Modeling/Assignments/Team Project 1')
library(ggplot2)
library(dplyr)
library(gridExtra)

# Load the data
lalonde <- read.csv('lalondedata.txt') %>%
    mutate(treat = as.factor(treat)) %>%
    mutate(treat = factor(treat, levels = c(0, 1)))
summary(lalonde)

# Check for multicollinearity
# Nothing is over .7
cor(lalonde[,-c(1,2)]) > .6
# Don't use both edcuation and nondegree

# Treatment vs. Wages
g1 <- ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re74)) + 
    ylim(c(0,60000))
g2 <- ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re75)) + 
    ylim(c(0,60000)) 
g3 <- ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re78)) + 
    ylim(c(0,60000))
grid.arrange(g1, g2, g3,
             nrow = 2)

# Treatment 1978
ggplot(lalonde) +
    geom_point(mapping = aes(x = age, y = re78)) + 
    geom_smooth(mapping = aes(x = age, y = re78)) +
    ylim(c(0,40000))

# Education
# Might make sense to segment by degree buckets
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(educ), y = re78)) + 
    ylim(c(0,40000))
ggplot(lalonde) +
    geom_point(mapping = aes(x = educ, y = re78)) + 
    ylim(c(0,40000))


ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(married), y = re78)) + 
    ylim(c(0,40000))

# Black
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78)) + 
    ylim(c(0,40000))
# Hispanic
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(hispan), y = re78)) + 
    ylim(c(0,40000))
prop.table(table(lalonde$hispan))

# Black and hispanic
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = as.factor(black), y = re78)) + 
    facet_grid(. ~ hispan) +
    ylim(c(0,40000))
    
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re78)) + 
    facet_grid(. ~ black) +
    ylim(c(0,60000))
    
ggplot(lalonde) +
    geom_boxplot(mapping = aes(x = treat, y = re78)) + 
    facet_grid(. ~ hispan) +
    ylim(c(0,60000))

