### Part 1 ###

setwd('C:/Users/featg/Downloads/Rfiles')
library(tidyverse)
library(ggplot2)
library(reshape2)

datav1 <- read_csv('BaculumR.csv')
datav1
datav2 <- datav1[,c(5:13)]
datav2
datav2 <- melt(datav2, id.vars="Baculum.Length")
ggplot(datav2, aes(Baculum.Length,value)) + geom_point() + facet_wrap(~variable) +
  theme_classic2() + labs(x="Baculum Length (in millimeters)", y="Baculum Measurements (in millimeters)", 
                         title="Baculum Length against other Baculum Measurements")

### Part 2 ###

datav3 <- subset(datav1, trimws(Age.group) !="")
datav3 <- datav3[,c(3,5,6,7,8,9,10,11,12,13)]
datav3 <- melt(datav3, id.vars=c("Baculum.Length","Age.group"))
datav3$Age.group[datav3$Age.group == 1] <- "Yearling"
datav3$Age.group[datav3$Age.group == 2] <- "Subadult"
datav3$Age.group[datav3$Age.group == 3] <- "Adult"
ggplot(data = datav3) +
  geom_point(mapping=aes(x=Baculum.Length, y=value, color=Age.group)) +
  facet_wrap(~variable) + theme_classic2() +
  labs(x="Baculum Length (in millimeters)", y="Baculum Measurements (in millimeters)", 
       title="Baculum Length against other Baculum Measurements by Age Groups", color="Age Groups")

### Part 3 ###

library(modelr)
library(dplyr)
options(na.action=na.warn)

datav4 <- mutate(datav1, Seal.Lgth = Seal.Lgth*10)
datav4

lmodel <- lm(Seal.Lgth ~ Baculum.Length, data = datav4)

par(mfrow=c(2,2))
plot(lmodel)

ggplot(datav4, aes(Baculum.Length,Seal.Lgth)) +
  geom_point() + geom_smooth(method = "lm", fill = NA) + theme_classic2() +
  labs(x="Baculum Length (in millimeters)", y="Seal Length (in millimeters)", 
       title="Baculum Length against Seal Length")

cor(datav4$Seal.Lgth, datav4$Baculum.Length, use = "complete.obs")

summary(lmodel)